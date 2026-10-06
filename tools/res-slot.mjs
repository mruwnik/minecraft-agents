#!/usr/bin/env node
// Why JavaScript: a thin launcher around flock(1) and /proc/meminfo (machine-wide resource gate); no engine behaviour.
// tools/res-slot <kind> [--need MB] -- <cmd...>   waits for a free slot of <kind> AND MemAvailable - need >= floor, then runs cmd.
// tools/res-slot status                            holders per kind (pid, command, age) and free memory.
// A run appends {start:true, pid, kind, cmd} to /tmp/mc-res/log.jsonl when it gets its slot, and {kind, needMb, waitedS, ranS, code, cmd} when it finishes.
// Kinds, need, max and the memory floor: tools/res-slot.json (tools/test-shards.mjs reads the same floor and tests numbers).
// Exit 75 "busy" after ~9 min of waiting (agents' foreground calls cap at 10 min): retry. The slot is an flock held by the command's own process, so it is freed when the command exits or dies.
import fs from 'node:fs'
import path from 'node:path'
import { spawn, spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'

const CONFIG = process.env.RES_SLOT_CONFIG ?? path.join(path.dirname(fileURLToPath(import.meta.url)), 'res-slot.json')
export const loadConfig = (file = CONFIG) => JSON.parse(fs.readFileSync(file, 'utf8'))
export const availableMb = () => Number(fs.readFileSync(process.env.RES_SLOT_MEMINFO ?? '/proc/meminfo', 'utf8').match(/MemAvailable:\s+(\d+)/)[1]) / 1024

// Pure: run when a slot is free and memory leaves the floor intact; else wait, or busy once waited past the limit.
// reservedMb: need of recent grants whose processes have not ramped up yet (see reservedMb below); it counts as already used.
export const decide = ({ freeSlots, availableMb, needMb, floorMb, waitedMs, maxWaitMs, reservedMb = 0 }) => {
  const free = availableMb - reservedMb
  const why = [freeSlots <= 0 && 'no free slot', free - needMb < floorMb && `memory ${Math.round(availableMb)} MB free${reservedMb ? `, ${Math.round(reservedMb)} reserved for starting runs` : ''}, need ${needMb} + floor ${floorMb}`].filter(Boolean).join(', ')
  if (!why) return { action: 'run' }
  return { action: waitedMs >= maxWaitMs ? 'busy' : 'wait', why }
}

export const slotDir = () => process.env.RES_SLOT_DIR ?? '/tmp/mc-res'
const dir = slotDir
const lockFile = (kind, i) => path.join(dir(), `${kind}.${i}`)
const sleep = (ms) => new Promise((r) => setTimeout(r, ms))
const held = (kind, i) => spawnSync('flock', ['-n', '-E', '213', lockFile(kind, i), 'true']).status === 213
const heldSlots = (kind, max) => [...Array(max).keys()].filter((i) => held(kind, i))

// Runs cmd under slot i; resolves to its exit code, or null when the slot was taken meanwhile. The holder records pid/start/command in <slot>.info.
// flock(1) arguments that run cmd under slot i, exiting `busyCode` when the slot is taken (tools/test-shards.mjs uses them too).
export const slotArgs = (kind, i, cmd, busyCode = 213, marker = '') =>
  ['-n', '-E', String(busyCode), lockFile(kind, i), 'sh', '-c', 'printf "%s %s %s\\n" "$$" "$(date +%s)" "$1" > "$0.info"; [ -z "$2" ] || : > "$2"; shift 2; exec "$@"', lockFile(kind, i), cmd.join(' '), marker, ...cmd]
let runSeq = 0
const tryRun = (kind, i, cmd) => new Promise((res) => {
  // The wrapper creates the marker only once it holds the lock, so a 213 with no marker is flock's "slot taken", and one with a marker is the command's own exit code.
  const marker = `${lockFile(kind, i)}.run.${process.pid}.${runSeq++}`
  fs.rmSync(marker, { force: true }) // a stale one from a killed parent whose pid was reused
  const p = spawn('flock', slotArgs(kind, i, cmd, 213, marker), { stdio: 'inherit' })
  // start line as soon as the command holds the slot (the holder stays visible in log.jsonl even if it is SIGKILLed); p.pid is the command's pid (flock and sh exec it)
  let announced = false
  const announce = () => {
    if (announced || !fs.existsSync(marker)) return
    announced = true
    logRun({ start: true, pid: p.pid, kind, cmd: cmd.join(' ') })
  }
  const poll = setInterval(announce, 100)
  p.on('close', (code, sig) => {
    clearInterval(poll)
    announce()
    const started = fs.existsSync(marker)
    fs.rmSync(marker, { force: true })
    res(code === 213 && !started ? null : code ?? 128 + (sig ? 9 : 0))
  })
})

// One line per finished run in <dir>/log.jsonl, so queue waits can be measured.
export const logRun = (entry) => fs.appendFileSync(path.join(dir(), 'log.jsonl'), JSON.stringify({ t: new Date().toISOString(), ...entry, cmd: entry.cmd.slice(0, 200) }) + '\n')

// Grants: <dir>/grants/<pid>.json = {needMb, t}. A grant reserves its need until rampMs after it started or its owner died.
// Pure: total need of grants still ramping.
export const reservedMb = (grants, now, rampMs, alive = () => true) =>
  grants.filter((g) => now - g.t < rampMs && alive(g.pid)).reduce((a, g) => a + g.needMb, 0)
const grantDir = () => path.join(dir(), 'grants')
const pidAlive = (pid) => { try { process.kill(pid, 0); return true } catch (e) { return e.code === 'EPERM' } }
const readGrants = () => {
  if (!fs.existsSync(grantDir())) return []
  return fs.readdirSync(grantDir()).flatMap((f) => {
    try { return [{ ...JSON.parse(fs.readFileSync(path.join(grantDir(), f), 'utf8')), pid: Number(f.split('.')[0]), file: f }] } catch { return [] }
  })
}
const rampMs = () => Number(process.env.RES_SLOT_RAMP_MS ?? 60000)
const currentReserved = () => {
  const grants = readGrants()
  const live = grants.filter((g) => Date.now() - g.t < rampMs() && pidAlive(g.pid))
  for (const g of grants) if (!live.includes(g)) fs.rmSync(path.join(grantDir(), g.file), { force: true })
  return reservedMb(live, Date.now(), rampMs())
}
// Short mkdir lock so check and grant are one step across waiters; a lock older than 10 s is stale.
const withGate = async (f) => {
  const gate = path.join(dir(), 'gate.lock')
  for (;;) {
    try { fs.mkdirSync(gate); break } catch (e) {
      if (e.code !== 'EEXIST') throw e
      try { if (Date.now() - fs.statSync(gate).mtimeMs > 10000) fs.rmdirSync(gate) } catch {}
      await sleep(20 + Math.random() * 50)
    }
  }
  try { return f() } finally { fs.rmSync(gate, { recursive: true, force: true }) }
}

// "t.0 pid 12 340s sleep 30; ..." for the slots of <kind> that are held (who a waiter is waiting for).
const holders = (kind, max) => heldSlots(kind, max).map((i) => {
  const info = lockFile(kind, i) + '.info'
  const [pid, start, ...c] = (fs.existsSync(info) ? fs.readFileSync(info, 'utf8').trim() : '? ? ?').split(' ')
  return `${kind}.${i} pid ${pid} ${start === '?' ? '?' : Math.round(Date.now() / 1000 - Number(start))}s ${c.join(' ')}`.slice(0, 160)
}).join('; ')

const status = (cfg) => {
  console.log(`MemAvailable ${Math.round(availableMb())} MB, floor ${cfg.floorMb} MB`)
  for (const [kind, k] of Object.entries(cfg.kinds)) {
    const h = heldSlots(kind, k.max)
    console.log(`${kind}: ${h.length}/${k.max} in use (default need ${k.needMb} MB each)`)
    for (const i of h) {
      const [pid, start, ...c] = (fs.existsSync(lockFile(kind, i) + '.info') ? fs.readFileSync(lockFile(kind, i) + '.info', 'utf8').trim() : '? ? ?').split(' ')
      console.log(`  ${kind}.${i}  pid ${pid}  age ${start === '?' ? '?' : Math.round(Date.now() / 1000 - Number(start))}s  ${c.join(' ')}`)
    }
  }
}

const main = async () => {
  const argv = process.argv.slice(2)
  const cfg = loadConfig()
  if (argv[0] === 'status') return status(cfg)
  const dd = argv.indexOf('--')
  const kind = argv[0], cmd = dd < 0 ? [] : argv.slice(dd + 1)
  const k = cfg.kinds[kind]
  if (!k || !cmd.length) { console.error(`usage: tools/res-slot <${Object.keys(cfg.kinds).join('|')}> [--need MB] -- <cmd...> | status`); process.exit(2) }
  const ni = argv.indexOf('--need')
  const needMb = ni > 0 && ni < dd ? Number(argv[ni + 1]) : k.needMb
  const maxWaitMs = Number(process.env.RES_SLOT_MAX_WAIT_MS ?? 540000), pollMs = Number(process.env.RES_SLOT_POLL_MS ?? 1000)
  fs.mkdirSync(dir(), { recursive: true })
  const t0 = Date.now()
  let lastLine = t0, lastReason = ''
  for (;;) {
    const free = [...Array(k.max).keys()].filter((i) => !held(kind, i))
    const gated = await withGate(() => {
      const reserved = currentReserved()
      const d = decide({ freeSlots: free.length, availableMb: availableMb(), needMb, floorMb: cfg.floorMb, waitedMs: Date.now() - t0, maxWaitMs, reservedMb: reserved })
      if (d.action === 'run') fs.mkdirSync(grantDir(), { recursive: true }), fs.writeFileSync(path.join(grantDir(), `${process.pid}.json`), JSON.stringify({ needMb, t: Date.now() }))
      return d
    })
    const d = gated
    if (d.action === 'run') {
      const t1 = Date.now()
      const dropGrant = () => fs.rmSync(path.join(grantDir(), `${process.pid}.json`), { force: true })
      for (const i of free) {
        const code = await tryRun(kind, i, cmd)
        if (code === null) continue
        logRun({ kind, needMb, waitedS: Math.round((t1 - t0) / 1000), ranS: Math.round((Date.now() - t1) / 1000), code, cmd: cmd.join(' ') })
        process.exit(code)
      }
      dropGrant()
    } else if (d.action === 'busy') { console.error(`res-slot: busy (${d.why}${free.length ? '' : `; held by ${holders(kind, k.max)}`}), retry later (gave up waiting after ${Math.round((Date.now() - t0) / 1000)}s)`); process.exit(75) }
    else {
      // live-tests phase line (TEST_EVENTS=1), once per change of reason (digits ignored: free MB drifts)
      const reason = d.why.replace(/\d+/g, '#')
      if (process.env.TEST_EVENTS && reason !== lastReason) console.log(`@@test ${JSON.stringify({ event: 'phase', name: `waiting: ${d.why}${free.length ? '' : `; held by ${holders(kind, k.max)}`}`.slice(0, 300) })}`)
      lastReason = reason
    }
    if (d.action === 'wait' && Date.now() - lastLine >= (Number(process.env.RES_SLOT_STATUS_MS) || 60000)) {
      lastLine = Date.now()
      console.error(`res-slot: waiting for ${kind} (${heldSlots(kind, k.max).length}/${k.max} slots in use, ${Math.round(availableMb())} MB free, ${d.why}${free.length ? '' : `; held by ${holders(kind, k.max)}`}), ${Math.round((Date.now() - t0) / 1000)}s`)
    }
    await sleep(pollMs + Math.random() * 300)
  }
}

if (process.argv[1] === fileURLToPath(import.meta.url)) await main()
