#!/usr/bin/env node
// Why JavaScript: a thin launcher around flock(1) and /proc/meminfo (machine-wide resource gate); no engine behaviour.
// tools/res-slot <kind> [--need MB] -- <cmd...>   waits for a free slot of <kind> AND MemAvailable - need >= floor, then runs cmd.
// Heavy runs (--heavy, RES_SLOT_HEAVY=1, or a .res-slot-heavy file in the cwd or a parent: tools/integration-worktree.sh writes it, since agents' test runs cannot set env) may also take the `reserve` slots a kind keeps back in res-slot.json.
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

// Pure: the free slots a run may take. The last `reserve` slots of a kind are for heavy runs (integration, world sweep) so many light holders cannot starve them.
export const hasHeavyMarker = (cwd) => {
  for (let d = path.resolve(cwd); ; d = path.dirname(d)) {
    if (fs.existsSync(path.join(d, '.res-slot-heavy'))) return true
    if (d === path.dirname(d)) return false
  }
}
export const usableSlots = (free, max, reserve, heavy) => heavy ? free : free.filter((i) => i < max - reserve)

// A world sweep (tools/world-test-pool.mjs) writes its pid to <dir>/sweep-active; while that pid lives, a light run of a kind with `sweepCap` starts only while fewer than that many slots are held by light runs (heavy holders have the reserve).
export const capDuringSweep = (free, heldCount, cap, sweeping) => !sweeping || cap === undefined || heldCount < cap ? free : []
const sweepActive = () => {
  try { return pidAlive(Number(fs.readFileSync(path.join(dir(), 'sweep-active'), 'utf8'))) } catch { return false }
}

// Slot indices a run tries, in order: `want` own slots (0..want-1); a heavy run tries the reserved ones (max-reserve..max-1) first, still `want` in all.
export const candidateSlots = (want, max, reserve, heavy) => [...(heavy ? [...Array(reserve).keys()].map((j) => max - reserve + j) : []), ...Array(want).keys()].slice(0, want)

export const slotDir = () => process.env.RES_SLOT_DIR ?? '/tmp/mc-res'
const dir = slotDir
const lockFile = (kind, i) => path.join(dir(), `${kind}.${i}`)
const sleep = (ms) => new Promise((r) => setTimeout(r, ms))
const held = (kind, i) => spawnSync('flock', ['-n', '-E', '213', lockFile(kind, i), 'true']).status === 213
const heldSlots = (kind, max) => [...Array(max).keys()].filter((i) => held(kind, i))
const lightHeldCount = (kind, max) => heldSlots(kind, max).filter((i) => !fs.existsSync(lockFile(kind, i) + '.heavy')).length

// Runs cmd under slot i; resolves to its exit code, or null when the slot was taken meanwhile. The holder records pid/start/command in <slot>.info.
// flock(1) arguments that run cmd under slot i, exiting `busyCode` when the slot is taken (tools/test-shards.mjs uses them too).
// A heavy holder leaves <slot>.heavy, a light one removes it, so the file exists only while a heavy run holds the slot.
export const slotArgs = (kind, i, cmd, busyCode = 213, marker = '', heavy = false) =>
  ['-n', '-E', String(busyCode), lockFile(kind, i), 'sh', '-c', 'printf "%s %s %s\\n" "$$" "$(date +%s)" "$1" > "$0.info"; if [ -n "$3" ]; then : > "$0.heavy"; else rm -f "$0.heavy"; fi; [ -z "$2" ] || : > "$2"; shift 3; exec "$@"', lockFile(kind, i), cmd.join(' '), marker, heavy ? '1' : '', ...cmd]
let runSeq = 0
const tryRun = (kind, i, cmd, onStart = () => {}, heavy = false) => new Promise((res) => {
  // The wrapper creates the marker only once it holds the lock, so a 213 with no marker is flock's "slot taken", and one with a marker is the command's own exit code.
  const marker = `${lockFile(kind, i)}.run.${process.pid}.${runSeq++}`
  fs.rmSync(marker, { force: true }) // a stale one from a killed parent whose pid was reused
  const p = spawn('flock', slotArgs(kind, i, cmd, 213, marker, heavy), { stdio: 'inherit', env: { ...process.env, RES_SLOT_HELD: [process.env.RES_SLOT_HELD, kind].filter(Boolean).join(',') } })
  // start line as soon as the command holds the slot (the holder stays visible in log.jsonl even if it is SIGKILLed); p.pid is the command's pid (flock and sh exec it)
  let announced = false
  const announce = () => {
    if (announced || !fs.existsSync(marker)) return
    announced = true
    onStart()
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
export const logRun = (entry) => fs.appendFileSync(path.join(dir(), 'log.jsonl'), JSON.stringify({ t: new Date().toISOString(), cwd: process.cwd(), run: process.env.MC_RUN_ID, ...entry, cmd: entry.cmd.slice(0, 200) }) + '\n')

// Grants: <dir>/grants/<pid>.json = {needMb, t}. A grant reserves its need until rampMs after it started or its owner died.
// Pure: need of grants still ramping, minus what their process trees already hold (MemAvailable has dropped by that much, so it must not count twice).
export const reservedMb = (grants, now, rampMs, alive = () => true, rss = () => 0) =>
  grants.filter((g) => now - g.t < rampMs && alive(g.pid)).reduce((a, g) => a + Math.max(0, g.needMb - rss(g.pid)), 0)
// Heavy waiters: <dir>/heavy-wait/<kind>.<pid>[.<n>], holding the JSON list of slots the waiter tries (empty: any slot). While one is alive, light runs of the kind leave those slots alone, so a freed slot reaches the heavy run.
export const isHeavy = (cwd = process.cwd()) => !!process.env.RES_SLOT_HEAVY || hasHeavyMarker(cwd)
// Pure: the free slots a light run may take. waits: one entry per live heavy waiter, the slots it tries, or null (any slot).
export const freeForLight = (free, waits) => waits.includes(null) ? [] : free.filter((i) => !waits.some((w) => w.includes(i)))
export const heavyWaitFile = (kind, pid = process.pid, n = '') => path.join(dir(), 'heavy-wait', `${kind}.${pid}${n && `.${n}`}`)
const readWait = (f) => { try { return JSON.parse(fs.readFileSync(f, 'utf8')) } catch { return null } }
export const heavyWaits = (kind, self = process.pid) => {
  const d = path.join(dir(), 'heavy-wait')
  if (!fs.existsSync(d)) return []
  return fs.readdirSync(d).filter((f) => f.startsWith(`${kind}.`) && Number(f.split('.')[1]) !== self && pidAlive(Number(f.split('.')[1]))).map((f) => readWait(path.join(d, f)))
}
const grantDir = () => path.join(dir(), 'grants')
const pidAlive = (pid) => { try { process.kill(pid, 0); return true } catch (e) { return e.code === 'EPERM' } }
const readGrants = () => {
  if (!fs.existsSync(grantDir())) return []
  return fs.readdirSync(grantDir()).flatMap((f) => {
    try { return [{ ...JSON.parse(fs.readFileSync(path.join(grantDir(), f), 'utf8')), pid: Number(f.split('.')[0]), file: f }] } catch { return [] }
  })
}
// {kids, rss} of every process, read from /proc once per gate check (rss counts shared pages per process, so a tree is slightly overcounted).
const procTable = () => {
  const kids = new Map()
  const rss = new Map()
  for (const p of fs.readdirSync('/proc').filter((f) => /^\d+$/.test(f))) {
    try {
      const st = fs.readFileSync(`/proc/${p}/stat`, 'utf8')
      const ppid = Number(st.slice(st.lastIndexOf(')') + 2).split(' ')[1])
      kids.set(ppid, [...(kids.get(ppid) ?? []), Number(p)])
      rss.set(Number(p), Number(fs.readFileSync(`/proc/${p}/statm`, 'utf8').split(' ')[1]) * 4096 / 1048576)
    } catch { /* process gone */ }
  }
  return { kids, rss }
}
// Pure: resident MB of root and all its descendants in a procTable.
export const treeRssMb = ({ kids, rss }, root) => {
  const sum = (pid) => (rss.get(pid) ?? 0) + (kids.get(pid) ?? []).reduce((a, k) => a + sum(k), 0)
  return sum(root)
}
const rampMs = () => Number(process.env.RES_SLOT_RAMP_MS ?? 60000)
const currentReserved = () => {
  const grants = readGrants()
  const live = grants.filter((g) => Date.now() - g.t < rampMs() && pidAlive(g.pid))
  for (const g of grants) if (!live.includes(g)) fs.rmSync(path.join(grantDir(), g.file), { force: true })
  if (!live.length) return 0
  const table = procTable()
  return reservedMb(live, Date.now(), rampMs(), pidAlive, (pid) => treeRssMb(table, pid))
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
  const hi = argv.indexOf('--heavy')
  const heavy = (hi > 0 && hi < dd) || !!process.env.RES_SLOT_HEAVY || hasHeavyMarker(process.cwd())
  const ni = argv.indexOf('--need')
  const needMb = ni > 0 && ni < dd ? Number(argv[ni + 1]) : k.needMb
  const maxWaitMs = Number(process.env.RES_SLOT_MAX_WAIT_MS ?? 540000), pollMs = Number(process.env.RES_SLOT_POLL_MS ?? 1000)
  fs.mkdirSync(dir(), { recursive: true })
  const t0 = Date.now()
  let lastLine = t0, lastReason = ''
  for (;;) {
    if (heavy) fs.mkdirSync(path.dirname(heavyWaitFile(kind)), { recursive: true }), fs.writeFileSync(heavyWaitFile(kind), '')
    const open = usableSlots([...Array(k.max).keys()].filter((i) => !held(kind, i)), k.max, k.reserve ?? 0, heavy)
    let free = open
    const sweeping = !heavy && sweepActive()
    const blockers = () => free.length ? '' : sweeping && k.sweepCap !== undefined && open.length ? `; a world sweep is running: other agents' ${kind} runs are capped to ${k.sweepCap}` : open.length ? '; free slots kept for a waiting heavy run' : `; held by ${holders(kind, k.max)}`
    const gated = await withGate(() => {
      const reserved = currentReserved()
      free = heavy ? open : capDuringSweep(freeForLight(open, heavyWaits(kind)), lightHeldCount(kind, k.max), k.sweepCap, sweeping)
      const d = decide({ freeSlots: free.length, availableMb: availableMb(), needMb, floorMb: cfg.floorMb, waitedMs: Date.now() - t0, maxWaitMs, reservedMb: reserved })
      if (d.action === 'run') fs.mkdirSync(grantDir(), { recursive: true }), fs.writeFileSync(path.join(grantDir(), `${process.pid}.json`), JSON.stringify({ needMb, t: Date.now() }))
      return d
    })
    const d = gated
    if (d.action === 'run') {
      const t1 = Date.now()
      const dropGrant = () => fs.rmSync(path.join(grantDir(), `${process.pid}.json`), { force: true })
      for (const i of free) {
        const code = await tryRun(kind, i, cmd, () => fs.rmSync(heavyWaitFile(kind), { force: true }), heavy) // the marker stays until the command holds the slot, else a light waiter takes it in the gap
        if (code === null) continue
        logRun({ kind, needMb, waitedS: Math.round((t1 - t0) / 1000), ranS: Math.round((Date.now() - t1) / 1000), code, cmd: cmd.join(' ') })
        process.exit(code)
      }
      dropGrant()
    } else if (d.action === 'busy') { fs.rmSync(heavyWaitFile(kind), { force: true }); console.error(`res-slot: busy (${d.why}${blockers()}), retry later (gave up waiting after ${Math.round((Date.now() - t0) / 1000)}s)`); process.exit(75) }
    else {
      // live-tests phase line (TEST_EVENTS=1), once per change of reason (digits ignored: free MB drifts)
      const reason = d.why.replace(/\d+/g, '#')
      if (process.env.TEST_EVENTS && reason !== lastReason) console.log(`@@test ${JSON.stringify({ event: 'phase', t: Date.now(), name: `waiting: ${d.why}${blockers()}`.slice(0, 300) })}`)
      lastReason = reason
    }
    if (d.action === 'wait' && Date.now() - lastLine >= (Number(process.env.RES_SLOT_STATUS_MS) || 60000)) {
      lastLine = Date.now()
      console.error(`res-slot: waiting for ${kind} (${heldSlots(kind, k.max).length}/${k.max} slots in use, ${Math.round(availableMb())} MB free, ${d.why}${blockers()}), ${Math.round((Date.now() - t0) / 1000)}s`)
    }
    await sleep(pollMs + Math.random() * 300)
  }
}

if (process.argv[1] === fileURLToPath(import.meta.url)) await main()
