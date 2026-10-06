#!/usr/bin/env node
// Why JavaScript: a thin launcher around flock(1) and /proc/meminfo (machine-wide resource gate); no engine behaviour.
// tools/res-slot <kind> [--need MB] -- <cmd...>   waits for a free slot of <kind> AND MemAvailable - need >= floor, then runs cmd.
// tools/res-slot status                            holders per kind (pid, command, age) and free memory.
// Each finished run appends {kind, needMb, waitedS, ranS, code, cmd} to /tmp/mc-res/log.jsonl.
// Kinds, need, max and the memory floor: tools/res-slot.json (tools/test-shards.mjs reads the same floor and tests numbers).
// Exit 75 "busy" after ~9 min of waiting (agents' foreground calls cap at 10 min): retry. The slot is an flock held by the command's own process, so it is freed when the command exits or dies.
import fs from 'node:fs'
import path from 'node:path'
import { spawn, spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'

const CONFIG = process.env.RES_SLOT_CONFIG ?? path.join(path.dirname(fileURLToPath(import.meta.url)), 'res-slot.json')
export const loadConfig = (file = CONFIG) => JSON.parse(fs.readFileSync(file, 'utf8'))
export const availableMb = () => Number(fs.readFileSync('/proc/meminfo', 'utf8').match(/MemAvailable:\s+(\d+)/)[1]) / 1024

// Pure: run when a slot is free and memory leaves the floor intact; else wait, or busy once waited past the limit.
export const decide = ({ freeSlots, availableMb, needMb, floorMb, waitedMs, maxWaitMs }) => {
  const why = [freeSlots <= 0 && 'no free slot', availableMb - needMb < floorMb && `memory ${Math.round(availableMb)} MB free, need ${needMb} + floor ${floorMb}`].filter(Boolean).join(', ')
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
export const slotArgs = (kind, i, cmd, busyCode = 213) =>
  ['-n', '-E', String(busyCode), lockFile(kind, i), 'sh', '-c', 'printf "%s %s %s\\n" "$$" "$(date +%s)" "$1" > "$0.info"; shift; exec "$@"', lockFile(kind, i), cmd.join(' '), ...cmd]
const tryRun = (kind, i, cmd) => new Promise((res) => {
  const p = spawn('flock', slotArgs(kind, i, cmd), { stdio: 'inherit' })
  p.on('close', (code, sig) => res(code === 213 ? null : code ?? 128 + (sig ? 9 : 0)))
})

// One line per finished run in <dir>/log.jsonl, so queue waits can be measured.
export const logRun = (entry) => fs.appendFileSync(path.join(dir(), 'log.jsonl'), JSON.stringify({ t: new Date().toISOString(), ...entry, cmd: entry.cmd.slice(0, 200) }) + '\n')

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
  let lastLine = t0
  for (;;) {
    const free = [...Array(k.max).keys()].filter((i) => !held(kind, i))
    const d = decide({ freeSlots: free.length, availableMb: availableMb(), needMb, floorMb: cfg.floorMb, waitedMs: Date.now() - t0, maxWaitMs })
    if (d.action === 'run') {
      const t1 = Date.now()
      for (const i of free) {
        const code = await tryRun(kind, i, cmd)
        if (code === null) continue
        logRun({ kind, needMb, waitedS: Math.round((t1 - t0) / 1000), ranS: Math.round((Date.now() - t1) / 1000), code, cmd: cmd.join(' ') })
        process.exit(code)
      }
    } else if (d.action === 'busy') { console.error(`res-slot: busy (${d.why}), retry later`); process.exit(75) }
    else if (Date.now() - lastLine >= (Number(process.env.RES_SLOT_STATUS_MS) || 60000)) {
      lastLine = Date.now()
      console.error(`res-slot: waiting for ${kind} (${heldSlots(kind, k.max).length}/${k.max} slots in use, ${Math.round(availableMb())} MB free, ${d.why}), ${Math.round((Date.now() - t0) / 1000)}s`)
    }
    await sleep(pollMs + Math.random() * 300)
  }
}

if (process.argv[1] === fileURLToPath(import.meta.url)) await main()
