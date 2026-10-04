#!/usr/bin/env node
// Moves every body folder from state/agents/<name> into its world, state/worlds/<world>/agents/<name>, the world being the
// one its config.json names (a folder naming none is reported and skipped, never guessed). A login cache, <folder>/auth,
// goes to state/accounts/<name> (one per account, shared by every world). Everything is a rename on one filesystem; the
// contents of auth/ are never opened. The start/mc/restart wrappers and BRIEFING.md links are rewritten for the new
// depth, and the world key leaves config.json (the folder says it now).
//   node tools/move-bodies.mjs [--state <dir>]                 dry run: every planned rename, nothing changed
//   node tools/move-bodies.mjs [--state <dir>] --apply         do it; writes state/agents-move-manifest-<time>.json
//   node tools/move-bodies.mjs --reverse <manifest>            put everything back as the manifest says it was
// Refuses (exit 2) while any body runs: a body.pid naming a live process, an engine socket that accepts a connection, or
// a process running out/body.cjs, src/bot.mjs or tools/start-body on this state root (see bodyProcesses; MOVE_BODIES_NO_PROC_SCAN=1
// leaves out that last scan; the tests use it). Exits 1 when anything is left in state/agents, else 0.
import fs from 'node:fs'
import net from 'node:net'
import path from 'node:path'
import { parseArgs } from 'node:util'
import { fileURLToPath } from 'node:url'
import { NAME, bodyDir, accountDir } from '../../engine/js/bodies.mjs'

const WRAPPERS = ['start', 'mc', 'restart']
const SOCKETS = ['control.sock', 'events.sock']
const PID_FILES = ['body.pid', path.join('engine', 'body.pid')]

// ---------------------------------------------------------------- pure rewrites
// a wrapper reaches tools/ from its own folder: three levels up from state/agents/<name>, five from the world's agents/
export const rewriteWrapper = text => text.replace(/(?<!\.\.\/)\.\.\/\.\.\/\.\.\/tools\//g, '../../../../../legacy/tools/')

// BRIEFING.md: the repo files five levels up; WORLD.md is the world folder's, two levels up
export const rewriteBriefing = text => text
  .replace(/(?<!\.\.\/)\.\.\/\.\.\/\.\.\/(harness\/|AGENT_GUIDE\.md)/g, '../../../../../legacy/$1')
  .replace(/(?<!\.\.\/)\.\.\/\.\.\/worlds\/[A-Za-z0-9_-]+\/WORLD\.md/g, '../../WORLD.md')

// config.json without its world key, in the same indentation and trailing newline
export const withoutWorld = text => {
  const { world, ...rest } = JSON.parse(text)
  const indent = /\n( +)"/.exec(text)?.[1].length ?? 0
  return JSON.stringify(rest, null, indent) + (text.endsWith('\n') ? '\n' : '')
}

// ---------------------------------------------------------------- the plan
const entriesOf = dir => fs.existsSync(dir) ? fs.readdirSync(dir, { withFileTypes: true }).sort((a, b) => a.name.localeCompare(b.name)) : []
const readConfig = file => { try { return JSON.parse(fs.readFileSync(file, 'utf8')) } catch { return null } }
const isDir = p => fs.lstatSync(p, { throwIfNoEntry: false })?.isDirectory() === true
const exists = p => fs.lstatSync(p, { throwIfNoEntry: false }) !== undefined

const rewritesOf = from => [
  ...WRAPPERS.map(file => [file, rewriteWrapper]),
  ['BRIEFING.md', rewriteBriefing],
  ['config.json', withoutWorld]
].filter(([file]) => fs.lstatSync(path.join(from, file), { throwIfNoEntry: false })?.isFile())
  .map(([file, rewrite]) => {
    const before = fs.readFileSync(path.join(from, file), 'utf8')
    return { file, before, after: rewrite(before) }
  })
  .filter(r => r.before !== r.after)

// { moves: [{ name, world, from, to, account, rewrites }], skipped: [{ name, reason }], left: [name], problems: [text] }
export function planMove (stateDir) {
  const agents = path.join(stateDir, 'agents')
  const plan = { stateDir, moves: [], skipped: [], left: [], problems: [] }
  for (const entry of entriesOf(agents)) {
    const name = entry.name
    const from = path.join(agents, name)
    if (!entry.isDirectory()) { plan.left.push(name); continue }
    if (!NAME.test(name)) { plan.skipped.push({ name, reason: 'not a body name' }); continue }
    const config = readConfig(path.join(from, 'config.json'))
    if (!config) { plan.skipped.push({ name, reason: 'no readable config.json' }); continue }
    const world = config.world
    if (typeof world !== 'string' || !NAME.test(world)) { plan.skipped.push({ name, reason: 'config.json names no world' }); continue }
    const to = bodyDir(stateDir, world, name)
    if (!isDir(path.join(stateDir, 'worlds', world))) plan.problems.push(`${name}: no world folder ${path.join(stateDir, 'worlds', world)}`)
    if (exists(to)) plan.problems.push(`${name}: ${to} exists`)
    const sock = path.join(to, 'engine', 'control.sock')
    if (Buffer.byteLength(sock) > 107) plan.problems.push(`${name}: socket path ${sock} is over 107 bytes`)
    const auth = path.join(from, 'auth')
    const account = exists(auth) ? { from: auth, to: accountDir(stateDir, name) } : null
    if (account && exists(account.to)) plan.problems.push(`${name}: ${path.relative(stateDir, account.to)} exists`)
    plan.moves.push({ name, world, from, to, account, rewrites: rewritesOf(from) })
  }
  return plan
}

// the longest engine socket path of the moved bodies (a unix socket path must stay under ~107 bytes)
export const socketReport = plan => plan.moves
  .map(m => path.join(m.to, 'engine', 'control.sock'))
  .map(p => ({ path: p, bytes: Buffer.byteLength(p) }))
  .reduce((a, b) => (b.bytes > (a?.bytes ?? -1) ? b : a), null)

// ---------------------------------------------------------------- is anything running?
// A process is { pid, argv, cwd } (cwd null when unreadable). It is a running body of stateDir when the place it works
// in lies inside stateDir: an engine body's state root (--state-dir, else <cwd>/../state as engine.main defaults), an old
// bot's home (its argument, else its cwd), a start-body supervisor's folder argument. A body process whose place cannot
// be told counts as running.
const BODY_SCRIPTS = [
  [/(^|\/)out\/body\.cjs$/, (p, i) => {
    const given = p.argv[p.argv.indexOf('--state-dir', i) + 1]
    return p.argv.indexOf('--state-dir', i) > 0 && given ? path.resolve(p.cwd, given) : path.resolve(p.cwd, '..', 'state')
  }],
  [/(^|\/)src\/bot\.mjs$/, (p, i) => path.resolve(p.cwd, p.argv[i + 1] ?? '.')],
  [/(^|\/)tools\/start-body$/, (p, i) => (p.argv[i + 1] === undefined ? null : path.resolve(p.cwd, p.argv[i + 1]))]
]
const inside = (dir, root) => {
  const rel = path.relative(root, dir)
  return rel === '' || (!rel.startsWith('..') && !path.isAbsolute(rel))
}
export const bodyProcesses = (procs, stateDir) => procs.filter(p => {
  const at = p.argv.findIndex(a => BODY_SCRIPTS.some(([re]) => re.test(a)))
  if (at < 0) return false
  if (p.cwd === null) return true
  const place = BODY_SCRIPTS.find(([re]) => re.test(p.argv[at]))[1](p, at)
  return place === null || inside(place, path.resolve(stateDir))
})

const cwdOf = n => {
  try {
    return fs.readlinkSync(`/proc/${n}/cwd`)
  } catch {
    return null
  }
}
export const readProcs = () => fs.readdirSync('/proc').filter(n => /^\d+$/.test(n) && Number(n) !== process.pid).flatMap(n => {
  try {
    return [{ pid: Number(n), argv: fs.readFileSync(`/proc/${n}/cmdline`, 'utf8').split('\0').filter(Boolean), cwd: cwdOf(n) }]
  } catch {
    return []
  }
})

const alive = pid => {
  try {
    process.kill(pid, 0)
    return true
  } catch (e) {
    return e.code === 'EPERM'
  }
}

const accepts = socketPath => new Promise(resolve => {
  const sock = net.connect(socketPath)
  const done = ok => { sock.destroy(); resolve(ok) }
  sock.setTimeout(500, () => done(false))
  sock.on('connect', () => done(true))
  sock.on('error', () => done(false))
})

// reasons, one per running body found in these folders or among the processes
export async function runningIn (dirs, stateDir, { procs }) {
  const found = []
  for (const dir of dirs) {
    const name = path.basename(dir)
    for (const file of PID_FILES) {
      const pid = Number(fs.existsSync(path.join(dir, file)) ? fs.readFileSync(path.join(dir, file), 'utf8').trim() : NaN)
      if (Number.isInteger(pid) && pid > 0 && alive(pid)) found.push(`${name}: ${file} names a live process (${pid})`)
    }
    for (const sock of SOCKETS) {
      const file = path.join(dir, 'engine', sock)
      if (exists(file) && await accepts(file)) found.push(`${name}: engine/${sock} accepts a connection`)
    }
  }
  return [...found, ...bodyProcesses(procs, stateDir).map(p => `pid ${p.pid} (cwd ${p.cwd ?? 'unknown'}): ${p.argv.join(' ')}`)]
}

export const runningBodies = (stateDir, opts) =>
  runningIn(entriesOf(path.join(stateDir, 'agents')).filter(e => e.isDirectory()).map(e => path.join(stateDir, 'agents', e.name)), stateDir, opts)

// ---------------------------------------------------------------- doing it
const writeKeepingMode = (file, text) => fs.writeFileSync(file, text)

// renames everything in the plan; the manifest is written first, so a move cut short can still be reversed
export function applyMove (plan, manifestFile) {
  if (plan.problems.length) throw new Error(`refusing: ${plan.problems.join('; ')}`)
  fs.writeFileSync(manifestFile, JSON.stringify({ at: new Date().toISOString(), stateDir: plan.stateDir, moves: plan.moves }, null, 1) + '\n')
  for (const m of plan.moves) {
    if (m.account) {
      fs.mkdirSync(path.dirname(m.account.to), { recursive: true })
      fs.renameSync(m.account.from, m.account.to)
    }
    fs.mkdirSync(path.dirname(m.to), { recursive: true })
    fs.renameSync(m.from, m.to)
    for (const r of m.rewrites) writeKeepingMode(path.join(m.to, r.file), r.after)
  }
  const agents = path.join(plan.stateDir, 'agents')
  const left = entriesOf(agents).map(e => e.name)
  if (fs.existsSync(agents) && left.length === 0) fs.rmdirSync(agents)
  return { moved: plan.moves.length, left }
}

// puts back what the manifest moved: files as they were, the folder, then its login cache into it
export function reverseMove (manifestFile) {
  const { stateDir, moves } = JSON.parse(fs.readFileSync(manifestFile, 'utf8'))
  fs.mkdirSync(path.join(stateDir, 'agents'), { recursive: true })
  let restored = 0
  const notes = []
  for (const m of [...moves].reverse()) {
    if (!exists(m.to) || exists(m.from)) { notes.push(`${m.name}: not moved back (${exists(m.from) ? `${m.from} exists` : `no ${m.to}`})`); continue }
    for (const r of m.rewrites) writeKeepingMode(path.join(m.to, r.file), r.before)
    fs.renameSync(m.to, m.from)
    if (m.account && exists(m.account.to) && !exists(m.account.from)) fs.renameSync(m.account.to, m.account.from)
    restored++
  }
  const accounts = path.join(stateDir, 'accounts')
  if (fs.existsSync(accounts) && fs.readdirSync(accounts).length === 0) fs.rmdirSync(accounts)
  return { restored, notes }
}

// ---------------------------------------------------------------- command line
const printPlan = (plan, out) => {
  for (const m of plan.moves) {
    if (m.account) out(`rename ${m.account.from} -> ${m.account.to}  (login cache, by name only)`)
    out(`rename ${m.from} -> ${m.to}`)
    for (const r of m.rewrites) out(`  rewrite ${r.file}${r.file === 'config.json' ? ': drop "world"' : ': relative links for the new depth'}`)
  }
  for (const s of plan.skipped) out(`skip ${s.name}: ${s.reason}`)
  for (const name of plan.left) out(`left ${name}: not a folder`)
  for (const p of plan.problems) out(`problem ${p}`)
  const sock = socketReport(plan)
  const verdict = !sock ? '' : sock.bytes > 107 ? '  TOO LONG: a unix socket path must stay within 107 bytes' : sock.bytes >= 100 ? `  works, ${107 - sock.bytes} bytes short of the 107-byte limit` : ''
  if (sock) out(`longest socket path: ${sock.bytes} bytes (${sock.path})${verdict}`)
}

async function main () {
  const { values } = parseArgs({ options: { state: { type: 'string' }, apply: { type: 'boolean', default: false }, reverse: { type: 'string' } } })
  const out = text => console.log(text)
  const procs = process.env.MOVE_BODIES_NO_PROC_SCAN === '1' ? [] : readProcs()
  if (values.reverse) {
    const { moves, stateDir: movedState } = JSON.parse(fs.readFileSync(values.reverse, 'utf8'))
    const running = await runningIn(moves.map(m => m.to).filter(exists), movedState, { procs })
    if (running.length) { console.error(`refusing: a body runs:\n  ${running.join('\n  ')}`); return 2 }
    const { restored, notes } = reverseMove(values.reverse)
    notes.forEach(out)
    out(`restored ${restored} of ${moves.length}`)
    return restored === moves.length ? 0 : 1
  }
  const stateDir = path.resolve(values.state ?? path.join(import.meta.dirname, '..', '..', 'state'))
  const plan = planMove(stateDir)
  const running = await runningBodies(stateDir, { procs })
  out(values.apply ? `moving bodies in ${stateDir}` : `dry run in ${stateDir} (nothing changed; --apply to do it)`)
  printPlan(plan, out)
  for (const r of running) out(`running ${r}`)
  const leftAfter = plan.skipped.length + plan.left.length
  if (!values.apply) {
    out(`would move ${plan.moves.length}, skipped ${plan.skipped.length}, left ${leftAfter}`)
    if (plan.problems.length || running.length) return 2
    return leftAfter ? 1 : 0
  }
  if (plan.problems.length || running.length) {
    console.error(`refusing: ${[...plan.problems, ...running].join('\n  ')}`)
    return 2
  }
  const manifest = path.join(stateDir, `agents-move-manifest-${new Date().toISOString().replace(/[:.]/g, '-')}.json`)
  const { moved, left } = applyMove(plan, manifest)
  out(`manifest ${manifest}`)
  out(`moved ${moved}, skipped ${plan.skipped.length}, left ${left.length}`)
  return left.length ? 1 : 0
}

if (process.argv[1] && fileURLToPath(import.meta.url) === path.resolve(process.argv[1])) process.exitCode = await main()
