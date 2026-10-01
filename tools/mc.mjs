// CLI for the bot's control API; see ./mc for usage.
import fs from 'node:fs'
import http from 'node:http'
import path from 'node:path'
import { terse, capOutput, describeClock, dawnVerdict, waitReport, parseClock, noHomeError, parseCliArgs, mapArgErrors } from '../src/cli.mjs'

// MC_HOME=<a bot's home dir> picks which body to drive (its config.json names the apiPort); default is the first bot.
const configFile = path.join(process.env.MC_HOME ?? path.join(import.meta.dirname, '..'), 'config.json')
const { apiPort = 3777 } = fs.existsSync(configFile) ? JSON.parse(fs.readFileSync(configFile, 'utf8')) : {}

const [action, ...rest] = process.argv.slice(2)
if (noHomeError(process.env.MC_HOME, action)) { console.error(`FAIL ${noHomeError(process.env.MC_HOME, action)}`); process.exit(1) }
const verbose = rest.includes('-v')
let args = parseCliArgs(rest)
// Load blueprint validation only for file input; ordinary status/recovery commands retain the isolated CLI import graph.
if (/^blueprint\.(show|check|build)$/.test(action) && args.file !== undefined) {
  try {
    const { blueprintFileArguments } = await import('../src/blueprint/source.mjs')
    args = blueprintFileArguments(action, args)
  } catch (error) { console.error(`FAIL ${error.message}`); process.exit(1) }
}
if (mapArgErrors(args)) { console.error(`FAIL ${mapArgErrors(args)}`); process.exit(1) }

const clockFile = path.join(import.meta.dirname, '..', 'state', 'clock.json')
const readClock = () => fs.existsSync(clockFile) ? parseClock(fs.readFileSync(clockFile, 'utf8')) : null
// the morning ping for a logged-off agent: run in the background, it exits (and so notifies you) when it is day
const DAWN_ENDINGS = {
  day: 'MORNING: start your body (./start) and play on',
  stale: 'NOBODY ONLINE: no body has reported the time for 90 s, and an empty world stands still. Start your body and check ./mc state',
  long: 'STILL NIGHT after 8 minutes (someone is awake): run ./mc dawn again'
}
if (action === 'dawn') {
  const started = Date.now()
  const tick = () => {
    // caught mid-write by a body on older code: look again in a second rather than call the world empty
    if (fs.existsSync(clockFile) && !readClock()) return setTimeout(tick, 1000)
    const verdict = dawnVerdict(readClock(), Date.now(), (Date.now() - started) / 1000)
    if (verdict === 'wait') return setTimeout(tick, 5000)
    console.log(`${DAWN_ENDINGS[verdict]} (${describeClock(readClock(), Date.now())})`)
    process.exit(0)
  }
  tick()
}

// wait seconds=<n, default 100: under Bash's default 2-minute timeout, so a plain foreground call always comes back>: blocks until something happens that needs the driver, prints it and exits. It reads events.jsonl, not
// the body, and keeps its place in .wait-offset, so what happens between two waits is reported by the next one
if (action === 'wait') {
  const home = process.env.MC_HOME ?? path.join(import.meta.dirname, '..')
  if (args.job !== undefined) {
    const deadline = Date.now() + (args.seconds ?? 100) * 1000
    const poll = () => new Promise((resolve, reject) => {
      const req = http.request(`http://127.0.0.1:${apiPort}/job`, { method: 'POST' }, res => {
        const chunks = []
        res.on('data', chunk => chunks.push(chunk))
        res.on('end', () => { try { resolve(JSON.parse(Buffer.concat(chunks).toString('utf8'))) } catch (error) { reject(error) } })
      })
      req.on('error', reject)
      req.end(JSON.stringify({ id: args.job }))
    })
    while (Date.now() < deadline) {
      const result = await poll()
      if (!result.ok) { console.log(`FAIL ${result.error}`); process.exit(1) }
      const job = result.job
      if (['completed', 'failed', 'cancelled', 'interrupted'].includes(job.status)) {
        console.log(verbose ? JSON.stringify(job, null, 1) : capOutput(terse({ ok: job.status === 'completed', job: job.id, ...(job.result ?? {}), ...(job.error ? { error: job.error } : {}) })))
        process.exit(job.status === 'completed' ? 0 : 1)
      }
      await new Promise(resolve => setTimeout(resolve, 1000))
    }
    const result = await poll()
    console.log(verbose ? JSON.stringify(result.job, null, 1) : `job ${args.job} still ${result.job?.status ?? 'unknown'}; inspect with ./mc job id=${args.job}`)
    process.exit(0)
  }
  const eventsFile = path.join(home, 'events.jsonl')
  const jobsFile = path.join(home, 'jobs.json')
  const offsetFile = path.join(home, '.wait-offset')
  const { username: me, chat = {} } = JSON.parse(fs.readFileSync(configFile, 'utf8'))
  // the agent roster (src/players.mjs agentNames, inlined: tools/mc.mjs may import nothing but src/cli.mjs, #148),
  // so waitReport's chattiness filter can tell a human sender (not one of these) from another agent body
  const agentsDir = path.join(home, '..')
  const agents = fs.existsSync(agentsDir)
    ? fs.readdirSync(agentsDir).filter(n => fs.existsSync(path.join(agentsDir, n, 'config.json')))
    : []
  const size = () => fs.existsSync(eventsFile) ? fs.statSync(eventsFile).size : 0
  const jobActive = () => {
    if (!fs.existsSync(jobsFile)) return false
    try {
      const shelf = JSON.parse(fs.readFileSync(jobsFile, 'utf8'))
      return shelf.active != null && shelf.jobs?.some(job => job.id === shelf.active && ['running', 'cancelling'].includes(job.status)) === true
    } catch { return false }
  }
  const saved = fs.existsSync(offsetFile) ? Number(fs.readFileSync(offsetFile, 'utf8')) : NaN
  let offset = saved <= size() ? saved : size()
  const deadline = Date.now() + (args.seconds ?? 100) * 1000
  const tick = () => {
    const fresh = Buffer.alloc(size() - offset)
    if (fresh.length) fs.readSync(fs.openSync(eventsFile, 'r'), fresh, 0, fresh.length, offset)
    const report = waitReport(fresh.toString('utf8'), me, Date.now(), chat, agents, { jobActive: jobActive() })
    offset += report.consumed
    fs.writeFileSync(offsetFile, String(offset))
    if (!report.lines.length && Date.now() < deadline) return setTimeout(tick, 1000)
    console.log(report.lines.length ? capOutput(report.lines.join('\n')) : `quiet for ${args.seconds ?? 100}s: decide what to do next, or run ./mc wait again`)
    process.exit(0)
  }
  tick()
}

// what went wrong across every agent since a time (autopilot card); tools/incidents.mjs is loaded only for this action,
// so a fault in it breaks nothing but this answer
if (action === 'incidents') {
  import('./incidents.mjs')
    .then(({ incidentsReport }) => console.log(incidentsReport(path.join(import.meta.dirname, '..', 'state', 'agents'), args.since)))
    .catch(e => { console.error(`FAIL ${e.message}`); process.exit(1) })
}

// the one action that needs no body: what time is it in the world, as last seen by any running body
if (action === 'clock') {
  console.log(describeClock(readClock(), Date.now()))
  process.exit(0)
}

// dawn, wait and incidents keep this process alive on their own and never talk to a body
// node:http, not fetch: fetch gives up ("fetch failed") when no answer has come after 300 s, and a long mine or build takes longer
const post = (url, body) => new Promise((resolve, reject) => {
  const req = http.request(url, { method: 'POST' }, res => {
    const chunks = []
    res.on('data', c => chunks.push(c))
    res.on('end', () => resolve(Buffer.concat(chunks).toString('utf8')))
  })
  req.on('error', reject)
  req.end(body)
})
if (!['dawn', 'wait', 'incidents'].includes(action)) post(`http://127.0.0.1:${apiPort}/${action}`, JSON.stringify(args))
  .then(JSON.parse)
  // the catalogue is long by nature and read once: it is the one answer not cut down to 1500 characters
  .then(j => console.log(verbose ? JSON.stringify(j, null, 1) : capOutput(terse(j), action === 'help' ? 6000 : undefined)))
  .catch(e => { console.error('bot process not reachable:', e.message); process.exit(1) })
