// CLI for the bot's control API; see ./mc for usage.
import fs from 'node:fs'
import http from 'node:http'
import path from 'node:path'
import { terse, capOutput, describeClock, dawnVerdict, waitReport, parseClock, noHomeError, parseCliArgs } from '../src/lib.mjs'

// MC_HOME=<a bot's home dir> picks which body to drive (its config.json names the apiPort); default is the first bot.
const configFile = path.join(process.env.MC_HOME ?? path.join(import.meta.dirname, '..'), 'config.json')
const { apiPort = 3777 } = fs.existsSync(configFile) ? JSON.parse(fs.readFileSync(configFile, 'utf8')) : {}

const [action, ...rest] = process.argv.slice(2)
if (noHomeError(process.env.MC_HOME, action)) { console.error(`FAIL ${noHomeError(process.env.MC_HOME, action)}`); process.exit(1) }
const verbose = rest.includes('-v')
const args = parseCliArgs(rest)

const clockFile = path.join(import.meta.dirname, '..', 'state', 'clock.json')
const readClock = () => fs.existsSync(clockFile) ? parseClock(fs.readFileSync(clockFile, 'utf8')) : null
// the morning ping for a logged-off agent: run in the background, it exits (and so notifies you) when it is day
const DAWN_ENDINGS = {
  day: 'MORNING: start your body (./start in the background) and play on',
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
  const eventsFile = path.join(home, 'events.jsonl')
  const offsetFile = path.join(home, '.wait-offset')
  const me = JSON.parse(fs.readFileSync(configFile, 'utf8')).username
  const size = () => fs.existsSync(eventsFile) ? fs.statSync(eventsFile).size : 0
  const saved = fs.existsSync(offsetFile) ? Number(fs.readFileSync(offsetFile, 'utf8')) : NaN
  let offset = saved <= size() ? saved : size()
  const deadline = Date.now() + (args.seconds ?? 100) * 1000
  const tick = () => {
    const fresh = Buffer.alloc(size() - offset)
    if (fresh.length) fs.readSync(fs.openSync(eventsFile, 'r'), fresh, 0, fresh.length, offset)
    const report = waitReport(fresh.toString('utf8'), me)
    offset += report.consumed
    fs.writeFileSync(offsetFile, String(offset))
    if (!report.lines.length && Date.now() < deadline) return setTimeout(tick, 1000)
    console.log(report.lines.length ? capOutput(report.lines.join('\n')) : `quiet for ${args.seconds ?? 100}s: decide what to do next, or run ./mc wait again`)
    process.exit(0)
  }
  tick()
}

// the one action that needs no body: what time is it in the world, as last seen by any running body
if (action === 'clock') {
  console.log(describeClock(readClock(), Date.now()))
  process.exit(0)
}

// dawn and wait keep this process alive on their own timers and never talk to a body
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
if (action !== 'dawn' && action !== 'wait') post(`http://127.0.0.1:${apiPort}/${action}`, JSON.stringify(args))
  .then(JSON.parse)
  // the catalogue is long by nature and read once: it is the one answer not cut down to 1500 characters
  .then(j => console.log(verbose ? JSON.stringify(j, null, 1) : capOutput(terse(j), action === 'help' ? 6000 : undefined)))
  .catch(e => { console.error('bot process not reachable:', e.message); process.exit(1) })
