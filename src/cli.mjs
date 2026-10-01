// What ./mc needs, and nothing it does not (#148). tools/mc.mjs imports only this file, and this file imports nothing, so a
// half-saved lib.mjs or bot.mjs breaks bodies (which the start gate then refuses to start) but never the command that reports it.
// lib.mjs re-exports all of it: one copy of each helper for bodies, tests and mc alike

// This file's own location on disk, with no import needed to find it: file:///.../bot/src/cli.mjs -> /.../bot
const BOT_ROOT = import.meta.url.replace(/^file:\/\//, '').replace(/\/src\/cli\.mjs$/, '')

const LONG_FIELDS = ['task', 'action', 'seconds', 'gained', 'lost', 'ate', 'pos', 'ok', 'error']
// a short result (no action) renders only these itself: the rest of LONG_FIELDS are ordinary fields there. `eat` answers
// ate= and gained= (numbers, not the long result's count maps), and the long list swallowed both: "ok food=10 health=10"
const SHORT_FIELDS = ['pos', 'ok', 'error']
const isPos = v => Object.keys(v).length === 3 && ['x', 'y', 'z'].every(k => typeof v[k] === 'number')
const isEmpty = v => v == null || v === false || v === '' || (typeof v === 'object' && Object.keys(v).length === 0)
const bracket = v => typeof v === 'object' && !Array.isArray(v) && !isPos(v) ? `(${compact(v)})` : compact(v)

// JSON without the punctuation tax: positions are x,y,z, counts are name:count, true is a bare key and
// null/false/empty simply vanish. Meant to be read by an LLM that pays per token, not parsed.
export function compact (v, counts = true) {
  if (isEmpty(v)) return ''
  if (typeof v === 'number') return String(Math.round(v * 10) / 10)
  if (typeof v !== 'object') return String(v)
  if (Array.isArray(v)) return v.filter(i => !isEmpty(i)).map(bracket).join(' ')
  if (isPos(v)) return ['x', 'y', 'z'].map(k => Math.floor(v[k])).join(',')
  const entries = Object.entries(v).filter(([, val]) => !isEmpty(val))
  if (counts && entries.every(([, val]) => typeof val === 'number')) return entries.map(([k, n]) => n === 1 ? k : `${k}:${n}`).join(' ')
  return entries.map(([k, val]) => val === true ? k : typeof val === 'object' && !Array.isArray(val) && !isPos(val) ? `${k}(${compact(val)})` : `${k}=${compact(val)}`).join(' ')
}

const signed = (sign, obj = {}) => Object.entries(obj).map(([k, n]) => `${sign}${k}:${n}`)

// One short line per API result: every token of output costs the driver context.
export function terse (r) {
  if (typeof r.map === 'string') return r.map
  if (typeof r.text === 'string') return r.text
  const head = r.ok ? 'ok' : 'FAIL'
  const error = r.error ? [`error: ${r.error}`] : []
  if (r.status === 'running' && r.task !== undefined) return `ok running task=${r.task} (still going: block on ./mc wait for its task_done, do not end your turn)`
  if (r.status === 'running' || r.status === 'queued') return `ok ${r.status} job=${r.job ?? r.task} (inspect with ./mc job id=${r.job ?? r.task}; block with ./mc wait job=${r.job ?? r.task})`
  const hidden = r.action ? LONG_FIELDS : SHORT_FIELDS
  const extras = compact(Object.fromEntries(Object.entries(r).filter(([k]) => !hidden.includes(k))), false)
  if (!r.action) return [head, ...(extras ? [extras] : []), ...(r.pos ? [`pos=${compact(r.pos)}`] : []), ...error].join(' ')
  const at = r.pos ? [`@${compact(r.pos)}`] : []
  const ate = r.ate ? [`ate=${signed('', r.ate).join(',')}`] : []
  return [head, r.action, `${r.seconds}s`, ...signed('+', r.gained), ...signed('-', r.lost), ...ate, ...at, ...(extras ? [extras] : []), ...error].join(' ')
}

export const capOutput = (text, limit = 1500) => text.length <= limit
  ? text
  : `${text.slice(0, limit)}\n[+${text.length - limit} chars cut: narrow the query, or delegate reading the full output (-v) to a subagent]`

export const between = (v, a, b) => v >= Math.min(a, b) && v <= Math.max(a, b)

// `./mc dawn` blocks until morning so that its exit wakes a logged-off agent; it must never wait for ever
export function dawnVerdict (clock, now, waitedSeconds) {
  if (!clock || now - clock.at > 90000) return 'stale'
  if (clock.day) return 'day'
  return waitedSeconds >= 8 * 60 ? 'long' : 'wait'
}

// every running body writes the game time to clock.json; a logged-off agent reads it (./mc clock) instead of reconnecting to look,
// which would stop the night from skipping
export function describeClock (clock, now) {
  const age = clock ? Math.round((now - clock.at) / 1000) : null
  if (!clock || age > 60) return `time unknown: no body has reported ${clock ? `for ${age}s` : 'yet'} (nobody online); start your body and check state`
  return `time=${clock.day ? 'day' : 'night'} ${clock.timeOfDay} (seen ${age}s ago by ${clock.by})`
}

// ./mc wait: an idle subagent is not woken by its monitor (the events only reach it with the next message), so drivers wait inside a
// blocking command instead. These are the events worth ending the wait for
const WAKE_TYPES = new Set(['tool_broke', 'whisper', 'chat_refused', 'died', 'kicked', 'body_down', 'error', 'job_failed', 'job_cancelled', 'job_interrupted', 'wedged', 'stalled', 'buried',
  'watch_hit', 'night_fell', 'dawn', 'woke_up', 'bedtime_failed', 'code_updated',
  // a run the body gave up on is the agent's problem now, and an agent asleep in ./mc wait cannot take it (#138)
  'flee_stuck', 'flee_held',
  // a routine on autopilot that ended, and a body its own watch found going nowhere (autopilot card, src/navigation/stuck.mjs)
  'routine_stopped', 'stuck', 'farm_attention', 'forestry_attention'])
export const wakeWorthy = (event, me, { jobActive = false } = {}) => (jobActive && ['night_fell', 'dawn', 'woke_up'].includes(event.type) ? false : WAKE_TYPES.has(event.type)) ||
  (['task_done', 'task_cancelled'].includes(event.type) && event.notify !== false) ||
  (event.type === 'job_completed' && event.notify !== false) ||
  (['job_progress', 'job_waiting'].includes(event.type) && event.verbose === true) ||
  (event.type === 'chat' && event.from !== me) || (event.type === 'hurt' && event.health <= 8)

// Chattiness (card 2e032c4a): how much a chat/whisper line asks ME for an answer, in [0,1], and whether it is loud
// enough for chattiness to end a wait over. Canonically this belongs with the rest of src/chatter.mjs, but tools/mc.mjs
// and this file are both restricted (#148, see the top of this file) to importing none of our other files, so it
// lives here and chatter.mjs re-exports it instead of holding a second copy.
const GREETINGS = new Set(['morning', 'good morning', 'hi', 'hello', 'hey', 'thanks', 'thank you', 'ok', 'okay', 'gg', 'nice', 'bye', 'night', 'goodnight'])
const DANGER_WORDS = ['help', 'dying', 'creeper', 'fire', 'lost', 'stuck']
const QUESTION_STARTS = /^(who|what|where|when|why|how|can|could|anyone)\b/i
const escapeRegExp = s => s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')
const norm = message => String(message ?? '').trim()

export const isGreeting = message => GREETINGS.has(norm(message).toLowerCase().replace(/[!.?]+$/, ''))
export const isQuestion = message => { const text = norm(message); return /\?\s*$/.test(text) || QUESTION_STARTS.test(text) }
export const isDanger = message => DANGER_WORDS.some(word => new RegExp(`\\b${word}\\b`, 'i').test(norm(message)))
export const namesMe = (message, me) => !!me && new RegExp(`\\b${escapeRegExp(me)}\\b`, 'i').test(norm(message))

// A whisper (to === me) always tops out at 1. Otherwise the weight starts at a greeting's quiet 0.1 or the default
// 0.3, and is only ever raised (never lowered) by danger, a question, my own name, or a human sender - independent
// signals combined with max, not a priority order, so "Chani: good morning" and "Steve: good morning" differ (only
// the second is a human) but neither is silenced by being a greeting once something else has raised it.
export function messageWeight ({ from, message, to, me, humans = [] } = {}) {
  if (to && me && to === me) return 1
  let weight = isGreeting(message) ? 0.1 : 0.3
  if (isDanger(message)) weight = Math.max(weight, 0.7)
  if (isQuestion(message)) weight = Math.max(weight, 0.6)
  if (namesMe(message, me)) weight = Math.max(weight, 0.9)
  if (humans.includes(from)) weight = Math.max(weight, 0.8)
  return weight
}

// The allow/deny/threshold decision: deny wins outright (even a whisper - that is what deny is for), a non-empty
// allow list is the only door in, and otherwise a line is heard when chattiness plus its weight reaches 1 (chattiness
// 1 hears everything since the lowest weight is 0.1; chattiness 0 hears only a whisper, since nothing else weighs 1).
export function hears ({ event, chat = {}, me, humans = [] }) {
  const { chattiness = 1, allow = [], deny = [] } = chat
  if (deny.includes(event.from)) return false
  if (allow.length && !allow.includes(event.from)) return false
  const to = event.type === 'whisper' ? me : event.to
  return chattiness + messageWeight({ from: event.from, message: event.message, to, me, humans }) >= 1
}

// Nothing is hidden, only quiet: the skipped lines are still in events.jsonl, and this says how many and how to read them.
export const skippedLine = (count, chattiness) => `skipped ${count} chat line${count === 1 ? '' : 's'} below your chattiness (${chattiness}): ./mc events type=chat n=${count}`

const eventLine = ({ seq, t, type, ...rest }) => [type, ...Object.entries(rest).map(([k, v]) => v === true ? k : `${k}=${typeof v === 'string' ? v : JSON.stringify(v)}`)].join(' ')

// text: what was appended to events.jsonl since the last look. consumed: how many bytes of it were whole lines (the rest is read next time)
// what happened between two waits is reported by the second one: anything older than a minute says so ("(3m ago) died"), or a driver
// reads an old death as a new one
const aged = (event, now) => {
  const minutes = Math.floor((now - Date.parse(event.t)) / 60000)
  return minutes >= 1 ? `(${minutes}m ago) ` : ''
}
// chat= is the agent's config.chat ({chattiness, allow, deny}, default = today's behaviour: chattiness 1, no lists);
// agents= is the roster of agent body names (src/players.mjs agentNames) so a sender not in it counts as a human for
// messageWeight's human bonus. A chat/whisper event that wakeWorthy would report but hears() would not is left out
// and counted instead: nothing is hidden, `skippedLine` says how many and how to read them.
export function waitReport (text, me, now = Date.now(), chat = {}, agents = [], { jobActive = false } = {}) {
  const whole = text.slice(0, text.lastIndexOf('\n') + 1)
  const events = whole.split('\n').filter(Boolean).map(line => JSON.parse(line))
  const candidates = events.filter(e => wakeWorthy(e, me, { jobActive }))
  // an empty (unprovided) roster means "unknown", not "everyone is human": only flag a sender human when there IS a
  // roster and it leaves them out, so a caller that skips agents= gets today's behaviour, not a surprise 0.8 floor
  const chatty = e => (e.type !== 'chat' && e.type !== 'whisper') ||
    hears({ event: e, chat, me, humans: agents.length && !agents.includes(e.from) ? [e.from] : [] })
  const heard = candidates.filter(chatty)
  const skipped = candidates.length - heard.length
  const lines = heard.map(e => aged(e, now) + eventLine(e))
  return { lines: skipped ? [...lines, skippedLine(skipped, chat.chattiness ?? 1)] : lines, consumed: Buffer.byteLength(whole) }
}

// bedtime reflex: a body whose driver is away (monitor expired, waiting, asleep itself) still goes to bed, so one absent driver does not
// keep the night going for everyone. A driver who is at work (a task, or a command in the last 90 s) is left alone
export const bedtime = s => s.night && !s.busy && !s.asleep && s.bedNear && !s.hostileNear && s.reflexes && s.idleMs >= 90000 &&
  s.sinceTryMs >= Math.min(30000 * 2 ** s.failures, 300000)

// mc without MC_HOME knows no body. It used to fall back to the first one (Claude's): whoever ran bot/mc from a drifted shell drove somebody else's body

export const noHomeError = (home, action) => home || ['clock', 'dawn', 'incidents'].includes(action) ? null : `no agent chosen: this is the shared bot/ folder, and its mc drives nobody. Run YOUR OWN wrapper with its full path: ${BOT_ROOT}/state/agents/<YourName>/mc <action> ... (your shell has probably drifted out of your folder: cd back into it)`

// clock.json as read from disk; null when it was caught mid-write (the next look, a few seconds on, finds it whole)
export function parseClock (text) {
  try { return JSON.parse(text) } catch { return null }
}

// Arguments that name items and counts (withdraw, deposit, farm.compost) want a JSON map. A string that reaches the
// body instead is walked character by character by whatever reads it: `withdraw items=bread:7,oak_planks:2` answered
// "6:0/7 19:0/2". So the CLI reads that shorthand (name:count pairs, a count left out or `all` meaning all of it) as
// the map it stands for, and anything else that is not a map or a list is refused before it is sent - by the CLI, and
// by any body-side action that calls mapArgErrors on what it was given.
const MAP_ARGS = ['items']
const SHORTHAND = /^[a-z0-9_]+(:(\d+|all))?(,[a-z0-9_]+(:(\d+|all))?)*$/i
export const MAP_ARG_HELP = key => `${key}= wants JSON, e.g. ${key}='{"bread":7,"oak_planks":2}' (or the shorthand ${key}=bread:7,oak_planks:2)`

const shorthandMap = raw => Object.fromEntries(raw.split(',').map(pair => {
  const [name, count = 'all'] = pair.split(':')
  return [name, count === 'all' ? 'all' : Number(count)]
}))
const mapValue = raw => typeof raw === 'string' && SHORTHAND.test(raw) ? shorthandMap(raw) : raw

// the map arguments that arrived as something else: null when every one of them is a map or a list
export const mapArgErrors = args => MAP_ARGS
  .filter(key => args[key] !== undefined && (args[key] === null || typeof args[key] !== 'object'))
  .map(MAP_ARG_HELP)[0] ?? null

// ./mc <action> key=value ...: a bare word (./mc help farm.maintain) is the topic, -v asks for the raw answer
export const parseCliArgs = argv => Object.fromEntries(argv.filter(kv => kv !== '-v').map(kv => {
  const at = kv.indexOf('=')
  const parse = v => { try { return JSON.parse(v) } catch { return v } }
  if (at < 0) return ['topic', parse(kv)]
  const key = kv.slice(0, at)
  const value = parse(kv.slice(at + 1))
  return [key, MAP_ARGS.includes(key) ? mapValue(value) : value]
}))

// node --check's complaint as one line a driver can act on ('src/lib.mjs:3: SyntaxError: ...'): the start gate writes it into body_down
export function checkFailure (file, stderr) {
  const lines = String(stderr).split('\n')
  const line = lines[0].match(/:(\d+)$/)?.[1]
  const error = lines.find(l => /^\w*Error\b/.test(l))
  return error ? `${file}${line ? `:${line}` : ''}: ${error}` : `${file}: node --check failed`
}

// ./mc incidents [since=<minutes|ISO>] (autopilot card): what went wrong across every agent since a time, one line each,
// newest last, for a lead session that resumes and asks "what happened while I was away?". The folders are read by
// tools/incidents.mjs; this is the shaping, pure, in the one file tools/mc.mjs may import.
export const INCIDENT_TYPES = ['died', 'body_down', 'kicked', 'routine_stopped', 'stuck']

export function sinceTime (since, now = Date.now()) {
  if (since === undefined) return now - 60 * 60000
  if (typeof since === 'number') return now - since * 60000
  const at = Date.parse(since)
  if (Number.isNaN(at)) throw new Error(`since=${since} is neither minutes nor an ISO time (since=90, since=2026-09-26T12:00:00Z)`)
  return at
}

const incidentPos = pos => pos && typeof pos === 'object' ? ['x', 'y', 'z'].map(k => Math.floor(pos[k])).join(',') : (pos || '-')
const incidentReason = e => {
  if (e.type === 'died') return e.cause ?? '-'
  if (e.type === 'body_down') return [e.exit !== undefined && `exit ${e.exit}`, e.advice ?? e.reason].filter(Boolean).join(': ')
  if (e.type === 'routine_stopped') return `${e.reason}${e.step ? ` at ${e.step}` : ''}`
  return e.reason ?? '-'
}
const parseEvent = line => { try { return JSON.parse(line) } catch { return null } }

// logs: [{ agent, text }], the whole events.jsonl of each agent folder
export function incidentLines (logs, since, now = Date.now()) {
  const from = sinceTime(since, now)
  return logs
    .flatMap(({ agent, text }) => text.split('\n').filter(Boolean).map(parseEvent).filter(e => e && INCIDENT_TYPES.includes(e.type)).map(e => ({ agent, ...e })))
    .filter(e => Date.parse(e.t) >= from)
    .sort((a, b) => Date.parse(a.t) - Date.parse(b.t))
    .map(e => `${e.t} ${e.agent} ${e.type} ${incidentPos(e.pos)} ${incidentReason(e)}`)
}
