// What the dashboard knows about an ENGINE body: it has no HTTP API, so everything comes from the events it appends
// to state/agents/<name>/engine/events.jsonl. Pure, and free of node builtins: fs access stays in tools/dashboard.mjs.

// The longest gap between two events inside one run was ~10 s (a restart gap is 60 s+), so 30 s is 3x margin.
export const ENGINE_UP_MS = 30_000
export const WARN_WINDOW_MS = 10 * 60_000
export const RECENT_MAX = 10

// job.round_started / job.yielded arrive at info every 0.25-10 s per running job: they are noise in a "what happened" list
const HEARTBEATS = new Set(['round_started', 'yielded'])
const JOB_ENDS = new Set(['completed', 'failed', 'cancelled'])
const LEVELS_SHOWN = new Set(['info', 'warn', 'error'])

export const emptyEngine = Object.freeze({ last: null, pos: null, job: null, reflex: null, recent: [], warns: [] })

const describe = e => e.text ?? e.error ?? `${e.source}.${e.kind}${e.name ? ' ' + e.name : ''}`

// only a top-level job (its own id is the head of its chain) is "the current job"; a queued job is not running yet
const nextJob = (job, e) => {
  if (e.source === 'system' && e.kind === 'started') return null
  if (e.source !== 'job' || e.kind === 'queued' || !e.job || e.chain?.[0] !== e.job) return job
  if (JOB_ENDS.has(e.kind)) return job?.id === e.job ? null : job
  return { id: e.job, name: e.name ?? (job?.id === e.job ? job.name : null) }
}

const nextReflex = (reflex, e) => {
  if (e.source === 'system' && e.kind === 'started') return null
  if (e.source !== 'reflex') return reflex
  if (e.kind === 'fired') return e.reflex ?? reflex
  if (e.kind === 'ended') return e.reflex === reflex ? null : reflex
  return reflex
}

const noteworthy = e => LEVELS_SHOWN.has(e.level) && !(e.source === 'job' && HEARTBEATS.has(e.kind))

const foldOne = (state, e) => ({
  last: { t: e.t, seq: e.seq },
  pos: e.pos ?? state.pos,
  job: nextJob(state.job, e),
  reflex: nextReflex(state.reflex, e),
  recent: noteworthy(e) ? [...state.recent, { t: e.t, level: e.level, source: e.source, kind: e.kind, text: describe(e) }].slice(-RECENT_MAX) : state.recent,
  warns: e.level === 'warn' || e.level === 'error' ? [...state.warns, { t: e.t, level: e.level }] : state.warns
})

// events: oldest first. Returns a new state; the old one is untouched.
export const foldEngine = (state, events) => {
  const next = events.reduce(foldOne, state)
  if (next === state) return state
  const from = next.last.t - WARN_WINDOW_MS
  return { ...next, warns: next.warns.filter(w => w.t > from) }
}

export const engineView = (state, now) => {
  if (!state.last) return { up: false, error: 'no events yet', at: null, ageMs: null, job: null, reflex: null, pos: null, recent: [], warn10m: 0, error10m: 0 }
  const ageMs = Math.max(0, now - state.last.t)
  const up = ageMs < ENGINE_UP_MS
  const warns = state.warns.filter(w => w.t > now - WARN_WINDOW_MS)
  return {
    up,
    error: up ? null : `last event ${Math.round(ageMs / 1000)}s ago`,
    at: state.last.t,
    ageMs,
    job: state.job,
    reflex: state.reflex,
    pos: state.pos,
    recent: state.recent,
    warn10m: warns.filter(w => w.level === 'warn').length,
    error10m: warns.filter(w => w.level === 'error').length
  }
}

// ---------------------------------------------------------------- reading the file in pieces
const NEWLINE = 10
const decoder = new TextDecoder()

const concat = (a, b) => {
  const out = new Uint8Array(a.length + b.length)
  out.set(a, 0)
  out.set(b, a.length)
  return out
}

// the bytes up to and including the last newline are whole lines; the rest is a line still being written
export const completeLines = (carry, chunk) => {
  const bytes = concat(carry, chunk)
  const cut = bytes.lastIndexOf(NEWLINE) + 1
  return { complete: bytes.subarray(0, cut), rest: bytes.subarray(cut) }
}

// a read that starts mid-file begins inside some line: drop it
export const dropTornHead = bytes => bytes.subarray(bytes.indexOf(NEWLINE) + 1 || bytes.length)

export const parseEngineLines = text => text.split('\n').filter(Boolean).flatMap(line => {
  try {
    return [JSON.parse(line)]
  } catch {
    return []
  }
})

export const decodeBytes = bytes => decoder.decode(bytes)

// ---------------------------------------------------------------- agents and bodies
const parseJson = text => {
  try {
    return JSON.parse(text)
  } catch {
    return null
  }
}

// entries: [{ name, text: raw config.json }] for the folders that hold an engine. No apiPort is needed (the engine serves none).
export const parseEngineAgents = entries => entries
  .map(({ name, text }) => {
    const cfg = parseJson(text)
    return { name, username: cfg?.username ?? name, world: cfg?.world ?? null }
  })
  .sort((a, b) => a.name.localeCompare(b.name))

// the same shape mergeBodies makes for an API body, so scoping, grouping and the map treat it alike
export const engineBody = (agent, view) => ({
  ...agent,
  apiPort: null,
  harness: null,
  character: null,
  up: view.up,
  error: view.error,
  at: view.at,
  state: view.pos ? { pos: view.pos } : null,
  engine: view
})
