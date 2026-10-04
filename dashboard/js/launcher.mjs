// Why JavaScript: pure state machine in Node, outside the cljs build, survives a broken
// compile. Imported by start.mjs, which depends on it for the same reason.
// Pure decisions of the dashboard launcher (start.mjs): no processes, no clocks, no IO.
// Phases: idle, building. A restart request during a build sets `pending` (they coalesce into one more build).
// `stopping` is why the server is being stopped: 'swap' (replace it) or 'quit' (the launcher is ending).

export const minMemoryMb = 3500

export const initial = { phase: 'idle', pending: false, stopping: null, quitting: false }

// ui before server: shadow-cljs writes out/server.cjs only when the server step runs, so a failed ui build leaves the old
// file. viewer last and optional: the view pages' cljs (tools/view/web/cljs/viewer.mjs), rebuilt only when its sources
// changed; its failure is a warning, because the launcher exists to keep the dashboard running.
export const buildSteps = ['ui', 'server', 'viewer']
const optionalSteps = ['viewer']

// [command, args] of a step, run in the dashboard dir. The compile JVM is shared machine-wide, hence flock (build-cljs.mjs
// takes it itself, only when it builds).
export const stepCommand = (step) =>
  step === 'viewer'
    ? ['node', ['../tools/view/build-cljs.mjs']]
    : ['flock', ['/tmp/mc-compile.lock', 'npx', 'shadow-cljs', 'compile', step]]

// whether the run stops after this step: a required step failed
export const stopsBuild = (step, code) => code !== 0 && !optionalSteps.includes(step)

// codes: exit codes of the steps run so far, in order (the run stops at the first failed required step).
export const buildOutcome = (codes) => {
  const i = codes.findIndex((c, j) => stopsBuild(buildSteps[j], c))
  return {
    ok: i < 0,
    failed: i < 0 ? null : buildSteps[i],
    warned: buildSteps.filter((step, j) => j < codes.length && codes[j] !== 0 && optionalSteps.includes(step)),
  }
}

export const onRestartRequest = (state) =>
  state.quitting ? { state, actions: [] } :
  state.phase === 'building'
    ? { state: { ...state, pending: true }, actions: [] }
    : { state: { ...state, phase: 'building' }, actions: ['build'] }

// A good build swaps the server; a pending request starts the next build after that.
export const onBuildDone = (state, ok) => {
  if (state.quitting) return { state: { ...state, phase: 'idle', pending: false }, actions: [] }
  const first = ok ? ['swap'] : ['report-failure']
  return state.pending
    ? { state: { ...state, phase: 'building', pending: false }, actions: [...first, 'build'] }
    : { state: { ...state, phase: 'idle' }, actions: first }
}

// A quit drops pending work, kills the build, and stops the server (or exits when there is none).
export const onQuit = (state, serverRunning) => ({
  state: { ...state, quitting: true, pending: false, stopping: serverRunning ? 'quit' : state.stopping },
  actions: ['kill-build', serverRunning ? 'stop-server' : 'exit:0'],
})

// An exit nobody asked for ends the launcher with the server's code (no restart loops).
export const onServerExit = (state, code, signal) => {
  if (state.stopping === 'swap') return { state: { ...state, stopping: null }, actions: ['start'] }
  if (state.stopping === 'quit') return { state, actions: ['exit:0'] }
  return { state, actions: [`exit:${typeof code === 'number' ? code : 1}`] }
}

export const parseMemAvailableMb = (meminfo) => {
  const m = /^MemAvailable:\s+(\d+)\s+kB/m.exec(meminfo)
  return m ? Math.floor(Number(m[1]) / 1024) : null
}

export const enoughMemory = (availableMb) => availableMb !== null && availableMb >= minMemoryMb
