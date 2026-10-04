// Why JavaScript: pure helpers of tools/view-web-bench.mjs (the chromium/CDP bench is JavaScript), kept apart so node --test can import them without starting a browser.
const WINDOW_S = 5
const percentile = (list, p) => (list.length ? [...list].sort((a, b) => a - b)[Math.min(list.length - 1, Math.floor(p * list.length))] : null)
const round = (v, digits = 4) => (v === null ? null : Math.round(v * 10 ** digits) / 10 ** digits)

// the samples (with their positions) in which a scene exists
const sceneCounts = (samples, agent) => samples.map(s => s.stats.scenes[agent]?.renders).filter(c => c !== undefined)

// renders per second of every card over the samples where it exists (a card that appears mid-run counts from its first sample),
// and its slowest 5-second window (samples one second apart). Cards seen in fewer than two samples are left out.
export const cardRates = samples => {
  const names = [...new Set(samples.flatMap(s => Object.keys(s.stats.scenes)))]
  const per = Object.fromEntries(names.map(a => [a, sceneCounts(samples, a)]).filter(([, counts]) => counts.length > 1).map(([a, counts]) => {
    const deltas = counts.slice(1).map((c, i) => c - counts[i]).filter(d => d >= 0) // a negative step is a counter reset (the card re-attached), left out
    if (!deltas.length) return [a, { run: null, worstWindow: null }]
    const windows = deltas.slice(WINDOW_S - 1).map((_, i) => deltas.slice(i, i + WINDOW_S).reduce((x, y) => x + y, 0) / WINDOW_S)
    return [a, { run: round(deltas.reduce((x, y) => x + y, 0) / deltas.length, 2), worstWindow: windows.length ? round(Math.min(...windows), 2) : null }]
  }))
  const cards = Object.values(per).filter(c => c.run !== null)
  return { minRun: Math.min(...cards.map(c => c.run)), minWindow: Math.min(...cards.map(c => c.worstWindow ?? Infinity)), seconds: samples.length - 1, perCard: per }
}

// mean fps of one scene over the samples that have it
export const meanSceneFps = (samples, agent) => {
  const fps = samples.map(s => s.stats.scenes[agent]?.fps).filter(v => v !== undefined)
  return fps.reduce((a, b) => a + b, 0) / fps.length
}

export const dashboardLoadError = (url, errorText) => `cannot load ${url}: ${errorText}`

export { percentile, round }
