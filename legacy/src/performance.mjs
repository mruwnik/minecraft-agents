// Slow planning is a diagnostic, never a gameplay stop. Keep reports bounded
// while retaining the number of repeated slow scans for the next report.
export function createSlowScanReporter ({ emit, now = Date.now, targetMs = 1000, cooldownMs = 60000 }) {
  const seen = new Map()
  return (operation, elapsedMs, details = {}) => {
    if (!Number.isFinite(elapsedMs) || elapsedMs <= targetMs) return null
    const at = now()
    const previous = seen.get(operation)
    if (previous && at - previous.at < cooldownMs) {
      previous.suppressed++
      return { operation, elapsed_ms: Math.round(elapsedMs), reported: false }
    }
    const bug = {
      ...details,
      operation,
      elapsed_ms: Math.round(elapsedMs),
      target_ms: targetMs,
      repeated: previous?.suppressed ?? 0,
      advice: 'Performance bug: this scan exceeded the target. Its result remains valid; continue the task and investigate the scan cost.'
    }
    seen.set(operation, { at, suppressed: 0 })
    // Even failure to write a diagnostic must not discard a successful scan.
    try { emit('performance_bug', bug) } catch { return { ...bug, reported: false } }
    return { ...bug, reported: true }
  }
}

// Synchronous scans keep their return value and exceptions unchanged. The
// timing includes the scan only, not the agent's walking or model latency.
export function timedScan (report, operation, run, details = {}, now = () => performance.now()) {
  const began = now()
  try { return run() } finally { report(operation, now() - began, details) }
}
