// Why JavaScript: thin launcher; a tiny HTTP client for `npm run restart` that must work when the cljs server is down or rebuilding.
// `npm run restart`: asks the running dashboard (started by `npm start`) to rebuild and restart the server, then waits for
// the launcher to finish (out/launcher-<port>.json: idle, build count moved), then for the new server (a different build-id
// on /api/build-id), and says whether it came up. A build the launcher reports as failed or refused ends it at once.
import { readFileSync } from 'node:fs'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { restartVerdict } from './js/launcher.mjs'

const port = Number(process.env.PORT || 3701)
const base = `http://127.0.0.1:${port}`
const waitMs = Number(process.env.RESTART_WAIT_MS || 360000)
const fetchMs = Number(process.env.RESTART_FETCH_MS || 10000)
const statusFile = join(dirname(fileURLToPath(import.meta.url)), 'out', `launcher-${port}.json`)
const get = (url, options = {}) => fetch(url, { ...options, signal: AbortSignal.timeout(fetchMs) })
const launcherStatus = () => {
  try { return JSON.parse(readFileSync(statusFile, 'utf8')) } catch { return null }
}
const sleep = (ms) => new Promise((r) => setTimeout(r, ms))

const buildId = async () => {
  try { return (await (await get(`${base}/api/build-id`)).json())['build-id'] } catch { return null }
}

// {kind:'ok', id} | {kind:'failed', failed} | {kind:'timeout'}
const waitForVerdict = async (seq0, old) => {
  const deadline = Date.now() + waitMs
  while (Date.now() < deadline) {
    await sleep(2000)
    const verdict = restartVerdict(launcherStatus(), { seq0, old, now: await buildId() })
    if (verdict.kind !== 'wait') return verdict
  }
  return { kind: 'timeout' }
}

try {
  const seq0 = launcherStatus()?.seq ?? 0
  const res = await get(`${base}/api/restart`, {
    method: 'POST', headers: { 'content-type': 'application/json' }, body: '{}',
  })
  const text = await res.text()
  console.log(`${res.status} ${text}`)
  if (res.status !== 202) process.exit(1)
  const old = JSON.parse(text)['build-id']
  const verdict = await waitForVerdict(seq0, old)
  if (verdict.kind === 'failed') {
    console.error(`the build failed${verdict.failed ? ` at the ${verdict.failed} step` : ' or was refused (too little memory)'}; the old server keeps running; see the launcher's terminal`)
    process.exit(2)
  }
  if (verdict.kind === 'timeout') {
    console.error(`no outcome after ${waitMs / 1000} s (build id still ${old}); see the launcher's terminal`)
    process.exit(2)
  }
  console.log(`restarted: build id ${verdict.id}`)
} catch (e) {
  console.error(`could not reach the dashboard on port ${port}: ${e.message}`)
  process.exit(1)
}
