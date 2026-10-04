// `npm run restart`: asks the running dashboard (started by `npm start`) to rebuild and restart the server, then waits for
// the new server (a different build-id on /api/build-id) and says whether it came up. Plain JS: a tiny HTTP client.
const port = Number(process.env.PORT || 3701)
const base = `http://127.0.0.1:${port}`
const waitMs = Number(process.env.RESTART_WAIT_MS || 360000)
const sleep = (ms) => new Promise((r) => setTimeout(r, ms))

const buildId = async () => {
  try { return (await (await fetch(`${base}/api/build-id`)).json())['build-id'] } catch { return null }
}

const waitForNewBuild = async (old) => {
  const deadline = Date.now() + waitMs
  while (Date.now() < deadline) {
    await sleep(2000)
    const now = await buildId()
    if (now !== null && now !== old) return now
  }
  return null
}

try {
  const res = await fetch(`${base}/api/restart`, {
    method: 'POST', headers: { 'content-type': 'application/json' }, body: '{}',
  })
  const text = await res.text()
  console.log(`${res.status} ${text}`)
  if (res.status !== 202) process.exit(1)
  const old = JSON.parse(text)['build-id']
  const fresh = await waitForNewBuild(old)
  if (fresh === null) {
    console.error(`build id still ${old} after ${waitMs / 1000} s: the build failed or was refused (the old server keeps running); see the launcher's terminal`)
    process.exit(2)
  }
  console.log(`restarted: build id ${fresh}`)
} catch (e) {
  console.error(`could not reach the dashboard on port ${port}: ${e.message}`)
  process.exit(1)
}
