// `npm run restart`: asks the running dashboard (started by `npm start`) to rebuild and restart. Plain JS: a tiny HTTP client.
const port = Number(process.env.PORT || 3701)
try {
  const res = await fetch(`http://127.0.0.1:${port}/api/restart`, {
    method: 'POST', headers: { 'content-type': 'application/json' }, body: '{}',
  })
  const text = await res.text()
  console.log(`${res.status} ${text}`)
  process.exit(res.status === 202 ? 0 : 1)
} catch (e) {
  console.error(`could not reach the dashboard on port ${port}: ${e.message}`)
  process.exit(1)
}
