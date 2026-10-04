// Why JavaScript: tests viewmount.mjs, which stays JS (mounts the JS view server).
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import http from 'node:http'
import os from 'node:os'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { mountView } from './viewmount.mjs'

const repo = path.join(path.dirname(fileURLToPath(import.meta.url)), '..', '..')
const stateDir = fs.mkdtempSync(path.join(os.tmpdir(), 'viewmount-'))
const mount = mountView({ repo, stateDir })

const handlesCases = [
  ['/view', true], ['/view/', true], ['/pose/w/Bob', true], ['/web/app.mjs', true], ['/columns/w/0.0.bin', true],
  ['/blocks/1.21', true], ['/textures/x', true], ['/hud/w/Bob', true], ['/drive/w/Bob', true], ['/agents', true],
  ['/poses', true], ['/elements/1.21.bin', true], ['/biomes/w.json', true], ['/posesx', false],
  ['/', false], ['/nope', false], ['/viewer', false], ['/agents/x', false], ['/api/bodies', false], ['/webx', false]
]
for (const [pathname, expected] of handlesCases) {
  test(`handles ${pathname}`, () => assert.equal(mount.handles(pathname), expected))
}

const listen = () => new Promise(resolve => {
  const server = http.createServer((req, res) => {
    if (!mount.handles(new URL(req.url, 'http://x').pathname)) {
      res.writeHead(404).end('not handled')
      return
    }
    mount.handle(req, res)
  })
  server.listen(0, '127.0.0.1', () => resolve(server))
})

const fetchFrom = async (server, url) => {
  const res = await fetch(`http://127.0.0.1:${server.address().port}${url}`)
  return { status: res.status, type: res.headers.get('content-type'), text: await res.text() }
}

const routes = [
  ['/view?agent=w/X', 200, /\/web\/app\.mjs/],
  ['/view/?agent=w/X', 200, /\/web\/app\.mjs/],
  ['/web/app.mjs', 200, /import|export|const/],
  ['/agents', 200, /^\[/],
  ['/nope', 404, /not handled/]
]
for (const [url, status, body] of routes) {
  test(`GET ${url}`, async () => {
    const server = await listen()
    const got = await fetchFrom(server, url)
    server.close()
    assert.equal(got.status, status)
    assert.match(got.text, body)
  })
}

test('/web/app.mjs is javascript and /agents is json', async () => {
  const server = await listen()
  const js = await fetchFrom(server, '/web/app.mjs')
  const json = await fetchFrom(server, '/agents')
  server.close()
  assert.match(js.type, /javascript/)
  assert.match(json.type, /json/)
})

const driveCases = [
  ['forged Origin', { origin: 'http://evil.example', 'content-type': 'application/json' }, 403],
  ['Origin null', { origin: 'null', 'content-type': 'application/json' }, 403],
  ['forged Host', { host: 'evil.example', 'content-type': 'application/json' }, 403],
  ['form content type', { 'content-type': 'application/x-www-form-urlencoded' }, 403]
]
for (const [name, headers, status] of driveCases) {
  test(`POST /drive/claude/ProbeWater with ${name} is refused`, async () => {
    const server = await listen()
    const res = await new Promise((resolve, reject) => {
      const req = http.request({ host: '127.0.0.1', port: server.address().port, path: '/drive/claude/ProbeWater', method: 'POST', headers }, resolve)
      req.on('error', reject)
      req.end('{"cmd":"stop"}')
    })
    res.resume()
    server.close()
    assert.equal(res.statusCode, status)
  })
}

test('close() resolves, also when the server never listened, and the mount can be closed twice', async () => {
  const own = mountView({ repo, stateDir })
  await own.close()
  await own.close()
})
