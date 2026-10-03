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
  ['/view', true], ['/view/', true], ['/pose/Bob', true], ['/web/app.mjs', true], ['/columns/w/0.0.bin', true],
  ['/blocks/1.21', true], ['/textures/x', true], ['/hud/Bob', true], ['/drive/Bob', true], ['/agents', true],
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
  ['/view?agent=X', 200, /\/web\/app\.mjs/],
  ['/view/?agent=X', 200, /\/web\/app\.mjs/],
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
