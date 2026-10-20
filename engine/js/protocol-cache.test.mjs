// Why JavaScript: tests the Node loader-boundary module protocol-cache.mjs.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'

const here = path.dirname(fileURLToPath(import.meta.url))

// Builds the play serializer pair in a fresh process and round-trips a packet; returns the parsed packet and the cached file count.
function run (dir, extraEnv = {}) {
  const script = `
    import './protocol-cache.mjs'
    import { createRequire } from 'node:module'
    const require = createRequire(import.meta.url)
    const { createSerializer, createDeserializer } = require('minecraft-protocol/src/transforms/serializer.js')
    const opts = { state: 'play', version: '1.21.4' }
    const ser = createSerializer(opts); const de = createDeserializer({ ...opts, isServer: true, noErrorLogging: true })
    const buf = ser.createPacketBuffer({ name: 'chat_command', params: { command: 'say hi' } })
    console.log(JSON.stringify(de.parsePacketBuffer(buf).data))
    await new Promise(resolve => setTimeout(resolve, 300))`
  const r = spawnSync(process.execPath, ['--input-type=module', '-e', script], { cwd: here, encoding: 'utf8', env: { ...process.env, MC_COMPILE_CACHE_DIR: dir, MC_PROTOCOL_CODE_CACHE_MS: '50', ...extraEnv } })
  assert.equal(r.status, 0, r.stderr)
  const sub = path.join(dir, 'protocol')
  const files = fs.existsSync(sub) ? fs.readdirSync(sub) : []
  return { data: JSON.parse(r.stdout), cached: files.filter(f => f.endsWith('.js')).length, codeCaches: files.filter(f => f.endsWith('.cache')).length }
}

test('cached protocol code gives the same packets as the stock compile, and is reused', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'protocache-'))
  try {
    const stock = run(dir, { MC_COMPILE_CACHE: 'off' })
    assert.equal(stock.cached, 0)
    const first = run(dir)
    assert.ok(first.cached >= 2)
    assert.equal(first.codeCaches, first.cached)
    assert.deepEqual(first.data, stock.data)
    const second = run(dir)
    assert.equal(second.cached, first.cached)
    assert.deepEqual(second.data, stock.data)
  } finally { fs.rmSync(dir, { recursive: true, force: true }) }
})
