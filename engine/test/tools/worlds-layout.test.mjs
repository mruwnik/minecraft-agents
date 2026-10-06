// Why JavaScript: tests the engine/tools launchers, which stay JS; a JS test is the honest check of a JS entry point (process, argv, exit code).
// Node filesystem/socket boundary tests; application path selection lives in CLJS.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import http from 'node:http'
import { spawn } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { requestFor as observe } from '../../tools/observe.mjs'
const drive = argv => tools.driveRequestFor(argv)
const socketPathFor = request => tools.driveSocketPathFor(request)
import { requestFor as world } from '../../tools/world.mjs'
import { requestFor as triggers } from '../../tools/triggers.mjs'
import tools from '../../tools/agent-tools-loader.mjs'
import { context } from '../../tools/world-data.mjs'
import { readEDN, writeEDN } from './edn.mjs'

function fixture (t) {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'world-layout-'))
  const worlds = path.join(root, 'custom-name')
  for (const w of ['a', 'b']) {
    fs.mkdirSync(path.join(worlds, w, 'agents', 'Bob', 'engine'), { recursive: true })
    fs.writeFileSync(path.join(worlds, w, 'world.json'), '{}')
  }
  fs.mkdirSync(path.join(root, 'legacy'))
  fs.symlinkSync(worlds, path.join(root, 'legacy', 'worlds'))
  t.after(() => fs.rmSync(root, { recursive: true, force: true }))
  return { root, worlds, legacy: path.join(root, 'legacy') }
}

test('every body command uses arbitrary canonical worlds directories and preserves legacy state', t => {
  const fx = fixture(t)
  for (const command of [observe, drive]) {
    const args = command === drive ? ['Bob', 'state'] : ['Bob']
    assert.match(command([...args, '--world', 'a', '--worlds', fx.worlds, '--state', fx.legacy]).error, /choose/)
  }
  for (const flags of [['--worlds', fx.worlds], ['--state', fx.legacy]]) {
    const expected = path.join(flags[0] === '--worlds' ? fx.worlds : path.join(fx.legacy, 'worlds'), 'a', 'agents', 'Bob', 'engine')
    for (const request of [observe(['Bob', '--world', 'a', ...flags]), triggers(['Bob', '--world', 'a', 'list', ...flags]),
      tools.jobsRequestFor(['Bob', '--world', 'a', 'list', ...flags]), tools.sayRequestFor(['Bob', '--world', 'a', 'hello', ...flags])]) {
      assert.equal(request.error, undefined)
      assert.equal(request.socketPath, path.join(expected, 'events.sock'))
    }
    const driven = drive(['Bob', 'state', '--world', 'a', ...flags])
    assert.equal(driven.error, undefined)
    assert.equal(socketPathFor(driven), path.join(expected, 'control.sock'))
    const submitted = world(['Bob', 'submit', 'wear', '--world', 'a', ...flags])
    assert.equal(submitted.error, undefined)
    assert.equal(submitted.socketPath, path.join(expected, 'events.sock'))
    const requests = [tools.mapOptions(['--world', 'a', ...flags, 'find']), tools.timeOptions(['--world', 'a', ...flags, 'clock']),
      tools.changesOptions(['--world', 'a', ...flags]), tools.entitiesOptions(['Bob', '--world', 'a', ...flags])]
    for (const request of requests) assert.equal(request.ctx['world-dir'], path.dirname(path.dirname(path.dirname(expected))))
    for (const kind of ['plan', 'blueprint']) {
      const request = tools.planRequestFor(kind, ['--world', 'a', ...flags, 'list'])
      assert.equal(request.error, undefined)
      assert.equal(context({ state: request.state, worlds: request.worlds, world: request.world, repoRoot: fx.root }).worldDir, path.dirname(path.dirname(path.dirname(expected))))
    }
  }
})

test('request generation records are shared across path aliases and isolated across worlds', async t => {
  const fx = fixture(t), seen = { a: [], b: [] }, servers = []
  for (const w of ['a', 'b']) {
    const server = http.createServer(async (req, res) => {
      res.setHeader('content-type', 'application/edn')
      if (req.method === 'GET') return res.end(writeEDN({ 'generation-id': `${w}-current` }))
      let text = ''; for await (const chunk of req) text += chunk
      seen[w].push(readEDN(text)['generation-id'])
      res.end('{:ok true}')
    })
    await new Promise(resolve => server.listen(path.join(fx.worlds, w, 'agents', 'Bob', 'engine', 'events.sock'), resolve))
    servers.push(server)
  }
  t.after(async () => { for (const server of servers) await new Promise(resolve => server.close(resolve)) })
  const cli = fileURLToPath(new URL('../../tools/jobs.mjs', import.meta.url))
  const run = (w, flags) => new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [cli, 'Bob', '--world', w, ...flags, 'cancel-all', '--request-id', 'same-id'], { cwd: os.tmpdir() })
    let output = ''; child.stdout.on('data', chunk => { output += chunk }); child.stderr.on('data', chunk => { output += chunk })
    child.on('error', reject); child.on('close', code => { assert.equal(code, 0, output); resolve() })
  })
  await run('a', ['--worlds', fx.worlds])
  await run('b', ['--worlds', fx.worlds])
  const file = path.join(fx.worlds, 'a', 'agents', 'Bob', '.commands', 'jobs', 'same-id.edn')
  fs.writeFileSync(file, '{:generation-id "a-original"}')
  await run('a', ['--state', fx.legacy])
  assert.deepEqual(seen, { a: ['a-current', 'a-original'], b: ['b-current'] })
  assert.equal(fs.existsSync(path.join(fx.root, 'commands')), false)
})
