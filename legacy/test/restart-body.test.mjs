// restart-body stops a running body cleanly (TERM its supervisor, wait for body.pid to clear) before handing off
// to start-body, so a restart gets the same start gate, lock and log rotation as ./start. See tools/restart-body.
import test from 'node:test'
import assert from 'node:assert/strict'
import { spawn } from 'node:child_process'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'

const ROOT = path.join(import.meta.dirname, '..')
const RESTART_BODY = path.join(ROOT, 'tools', 'restart-body')

const tmpAgentDir = () => fs.mkdtempSync(path.join(os.tmpdir(), 'restart-body-'))

// stands in for the real start-body: records how it was called instead of actually launching a bot
const fakeStartBody = dir => {
  const stub = path.join(dir, 'fake-start-body')
  fs.writeFileSync(stub, '#!/bin/bash\necho "$*" >> "$(dirname "$0")/start-body.log"\n', { mode: 0o755 })
  return stub
}

const run = (args, env) => new Promise(resolve => {
  // detached: its own descendants (ps, kill, the body it signals) stay out of this test process's group
  const child = spawn(RESTART_BODY, args, { env: { ...process.env, ...env }, detached: true })
  let out = ''
  child.stdout.on('data', d => { out += d })
  child.stderr.on('data', d => { out += d })
  child.on('close', code => resolve({ code, out }))
})

const alive = pid => { try { process.kill(pid, 0); return true } catch { return false } }
const waitUntil = async (fn, ms = 2000) => {
  const start = Date.now()
  while (!fn()) {
    if (Date.now() - start > ms) throw new Error('timed out waiting')
    await new Promise(r => setTimeout(r, 20))
  }
}

test('restart-body: refuses when no body.pid exists', async () => {
  const dir = tmpAgentDir()
  const { code, out } = await run([dir])
  assert.equal(code, 1)
  assert.match(out, /no body is running; use \.\/start/)
})

test('restart-body: refuses when body.pid names a pid that is not alive', async () => {
  const dir = tmpAgentDir()
  fs.writeFileSync(path.join(dir, 'body.pid'), '999999\n')
  const { code, out } = await run([dir])
  assert.equal(code, 1)
  assert.match(out, /no body is running; use \.\/start/)
})

test('restart-body: with no supervisor to clean up after it, kills the body itself once the wait times out, then starts again', async () => {
  const dir = tmpAgentDir()
  const stub = fakeStartBody(dir)
  // a harmless sleeping child the test itself spawned, detached so it is not in restart-body's process group; its
  // parent is still this test process, which looks nothing like the supervisor script, so restart-body falls back
  // to signalling it directly
  const child = spawn('sleep', ['5'], { detached: true, stdio: 'ignore' })
  child.unref()
  fs.writeFileSync(path.join(dir, 'body.pid'), `${child.pid}\n`)

  const { code, out } = await run([dir, '--now'], { START_BODY: stub, RESTART_BODY_TIMEOUT_S: '1' })

  assert.equal(code, 0, out)
  assert.ok(!fs.existsSync(path.join(dir, 'body.pid')), 'body.pid is gone')
  assert.ok(!alive(child.pid), 'the sleeping body is actually dead')
  assert.equal(fs.readFileSync(path.join(dir, 'start-body.log'), 'utf8'), `${dir} --now\n`)
})

test('restart-body: TERMs the supervisor (the body pid\'s parent), not the body, when one is running', async () => {
  const dir = tmpAgentDir()
  const stub = fakeStartBody(dir)
  const pidFile = path.join(dir, 'body.pid')
  // a stand-in supervisor: its name must contain "start-body" to be recognised, and like the real one it owns the
  // body as a child and traps TERM to take body.pid down with it
  const supervisor = path.join(dir, 'fake-start-body-supervisor')
  fs.writeFileSync(supervisor, [
    '#!/bin/bash',
    'sleep 5 &',
    'child=$!',
    'echo $child > "$1"',
    'trap \'kill $child 2>/dev/null; rm -f "$1"\' TERM',
    'wait $child'
  ].join('\n') + '\n', { mode: 0o755 })
  spawn(supervisor, [pidFile], { detached: true, stdio: 'ignore' }).unref()

  await waitUntil(() => fs.existsSync(pidFile))
  const bodyPid = Number(fs.readFileSync(pidFile, 'utf8').trim())

  const { code, out } = await run([dir], { START_BODY: stub, RESTART_BODY_TIMEOUT_S: '5' })

  assert.equal(code, 0, out)
  assert.ok(!fs.existsSync(pidFile), 'body.pid is gone (the supervisor\'s own trap removed it)')
  assert.ok(!alive(bodyPid), 'the body is dead too')
  assert.equal(fs.readFileSync(path.join(dir, 'start-body.log'), 'utf8'), `${dir}\n`)
})
