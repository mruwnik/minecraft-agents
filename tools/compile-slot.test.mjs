// Why JavaScript: node --test file for the compile-slot rule of tools/compile (fake repo, stub npx, no real shadow-cljs).
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { spawn, spawnSync } from 'node:child_process'

const tools = path.dirname(new URL(import.meta.url).pathname)
const sh = (cwd, cmd, args, env = {}) => spawnSync(cmd, args, { cwd, encoding: 'utf8', timeout: 60000, env: { ...process.env, ...env } })

// A main checkout and a worktree of it, each with tools/, plus a stub npx whose `shadow-cljs server` writes the pid/port files and stays up
// and whose compile notes that it runs, then holds until the file `go` exists in the checkout's dashboard/.
const setup = (t) => {
  const T = fs.mkdtempSync(path.join(os.tmpdir(), 'compile-slot-test-'))
  const main = path.join(T, 'main'), wt = path.join(T, 'wt'), bin = path.join(T, 'bin'), res = path.join(T, 'res')
  fs.mkdirSync(bin); fs.mkdirSync(res); fs.mkdirSync(path.join(main, 'tools'), { recursive: true })
  for (const f of ['compile', 'res-slot', 'res-slot.mjs']) fs.copyFileSync(path.join(tools, f), path.join(main, 'tools', f))
  for (const f of ['compile', 'res-slot']) fs.chmodSync(path.join(main, 'tools', f), 0o755)
  fs.writeFileSync(path.join(main, 'tools', 'x'), '')
  const git = (...a) => sh(main, 'git', ['-c', 'user.name=t', '-c', 'user.email=t@t', ...a])
  git('init', '-q'); git('add', '.'); git('commit', '-q', '-m', 'i'); git('worktree', 'add', '-q', '--detach', wt)
  for (const d of [main, wt]) fs.mkdirSync(path.join(d, 'dashboard', '.shadow-cljs'), { recursive: true })
  fs.writeFileSync(path.join(bin, 'npx'), `#!/bin/sh
if [ "$2" = server ]; then echo $$ > .shadow-cljs/server.pid; echo 1 > .shadow-cljs/nrepl.port; echo 1 > .shadow-cljs/http.port; exec sleep 40; fi
touch compiling; while [ -n "$HOLD" ] && [ ! -e go ]; do sleep 0.05; done
exit 0
`, { mode: 0o755 })
  fs.writeFileSync(path.join(res, 'cfg.json'), JSON.stringify({ floorMb: 0, kinds: { server: { needMb: 1, max: 1 } } }))
  const env = { PATH: `${bin}:${process.env.PATH}`, MC_COMPILE_LOCK: path.join(T, 'lock'), MC_COMPILE_QUEUE: path.join(T, 'queue'), MC_COMPILE_MIN_START_MB: '0', MC_COMPILE_POOL_SIZE: '0', MC_COMPILE_POOL_DIR: path.join(T, 'pool'), RES_SLOT_DIR: res, RES_SLOT_CONFIG: path.join(res, 'cfg.json'), RES_SLOT_MAX_WAIT_MS: '1500', RES_SLOT_POLL_MS: '200' }
  t.after(() => {
    for (const d of [main, wt]) { try { process.kill(Number(fs.readFileSync(path.join(d, 'dashboard/.shadow-cljs/server.pid'), 'utf8'))) } catch {} }
    fs.rmSync(T, { recursive: true, force: true })
  })
  return { main, wt, env, status: () => sh(main, 'node', ['tools/res-slot.mjs', 'status'], env).stdout }
}

const until = (f) => { const t0 = Date.now(); while (!f()) { if (Date.now() - t0 > 20000) throw new Error('timed out'); spawnSync('sleep', ['0.05']) } }

test('a worktree compile holds a server slot while it compiles, starts no server and frees the slot', async (t) => {
  const s = setup(t)
  const dash = path.join(s.wt, 'dashboard')
  const p = spawn('bash', ['tools/compile', 'engine', 'test'], { cwd: s.wt, env: { ...process.env, ...s.env, HOLD: '1' } })
  const done = new Promise((r) => p.on('exit', r))
  until(() => fs.existsSync(path.join(dash, 'compiling')))
  assert.match(s.status(), /server: 1\/1 in use/)
  fs.writeFileSync(path.join(dash, 'go'), '')
  assert.equal(await done, 0)
  assert.match(s.status(), /server: 0\/1 in use/)
  assert.equal(fs.existsSync(path.join(dash, '.shadow-cljs', 'server.pid')), false)
})

test('a worktree compile with no free server slot exits 75, busy', async (t) => {
  const s = setup(t)
  const dash = path.join(s.wt, 'dashboard')
  const p = spawn('bash', ['tools/compile', 'engine', 'test'], { cwd: s.wt, env: { ...process.env, ...s.env, HOLD: '1' } })
  const done = new Promise((r) => p.on('exit', r))
  t.after(() => { try { fs.writeFileSync(path.join(dash, 'go'), '') } catch {} })
  until(() => fs.existsSync(path.join(dash, 'compiling')))
  const r = sh(s.wt, 'bash', ['tools/compile', 'engine', 'test'], s.env)
  assert.equal(r.status, 75, r.stderr)
  assert.match(r.stderr, /busy/)
  fs.writeFileSync(path.join(dash, 'go'), '')
  assert.equal(await done, 0)
})

test('the main checkout starts its server without a slot', (t) => {
  const s = setup(t)
  const r = sh(s.main, 'bash', ['tools/compile', 'engine', 'test'], s.env)
  assert.equal(r.status, 0, r.stderr)
  assert.match(s.status(), /server: 0\/1 in use/)
})
