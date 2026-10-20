// Why JavaScript: node --test file for tools/fixer-worktree.sh, run against a throwaway fake repo.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { spawn, spawnSync } from 'node:child_process'

const script = path.resolve(import.meta.dirname, 'fixer-worktree.sh')
const card = 'abcd1234'

const sh = (cwd, cmd, args, env = {}) => spawnSync(cmd, args, { cwd, encoding: 'utf8', env: { ...process.env, ...env } })
const git = (repo, ...args) => sh(repo, 'git', ['-c', 'user.name=t', '-c', 'user.email=t@t', ...args])

function fakeRepo() {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'fwt-test-'))
  const repo = path.join(root, 'repo')
  fs.mkdirSync(path.join(repo, 'tools'), { recursive: true })
  for (const d of ['engine', 'dashboard']) fs.mkdirSync(path.join(repo, d, 'node_modules'), { recursive: true })
  for (const d of ['engine', 'dashboard']) fs.writeFileSync(path.join(repo, d, 'keep'), '')
  fs.mkdirSync(path.join(repo, 'node_modules'))
  fs.mkdirSync(path.join(repo, 'worlds/test/agents'), { recursive: true })
  fs.mkdirSync(path.join(repo, 'engine/test/fixtures/pathfinding'), { recursive: true })
  fs.copyFileSync(script, path.join(repo, 'tools/fixer-worktree.sh'))
  fs.chmodSync(path.join(repo, 'tools/fixer-worktree.sh'), 0o755)
  fs.writeFileSync(path.join(repo, 'tools/seed-shadow-cache.sh'), '#!/bin/sh\nexit 0\n', { mode: 0o755 })
  fs.writeFileSync(path.join(repo, '.gitignore'), 'worlds/\nnode_modules/\nengine/test/fixtures/\n')
  git(repo, 'init', '-q', '-b', 'master')
  git(repo, 'add', '.')
  git(repo, 'commit', '-q', '-m', 'x')
  return { root, repo, tool: path.join(repo, 'tools/fixer-worktree.sh'), env: { FIXER_WT_ROOT: path.join(root, 'wts') } }
}

const withRepo = (f) => async () => {
  const ctx = fakeRepo()
  try { await f(ctx) } finally { fs.rmSync(ctx.root, { recursive: true, force: true }) }
}

const writeLate = (wt) => {
  fs.mkdirSync(path.join(wt, '.claude/live-tests'), { recursive: true })
  fs.writeFileSync(path.join(wt, '.claude/live-tests/run.log'), 'late')
  fs.mkdirSync(path.join(wt, 'worlds/test/agents/Probe' + card), { recursive: true })
  fs.writeFileSync(path.join(wt, 'worlds/test/agents/Probe' + card + '/events.edn'), 'late')
}

test('--remove refuses while a process runs inside the worktree, and succeeds once it is gone', withRepo(async ({ repo, tool, env }) => {
  const wt = sh(repo, tool, [card], env).stdout.trim().split('\n').pop()
  const child = spawn('sleep', ['30'], { cwd: wt })
  try {
    const r = sh(repo, tool, ['--remove', card], env)
    assert.notEqual(r.status, 0)
    assert.match(r.stderr, new RegExp(`pids: .*${child.pid}`))
    assert.equal(fs.existsSync(wt), true)
  } finally { child.kill() }
  await new Promise((res) => child.on('close', res))
  const r = sh(repo, tool, ['--remove', card], env)
  assert.equal(r.status, 0, r.stderr)
  assert.equal(fs.existsSync(wt), false)
}))

test('a dir left holding only test logs and probe files is cleared by --remove and by re-creating', withRepo(({ repo, tool, env }) => {
  const wt = sh(repo, tool, [card], env).stdout.trim().split('\n').pop()
  // Simulates git having removed the worktree while a run wrote its files back.
  git(repo, 'worktree', 'remove', '--force', wt)
  writeLate(wt)
  const again = sh(repo, tool, [card], env)
  assert.equal(again.status, 0, again.stderr)
  assert.equal(fs.existsSync(path.join(wt, '.git')), true)
  fs.rmSync(path.join(wt, 'worlds'))
  writeLate(wt)
  const rm = sh(repo, tool, ['--remove', card], env)
  assert.equal(rm.status, 0, rm.stderr)
  assert.equal(fs.existsSync(wt), false)
}))

test('a stub holding any other file is not deleted', withRepo(({ repo, tool, env }) => {
  const wt = path.join(env.FIXER_WT_ROOT, 'fix-' + card)
  writeLate(wt)
  fs.writeFileSync(path.join(wt, 'precious.txt'), 'x')
  const r = sh(repo, tool, [card], env)
  assert.notEqual(r.status, 0)
  assert.equal(fs.existsSync(path.join(wt, 'precious.txt')), true)
}))

test('an existing fix branch with no commits beyond its base is reused; one with commits is refused', withRepo(({ repo, tool, env }) => {
  git(repo, 'branch', `fix/${card}`)
  const ok = sh(repo, tool, [card], env)
  assert.equal(ok.status, 0, ok.stderr)
  const wt = ok.stdout.trim().split('\n').pop()
  fs.writeFileSync(path.join(wt, 'new.txt'), 'x')
  git(wt, 'add', 'new.txt')
  git(wt, 'commit', '-q', '-m', 'work')
  git(repo, 'worktree', 'remove', '--force', wt)
  const refused = sh(repo, tool, [card], env)
  assert.notEqual(refused.status, 0)
  assert.match(refused.stderr, /already has commits/)
}))
