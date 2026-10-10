// Why JavaScript: node --test file for tools/fixer-worktree.sh and tools/land.sh, run against a throwaway fake repo.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { spawnSync } from 'node:child_process'

const tools = import.meta.dirname
const CARD = 'abcd1234'
const git = (cwd, ...args) => spawnSync('git', ['-c', 'user.name=t', '-c', 'user.email=t@t', ...args], { cwd, encoding: 'utf8' })
const run = (cwd, script, ...args) => spawnSync(path.join(cwd, 'tools', script), args, { cwd, encoding: 'utf8' })

function fakeRepo() {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'land-test-'))
  const repo = path.join(root, 'repo')
  fs.mkdirSync(path.join(repo, 'tools'), { recursive: true })
  for (const f of ['fixer-worktree.sh', 'land.sh']) fs.copyFileSync(path.join(tools, f), path.join(repo, 'tools', f))
  for (const d of ['.', 'engine', 'dashboard']) fs.mkdirSync(path.join(repo, d, 'node_modules'), { recursive: true })
  fs.mkdirSync(path.join(repo, 'engine/test/fixtures/pathfinding'), { recursive: true })
  for (const d of ['worlds', 'state', 'textures']) fs.mkdirSync(path.join(repo, d))
  fs.writeFileSync(path.join(repo, '.gitignore'), 'node_modules/\nworlds/\nstate/\ntextures/\nengine/test/fixtures/\nengine/out/\ndashboard/.shadow-cljs/\n')
  for (const d of ['engine', 'dashboard']) fs.writeFileSync(path.join(repo, d, 'keep'), '')
  fs.writeFileSync(path.join(repo, 'a.txt'), 'one\n')
  fs.writeFileSync(path.join(repo, 'b.txt'), 'one\n')
  git(repo, 'init', '-q', '-b', 'main')
  git(repo, 'add', '.')
  git(repo, 'commit', '-q', '-m', 'x')
  process.env.FIXER_WT_ROOT = path.join(root, 'wts')
  return { root, repo, wt: path.join(root, 'wts', `fix-${CARD}`) }
}

const commitIn = (dir, file, text) => {
  fs.writeFileSync(path.join(dir, file), text)
  git(dir, 'commit', '-q', '-am', `edit ${file}`)
}

test('the worktree shares worlds, state and node_modules with the main checkout and sits on fix/<card8>', () => {
  const { root, repo, wt } = fakeRepo()
  try {
    const r = run(repo, 'fixer-worktree.sh', CARD)
    assert.equal(r.status, 0, r.stderr)
    assert.equal(r.stdout.trim(), wt)
    assert.equal(git(wt, 'symbolic-ref', '--short', 'HEAD').stdout.trim(), `fix/${CARD}`)
    assert.equal(git(wt, 'status', '--porcelain').stdout, '')
    for (const d of ['worlds', 'state', 'node_modules', 'engine/node_modules']) {
      assert.equal(fs.readlinkSync(path.join(wt, d)), path.join(repo, d))
    }
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

test('land fast-forwards main, then removes the worktree and its branch', () => {
  const { root, repo, wt } = fakeRepo()
  try {
    run(repo, 'fixer-worktree.sh', CARD)
    commitIn(wt, 'a.txt', 'fixed\n')
    const r = run(wt, 'land.sh')
    assert.equal(r.status, 0, r.stderr)
    assert.equal(fs.readFileSync(path.join(repo, 'a.txt'), 'utf8'), 'fixed\n')
    assert.equal(fs.existsSync(wt), false)
    assert.equal(git(repo, 'branch', '--list', `fix/${CARD}`).stdout.trim(), '')
    assert.equal(fs.existsSync(path.join(repo, '.git/commit-lock')), false)
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

test('a branch behind main is rebased and not landed until land.sh runs again', () => {
  const { root, repo, wt } = fakeRepo()
  try {
    run(repo, 'fixer-worktree.sh', CARD)
    commitIn(wt, 'a.txt', 'fixed\n')
    commitIn(repo, 'b.txt', 'other\n')
    const first = run(wt, 'land.sh')
    assert.equal(first.status, 3, first.stderr)
    assert.equal(fs.readFileSync(path.join(repo, 'a.txt'), 'utf8'), 'one\n')
    const second = run(wt, 'land.sh')
    assert.equal(second.status, 0, second.stderr)
    assert.equal(fs.readFileSync(path.join(repo, 'a.txt'), 'utf8'), 'fixed\n')
    assert.equal(fs.readFileSync(path.join(repo, 'b.txt'), 'utf8'), 'other\n')
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

test('a rebase conflict is aborted and leaves the branch and main unchanged', () => {
  const { root, repo, wt } = fakeRepo()
  try {
    run(repo, 'fixer-worktree.sh', CARD)
    commitIn(wt, 'a.txt', 'mine\n')
    commitIn(repo, 'a.txt', 'theirs\n')
    const before = git(wt, 'rev-parse', 'HEAD').stdout
    const r = run(wt, 'land.sh', '--rebased-ok')
    assert.equal(r.status, 1, r.stderr)
    assert.equal(git(wt, 'rev-parse', 'HEAD').stdout, before)
    assert.equal(git(wt, 'status', '--porcelain', '--untracked-files=no').stdout, '')
    assert.equal(fs.readFileSync(path.join(repo, 'a.txt'), 'utf8'), 'theirs\n')
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

const stubCompile = repo => {
  const log = path.join(repo, 'compile.log')
  fs.writeFileSync(path.join(repo, 'tools', 'compile'), `#!/usr/bin/env bash\necho "$@" >> ${log}\n`, { mode: 0o755 })
  return log
}
const waitFor = (file, ms) => {
  const end = Date.now() + ms
  while (!fs.existsSync(file) && Date.now() < end) spawnSync('sleep', ['0.1'])
  return fs.existsSync(file)
}

test('landing a change to the agent-tools sources rebuilds the main checkout bundle, any other landing does not', () => {
  const { root, repo, wt } = fakeRepo()
  try {
    const log = stubCompile(repo)
    run(repo, 'fixer-worktree.sh', CARD)
    commitIn(wt, 'a.txt', 'fixed\n')
    assert.equal(run(wt, 'land.sh').status, 0)
    assert.equal(waitFor(log, 1000), false, 'a.txt is no bundle source')
    run(repo, 'fixer-worktree.sh', CARD)
    fs.mkdirSync(path.join(wt, 'dashboard/src/agent_tools'), { recursive: true })
    fs.writeFileSync(path.join(wt, 'dashboard/src/agent_tools/x.cljs'), '(ns x)\n')
    git(wt, 'add', '.')
    git(wt, 'commit', '-q', '-m', 'tool')
    assert.equal(run(wt, 'land.sh').status, 0)
    assert.equal(waitFor(log, 3000), true, 'compile was started')
    assert.equal(fs.readFileSync(log, 'utf8').trim(), 'dashboard agent-tools')
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

const recordTestRun = (wt, text = 'Ran 3 tests containing 4 assertions.\n0 failures, 0 errors.\n') => {
  fs.mkdirSync(path.join(wt, '.claude/live-tests'), { recursive: true })
  fs.writeFileSync(path.join(wt, '.claude/live-tests/engine-1.log'), text)
}

test('after a rebase over commits that touch its files, the branch lands only once a clean test run is recorded', () => {
  const { root, repo, wt } = fakeRepo()
  try {
    run(repo, 'fixer-worktree.sh', CARD)
    commitIn(wt, 'a.txt', 'mine\none\n')
    commitIn(wt, 'b.txt', 'mine\n')
    commitIn(repo, 'a.txt', 'one\nother\n')
    assert.equal(run(wt, 'land.sh').status, 3)
    const refused = run(wt, 'land.sh', '--rebased-ok')
    assert.equal(refused.status, 5, refused.stderr)
    assert.match(refused.stderr, /a\.txt/)
    assert.doesNotMatch(refused.stderr, /b\.txt/, 'only the overlap is named')
    assert.equal(run(wt, 'land.sh').status, 5, 'a plain rerun is refused the same way')
    recordTestRun(wt, 'Ran 3 tests containing 4 assertions.\n2 failures, 0 errors.\n')
    assert.equal(run(wt, 'land.sh', '--rebased-ok').status, 5, 'a failing run is no evidence')
    recordTestRun(wt)
    assert.equal(run(wt, 'land.sh', '--rebased-ok').status, 0)
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

test('--rebased-ok lands at once when the commits rebased onto touch none of its files', () => {
  const { root, repo, wt } = fakeRepo()
  try {
    run(repo, 'fixer-worktree.sh', CARD)
    commitIn(wt, 'a.txt', 'mine\n')
    commitIn(repo, 'b.txt', 'other\n')
    assert.equal(run(wt, 'land.sh', '--rebased-ok').status, 0)
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

test('a rebase over docs only (*.md) lands at once, even when the branch edits the same doc', () => {
  const { root, repo, wt } = fakeRepo()
  try {
    fs.writeFileSync(path.join(repo, 'README.md'), 'a\n\nb\n\nc\n')
    git(repo, 'add', '.'); git(repo, 'commit', '-q', '-m', 'readme')
    run(repo, 'fixer-worktree.sh', CARD)
    commitIn(wt, 'a.txt', 'mine\n')
    commitIn(wt, 'README.md', 'a\n\nb\n\nc mine\n')
    commitIn(repo, 'README.md', 'a other\n\nb\n\nc\n')
    const landed = run(wt, 'land.sh')
    assert.equal(landed.status, 0, landed.stderr)
    assert.equal(fs.readFileSync(path.join(repo, 'README.md'), 'utf8'), 'a other\n\nb\n\nc mine\n')
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

test('a docs overlap needs no test run, a code commit rebased over still gets exit 3 first', () => {
  const { root, repo, wt } = fakeRepo()
  try {
    fs.writeFileSync(path.join(repo, 'README.md'), 'a\n\nb\n\nc\n')
    git(repo, 'add', '.'); git(repo, 'commit', '-q', '-m', 'readme')
    run(repo, 'fixer-worktree.sh', CARD)
    commitIn(wt, 'a.txt', 'mine\n')
    commitIn(wt, 'README.md', 'a\n\nb\n\nc mine\n')
    commitIn(repo, 'README.md', 'a other\n\nb\n\nc\n')
    commitIn(repo, 'b.txt', 'other\n')
    assert.equal(run(wt, 'land.sh').status, 3)
    const landed = run(wt, 'land.sh', '--rebased-ok')
    assert.equal(landed.status, 0, landed.stderr)
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})
