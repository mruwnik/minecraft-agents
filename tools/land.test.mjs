// Why JavaScript: node --test file for tools/fixer-worktree.sh, tools/land.sh and tools/merge-master.sh, run against a throwaway fake repo.
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
  for (const f of ['fixer-worktree.sh', 'land.sh', 'commit-lock.sh', 'merge-master.sh', 'seed-shadow-cache.sh']) fs.copyFileSync(path.join(tools, f), path.join(repo, 'tools', f))
  for (const d of ['.', 'engine', 'dashboard']) fs.mkdirSync(path.join(repo, d, 'node_modules'), { recursive: true })
  fs.mkdirSync(path.join(repo, 'engine/test/fixtures/pathfinding'), { recursive: true })
  for (const d of ['worlds', 'state', 'textures']) fs.mkdirSync(path.join(repo, d))
  fs.writeFileSync(path.join(repo, '.gitignore'), 'node_modules/\nworlds/\nstate/\ntextures/\nengine/test/fixtures/\nengine/out/\ndashboard/.shadow-cljs/\n')
  for (const d of ['engine', 'dashboard']) fs.writeFileSync(path.join(repo, d, 'keep'), '')
  fs.writeFileSync(path.join(repo, 'a.txt'), 'one\n')
  fs.writeFileSync(path.join(repo, 'b.txt'), 'one\n')
  git(repo, 'init', '-q', '-b', 'master')
  git(repo, 'add', '.')
  git(repo, 'commit', '-q', '-m', 'x')
  process.env.FIXER_WT_ROOT = path.join(root, 'wts')
  return { root, repo, wt: path.join(root, 'wts', `fix-${CARD}`) }
}

const deadPid = () => spawnSync('sh', ['-c', 'echo $$'], { encoding: 'utf8' }).stdout.trim()

const commitIn = (dir, file, text) => {
  fs.mkdirSync(path.dirname(path.join(dir, file)), { recursive: true })
  fs.writeFileSync(path.join(dir, file), text)
  git(dir, 'add', file)
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

test('a fixture-only branch lands on a passing fixtures-suite log; a branch with other files needs the 0 failures log', () => {
  const { root, repo, wt } = fakeRepo()
  try {
    commitIn(repo, 'engine/fixtures/world/x.edn', 'one\n')
    run(repo, 'fixer-worktree.sh', CARD)
    commitIn(wt, 'engine/fixtures/world/x.edn', 'mine\none\n')
    commitIn(repo, 'engine/fixtures/world/x.edn', 'one\nother\n')
    assert.equal(run(wt, 'land.sh').status, 3)
    const checkLog = 'world-test --check: 12 cases, 0 failed\n'
    recordTestRun(wt, 'world-test --check: 12 cases, 1 failed\n')
    assert.equal(run(wt, 'land.sh', '--rebased-ok').status, 5, 'a failing check is no evidence')
    recordTestRun(wt, checkLog)
    assert.equal(run(wt, 'land.sh', '--rebased-ok').status, 0)
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
  const other = fakeRepo()
  try {
    commitIn(other.repo, 'engine/fixtures/world/x.edn', 'one\n')
    run(other.repo, 'fixer-worktree.sh', CARD)
    commitIn(other.wt, 'engine/fixtures/world/x.edn', 'mine\none\n')
    commitIn(other.wt, 'b.txt', 'mine\n')
    commitIn(other.repo, 'engine/fixtures/world/x.edn', 'one\nother\n')
    assert.equal(run(other.wt, 'land.sh').status, 3)
    recordTestRun(other.wt, 'world-test --check: 12 cases, 0 failed\n')
    assert.equal(run(other.wt, 'land.sh', '--rebased-ok').status, 5, 'code beside the fixtures still needs a unit run')
  } finally { fs.rmSync(other.root, { recursive: true, force: true }) }
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

function withJobContract() {
  const f = fakeRepo()
  git(f.repo, 'branch', 'job-contract')
  f.jc = path.join(f.root, 'jc')
  const r = run(f.repo, 'fixer-worktree.sh', '--checkout', 'job-contract', f.jc)
  assert.equal(r.status, 0, r.stderr)
  return f
}

test('a fix cut with --base job-contract lands on the job-contract checkout, not on master', () => {
  const { root, repo, jc, wt } = withJobContract()
  try {
    assert.equal(fs.readlinkSync(path.join(jc, 'node_modules')), path.join(repo, 'node_modules'))
    const cut = run(repo, 'fixer-worktree.sh', '--base', 'job-contract', CARD)
    assert.equal(cut.status, 0, cut.stderr)
    commitIn(wt, 'a.txt', 'fixed\n')
    commitIn(repo, 'b.txt', 'master only\n')
    const landed = run(wt, 'land.sh')
    assert.equal(landed.status, 0, landed.stderr)
    assert.equal(fs.readFileSync(path.join(jc, 'a.txt'), 'utf8'), 'fixed\n')
    assert.equal(fs.readFileSync(path.join(repo, 'a.txt'), 'utf8'), 'one\n')
    assert.equal(fs.existsSync(wt), false)
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

test('merge-master brings master into job-contract, and aborts cleanly on a conflict', () => {
  const { root, repo, jc } = withJobContract()
  try {
    commitIn(repo, 'b.txt', 'master\n')
    const merged = run(repo, 'merge-master.sh')
    assert.equal(merged.status, 0, merged.stderr)
    assert.equal(fs.readFileSync(path.join(jc, 'b.txt'), 'utf8'), 'master\n')
    commitIn(jc, 'a.txt', 'jc\n')
    commitIn(repo, 'a.txt', 'master\n')
    const before = git(jc, 'rev-parse', 'HEAD').stdout
    const conflict = run(repo, 'merge-master.sh')
    assert.equal(conflict.status, 1)
    assert.match(conflict.stderr, /a\.txt/)
    assert.equal(git(jc, 'rev-parse', 'HEAD').stdout, before)
    assert.equal(git(jc, 'status', '--porcelain', '--untracked-files=no').stdout, '')
    assert.equal(fs.existsSync(path.join(repo, '.git/commit-lock')), false)
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

test('land refuses a fix branch with no recorded fixbase instead of landing on the main checkout branch', () => {
  const { root, repo, wt } = fakeRepo()
  try {
    assert.equal(run(repo, 'fixer-worktree.sh', CARD).status, 0)
    git(wt, 'config', '--unset', `branch.fix/${CARD}.fixbase`)
    commitIn(wt, 'a.txt', 'fixed\n')
    const landed = run(wt, 'land.sh')
    assert.equal(landed.status, 1)
    assert.match(landed.stderr, /fixbase/)
    assert.equal(fs.readFileSync(path.join(repo, 'a.txt'), 'utf8'), 'one\n')
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

test('merge-master keeps git\'s own error when the merge fails without a conflict', () => {
  const { root, repo, jc } = withJobContract()
  try {
    fs.writeFileSync(path.join(repo, 'c.txt'), 'master\n')
    git(repo, 'add', 'c.txt')
    git(repo, 'commit', '-q', '-m', 'add c')
    fs.writeFileSync(path.join(jc, 'c.txt'), 'untracked\n')
    const r = run(repo, 'merge-master.sh')
    assert.equal(r.status, 1)
    assert.match(r.stderr, /c\.txt/)
    assert.match(r.stderr, /overwritten/)
    assert.equal(fs.existsSync(path.join(repo, '.git/commit-lock')), false)
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

test('-h/--help print usage and exit 0, an unknown flag exits 2, and none of them lands or creates anything', () => {
  const { root, repo, wt } = fakeRepo()
  try {
    run(repo, 'fixer-worktree.sh', CARD)
    commitIn(wt, 'a.txt', 'fixed\n')
    for (const flag of ['-h', '--help']) {
      const r = run(wt, 'land.sh', flag)
      assert.equal(r.status, 0, r.stderr)
      assert.match(r.stdout, /Usage/)
      const w = run(repo, 'fixer-worktree.sh', flag)
      assert.equal(w.status, 0, w.stderr)
      assert.match(w.stdout, /Usage/)
      const c = spawnSync(path.join(tools, 'commit-mine'), [flag], { cwd: repo, encoding: 'utf8' })
      assert.equal(c.status, 0, c.stderr)
      assert.match(c.stdout, /Usage/)
    }
    const bad = run(wt, 'land.sh', '--bogus')
    assert.equal(bad.status, 2)
    assert.match(bad.stderr, /Usage/)
    assert.equal(run(repo, 'fixer-worktree.sh', '--bogus').status, 2)
    assert.equal(spawnSync(path.join(tools, 'commit-mine'), ['--bogus'], { cwd: repo, encoding: 'utf8' }).status, 2)
    assert.equal(fs.readFileSync(path.join(repo, 'a.txt'), 'utf8'), 'one\n')
    assert.equal(fs.existsSync(wt), true)
    assert.equal(git(repo, 'branch', '--list', `fix/${CARD}`).stdout.trim() !== '', true)
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

test('--remove deletes the worktree and branch even with uncommitted work, and leaves main alone', () => {
  const { root, repo, wt } = fakeRepo()
  try {
    run(repo, 'fixer-worktree.sh', CARD)
    commitIn(wt, 'a.txt', 'committed\n')
    fs.writeFileSync(path.join(wt, 'b.txt'), 'dirty\n')
    const r = run(repo, 'fixer-worktree.sh', '--remove', CARD)
    assert.equal(r.status, 0, r.stderr)
    assert.equal(fs.existsSync(wt), false)
    assert.equal(git(repo, 'branch', '--list', `fix/${CARD}`).stdout.trim(), '')
    assert.equal(fs.readFileSync(path.join(repo, 'a.txt'), 'utf8'), 'one\n')
    assert.equal(run(repo, 'fixer-worktree.sh', '--remove', 'nothex').status, 1)
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

test('exit 4: main has uncommitted edits to a file the landing changes; nothing moves, the lock is freed, and landing works once main is clean', () => {
  const { root, repo, wt } = fakeRepo()
  try {
    run(repo, 'fixer-worktree.sh', CARD)
    commitIn(wt, 'a.txt', 'fixed\n')
    fs.writeFileSync(path.join(repo, 'a.txt'), 'someone else\n')
    const before = git(repo, 'rev-parse', 'HEAD').stdout
    const r = run(wt, 'land.sh')
    assert.equal(r.status, 4, r.stderr)
    assert.equal(git(repo, 'rev-parse', 'HEAD').stdout, before)
    assert.equal(fs.readFileSync(path.join(repo, 'a.txt'), 'utf8'), 'someone else\n')
    assert.equal(fs.existsSync(wt), true)
    assert.equal(fs.existsSync(path.join(repo, '.git/commit-lock')), false)
    fs.writeFileSync(path.join(repo, 'a.txt'), 'one\n')
    assert.equal(run(wt, 'land.sh').status, 0)
    assert.equal(fs.readFileSync(path.join(repo, 'a.txt'), 'utf8'), 'fixed\n')
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

test('a commit lock left by a dead process is reclaimed', () => {
  const { root, repo, wt } = fakeRepo()
  try {
    run(repo, 'fixer-worktree.sh', CARD)
    commitIn(wt, 'a.txt', 'fixed\n')
    const lock = path.join(repo, '.git/commit-lock')
    fs.mkdirSync(lock)
    fs.writeFileSync(path.join(lock, 'pid'), `${deadPid()}\n`)
    const r = run(wt, 'land.sh')
    assert.equal(r.status, 0, r.stderr)
    assert.equal(fs.readFileSync(path.join(repo, 'a.txt'), 'utf8'), 'fixed\n')
    assert.equal(fs.existsSync(lock), false)
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

test('a commit lock held by a live process is not stolen: land.sh gives up at the timeout and lands nothing', () => {
  const { root, repo, wt } = fakeRepo()
  try {
    run(repo, 'fixer-worktree.sh', CARD)
    commitIn(wt, 'a.txt', 'fixed\n')
    const lock = path.join(repo, '.git/commit-lock')
    fs.mkdirSync(lock)
    fs.writeFileSync(path.join(lock, 'pid'), `${process.pid}\n`)
    const r = spawnSync(path.join(wt, 'tools', 'land.sh'), [], { cwd: wt, encoding: 'utf8', env: { ...process.env, COMMIT_LOCK_TIMEOUT: '1' } })
    assert.equal(r.status, 1, r.stderr)
    assert.match(r.stderr, /could not take/)
    assert.equal(fs.readFileSync(path.join(repo, 'a.txt'), 'utf8'), 'one\n')
    assert.equal(fs.readFileSync(path.join(lock, 'pid'), 'utf8'), `${process.pid}\n`)
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})
