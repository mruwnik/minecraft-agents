// Why JavaScript: node:test file like the other tools/*.test.mjs; builds a fake git repo so the bisect logic runs without compiling the engine.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { execFileSync, spawnSync } from 'node:child_process'
import { mkdtempSync, mkdirSync, writeFileSync, copyFileSync, chmodSync, rmSync, existsSync } from 'node:fs'
import { join, dirname } from 'node:path'
import { fileURLToPath } from 'node:url'

const tools = dirname(fileURLToPath(import.meta.url))
const sh = (cwd, ...a) => execFileSync(a[0], a.slice(1), { cwd, encoding: 'utf8' }).trim()

// Fake repo: tools/compile fails while file BROKEN exists, tools/test-engine fails (printing a FAIL line) while file BAD exists.
const COMPILE = '#!/bin/sh\n[ -n "$BISECT_SLOW" ] && sleep 30\n[ -e "$(dirname "$0")/../BROKEN" ] && exit 1\nexit 0\n'
const TEST_ENGINE = '#!/bin/sh\n[ "$1" = engine.nope-test ] && exit 2\nif [ -e "$(dirname "$0")/../BAD" ]; then echo "FAIL in (a-test)"; exit 1; fi\nexit 0\n'
const WT = '#!/bin/sh\nrepo="$(cd "$(dirname "$0")/.." && pwd)"\nif [ "$1" = --remove ]; then echo removed >> "$WT_LOG"; git -C "$repo" worktree remove --force "$2"; exit; fi\ngit -C "$repo" worktree add --detach "$2" "$1"\n'

function fakeRepo() {
  const d = mkdtempSync(join(process.env.TMPDIR || '/tmp', 'bisect-test-'))
  mkdirSync(join(d, 'tools'))
  copyFileSync(join(tools, 'test-bisect'), join(d, 'tools/test-bisect'))
  for (const [f, body] of [['compile', COMPILE], ['test-engine', TEST_ENGINE], ['wt.sh', WT]]) writeFileSync(join(d, 'tools', f), body)
  for (const f of ['test-bisect', 'compile', 'test-engine', 'wt.sh']) chmodSync(join(d, 'tools', f), 0o755)
  sh(d, 'git', 'init', '-q'); sh(d, 'git', 'config', 'user.email', 't@t'); sh(d, 'git', 'config', 'user.name', 't')
  const commit = (n, msg) => { sh(d, 'git', 'add', '-A'); sh(d, 'git', 'commit', '-q', '-m', msg); return sh(d, 'git', 'rev-parse', '--short', 'HEAD') }
  const shas = [commit(0, 'c0 base')]
  writeFileSync(join(d, 'BROKEN'), ''); shas.push(commit(1, 'c1 breaks compile'))
  rmSync(join(d, 'BROKEN')); writeFileSync(join(d, 'x'), '2'); shas.push(commit(2, 'c2 fine'))
  writeFileSync(join(d, 'BAD'), ''); shas.push(commit(3, 'c3 culprit'))
  writeFileSync(join(d, 'x'), '4'); shas.push(commit(4, 'c4 after'))
  writeFileSync(join(d, 'x'), '5'); shas.push(commit(5, 'c5 after'))
  return { d, shas }
}
const run = (d, args, env = {}) => spawnSync(join(d, 'tools/test-bisect'), args, {
  cwd: d, encoding: 'utf8', env: { ...process.env, TEST_BISECT_WORKTREE_SH: join(d, 'tools/wt.sh'), WT_LOG: join(d, 'wt.log'), TMPDIR: d, ...env }, timeout: 60000 })

test('finds the first bad commit, skips a compile-broken one, removes the worktree', () => {
  const { d, shas } = fakeRepo()
  try {
    const r = run(d, ['engine.a-test', '--good', shas[0], '--bad', shas[5]])
    assert.equal(r.status, 0, r.stdout + r.stderr)
    assert.match(r.stdout, new RegExp(`FIRST BAD: ${shas[3]} c3 culprit`))
    assert.match(r.stdout, /FAIL in \(a-test\)/)
    assert.match(r.stdout, /steps: [1-9]/)
    assert.equal(sh(d, 'git', 'worktree', 'list').split('\n').length, 1)
    assert.ok(existsSync(join(d, 'wt.log')))
  } finally { rmSync(d, { recursive: true, force: true }) }
})

test('adjacent revs: the bad rev itself is the answer (never tested during the bisect, so its failures are collected afterwards)', () => {
  const { d, shas } = fakeRepo()
  try {
    const r = run(d, ['engine.a-test', '--good', shas[4], '--bad', shas[5]])
    assert.equal(r.status, 0, r.stdout + r.stderr)
    assert.match(r.stdout, new RegExp(`FIRST BAD: ${shas[5]} c5 after`))
    assert.match(r.stdout, /FAIL in \(a-test\)/)
    assert.equal(sh(d, 'git', 'worktree', 'list').split('\n').length, 1)
  } finally { rmSync(d, { recursive: true, force: true }) }
})

test('killed by TERM mid-run: the worktree is still removed', () => {
  const { d, shas } = fakeRepo()
  try {
    const p = spawnSync('sh', ['-c', `"${d}/tools/test-bisect" engine.a-test --good ${shas[0]} --bad ${shas[5]} & p=$!; sleep 3; kill -TERM $p; wait $p; echo rc=$?`], {
      cwd: d, encoding: 'utf8', env: { ...process.env, BISECT_SLOW: '1', TEST_BISECT_WORKTREE_SH: join(d, 'tools/wt.sh'), WT_LOG: join(d, 'wt.log'), TMPDIR: d }, timeout: 60000 })
    assert.match(p.stdout, /rc=130/)
    assert.equal(sh(d, 'git', 'worktree', 'list').split('\n').length, 1)
  } finally { rmSync(d, { recursive: true, force: true }) }
})

test('no namespace: usage, exit 2', () => {
  const { d } = fakeRepo()
  try { assert.equal(run(d, []).status, 2) } finally { rmSync(d, { recursive: true, force: true }) }
})
