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
const TEST_ENGINE = '#!/bin/sh\n[ "$1" = engine.nope-test ] && exit 2\n[ -n "$FAKE_TE_BUSY_N" ] && { n=$(cat "$(dirname "$0")/../te.busy" 2>/dev/null || echo 0); echo $((n + 1)) > "$(dirname "$0")/../te.busy"; [ "$n" -lt "$FAKE_TE_BUSY_N" ] && exit 75; }\n[ -n "$FAKE_TE_RC" ] && exit "$FAKE_TE_RC"\nif [ -e "$(dirname "$0")/../BAD" ]; then if [ -n "$FAKE_TE_CTRL" ]; then printf "FAIL in (a-test\\t\\033[31mred\\033[0m)\\n"; else echo "FAIL in (a-test)"; fi; exit "${FAKE_TE_BADRC:-1}"; fi\nexit 0\n'
const WT = '#!/bin/sh\nif [ "$1" != --remove ] && [ -n "$WT_BUSY_N" ]; then n=$(cat "$WT_LOG.busy" 2>/dev/null || echo 0); echo $((n + 1)) > "$WT_LOG.busy"; [ "$n" -lt "$WT_BUSY_N" ] && exit 75; fi\nrepo="$(cd "$(dirname "$0")/.." && pwd)"\nif [ "$1" = --remove ]; then echo removed >> "$WT_LOG"; git -C "$repo" worktree remove --force "$2"; exit; fi\ngit -C "$repo" worktree add --detach "$2" "$1"\n'

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

test('worktree setup busy (exit 75): waits and retries, then bisects', () => {
  const { d, shas } = fakeRepo()
  try {
    const r = run(d, ['engine.a-test', '--good', shas[0], '--bad', shas[5]], { WT_BUSY_N: '2', TEST_BISECT_RETRY_SLEEP: '0' })
    assert.equal(r.status, 0, r.stdout + r.stderr)
    assert.match(r.stdout, new RegExp(`FIRST BAD: ${shas[3]} c3 culprit`))
    assert.equal(sh(d, 'git', 'worktree', 'list').split('\n').length, 1)
  } finally { rmSync(d, { recursive: true, force: true }) }
})

test('worktree setup busy past the deadline: exit 75 with a message, no worktree left', () => {
  const { d, shas } = fakeRepo()
  try {
    const r = run(d, ['engine.a-test', '--good', shas[0], '--bad', shas[5]], { WT_BUSY_N: '1000', TEST_BISECT_RETRY_SLEEP: '0', TEST_BISECT_SLOT_WAIT: '1' })
    assert.equal(r.status, 75, r.stdout + r.stderr)
    assert.match(r.stderr, /no free server slot/)
    assert.equal(sh(d, 'git', 'worktree', 'list').split('\n').length, 1)
  } finally { rmSync(d, { recursive: true, force: true }) }
})

test('test-engine busy (75) or killed (137, 143) never scores a commit bad: every step skipped, no verdict', () => {
  for (const rc of ['75', '137', '143']) {
    const { d, shas } = fakeRepo()
    try {
      const r = run(d, ['engine.a-test', '--good', shas[0], '--bad', shas[5]], { FAKE_TE_RC: rc, TEST_BISECT_BUSY_RETRIES: '1', TEST_BISECT_RETRY_SLEEP: '0' })
      assert.notEqual(r.status, 0, r.stdout + r.stderr)
      assert.doesNotMatch(r.stdout, /: bad/)
      assert.match(r.stdout, /skipped \(exit /)
      assert.doesNotMatch(r.stdout, /FIRST BAD/)
    } finally { rmSync(d, { recursive: true, force: true }) }
  }
})

test('adjacent revs with test-engine always busy: inconclusive, never FIRST BAD, no failing-test list', () => {
  const { d, shas } = fakeRepo()
  try {
    const r = run(d, ['engine.a-test', '--good', shas[4], '--bad', shas[5]], { FAKE_TE_RC: '75', TEST_BISECT_BUSY_RETRIES: '2', TEST_BISECT_RETRY_SLEEP: '0' })
    assert.notEqual(r.status, 0, r.stdout + r.stderr)
    assert.doesNotMatch(r.stdout, /FIRST BAD/)
    assert.match(r.stdout, /INCONCLUSIVE/)
    assert.equal(sh(d, 'git', 'worktree', 'list').split('\n').length, 1)
  } finally { rmSync(d, { recursive: true, force: true }) }
})

test('a busy test-engine step is retried, then gives its verdict', () => {
  const { d, shas } = fakeRepo()
  try {
    const r = run(d, ['engine.a-test', '--good', shas[4], '--bad', shas[5]], { FAKE_TE_BUSY_N: '2', TEST_BISECT_BUSY_RETRIES: '5', TEST_BISECT_RETRY_SLEEP: '0' })
    assert.equal(r.status, 0, r.stdout + r.stderr)
    assert.match(r.stdout, new RegExp(`FIRST BAD: ${shas[5]} c5 after`))
    assert.match(r.stdout, /FAIL in \(a-test\)/)
  } finally { rmSync(d, { recursive: true, force: true }) }
})

const events = (out) => out.split('\n').filter((l) => l.startsWith('@@test ')).map((l) => JSON.parse(l.slice(7)))

test('TEST_EVENTS=1: plan, per-step phases, results, progress and a failed verdict result reach stdout', () => {
  const { d, shas } = fakeRepo()
  try {
    const r = run(d, ['engine.a-test', '--good', shas[0], '--bad', shas[5]], { TEST_EVENTS: '1' })
    assert.equal(r.status, 0, r.stdout + r.stderr)
    const ev = events(r.stdout)
    assert.deepEqual(ev.find((e) => e.event === 'plan'), { event: 'plan', total: 4 })
    const phases = ev.filter((e) => e.event === 'phase').map((e) => e.name)
    assert.ok(phases.some((p) => /^step 1\/4 [0-9a-f]+: compiling$/.test(p)), phases.join('|'))
    assert.ok(phases.some((p) => /^step 1\/4 [0-9a-f]+: testing$/.test(p)), phases.join('|'))
    const results = ev.filter((e) => e.event === 'result')
    assert.ok(results.some((e) => e.outcome === 'passed' && /c2 fine$/.test(e.name)), JSON.stringify(results))
    assert.ok(results.some((e) => e.outcome === 'failed' && /c3 culprit$/.test(e.name)), JSON.stringify(results))
    const progress = ev.filter((e) => e.event === 'progress')
    assert.ok(progress.length >= 2)
    assert.deepEqual(progress[0], { event: 'progress', done: 1, total: 4, unit: 'steps' })
    const verdict = results.at(-1)
    assert.equal(verdict.outcome, 'failed')
    assert.match(verdict.name, new RegExp(`^FIRST BAD ${shas[3]}`))
    assert.match(verdict.message, /FAIL in \(a-test\)/)
  } finally { rmSync(d, { recursive: true, force: true }) }
})

test('TEST_EVENTS=1: tabs and ESC in a failing-test line keep every @@test line valid JSON', () => {
  const { d, shas } = fakeRepo()
  try {
    const r = run(d, ['engine.a-test', '--good', shas[0], '--bad', shas[5]], { TEST_EVENTS: '1', FAKE_TE_CTRL: '1' })
    assert.equal(r.status, 0, r.stdout + r.stderr)
    const verdict = events(r.stdout).filter((e) => e.event === 'result').at(-1)
    assert.match(verdict.message, /a-test\t\x1b\[31mred\x1b\[0m\)/)
  } finally { rmSync(d, { recursive: true, force: true }) }
})

test('a step exit code other than 1 still scores bad and is named in the step line and the verdict', () => {
  const { d, shas } = fakeRepo()
  try {
    const r = run(d, ['engine.a-test', '--good', shas[0], '--bad', shas[5]], { TEST_EVENTS: '1', FAKE_TE_BADRC: '3' })
    assert.match(r.stdout, /step \d+ [0-9a-f]+: bad \(exit 3\)/)
    assert.match(r.stdout, /FIRST BAD/)
    assert.match(r.stdout, /exit 3/)
    assert.ok(events(r.stdout).filter((e) => e.event === 'result').some((e) => e.outcome === 'failed' && /exit 3/.test(e.message)))
  } finally { rmSync(d, { recursive: true, force: true }) }
})

test('TEST_EVENTS=1: an inconclusive run ends with an error result', () => {
  const { d, shas } = fakeRepo()
  try {
    const r = run(d, ['engine.a-test', '--good', shas[4], '--bad', shas[5]], { TEST_EVENTS: '1', FAKE_TE_RC: '75', TEST_BISECT_BUSY_RETRIES: '1', TEST_BISECT_RETRY_SLEEP: '0' })
    const verdict = events(r.stdout).filter((e) => e.event === 'result').at(-1)
    assert.ok(events(r.stdout).some((e) => e.event === 'result' && e.outcome === 'skipped'))
    assert.equal(verdict.outcome, 'error')
    assert.match(verdict.name, /^INCONCLUSIVE/)
  } finally { rmSync(d, { recursive: true, force: true }) }
})

test('without TEST_EVENTS no @@test line is printed', () => {
  const { d, shas } = fakeRepo()
  try {
    const r = run(d, ['engine.a-test', '--good', shas[0], '--bad', shas[5]])
    assert.doesNotMatch(r.stdout, /@@test/)
  } finally { rmSync(d, { recursive: true, force: true }) }
})
