// Why JavaScript: node --test file for tools/test-shards.mjs (a Node launcher).
import fs from 'node:fs'
import path from 'node:path'
import { spawn, spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import test from 'node:test'
import assert from 'node:assert/strict'
import { splitShards, partOf, parsePart, testNamespaces, slowest, memSlots, eventForwarder, shardOutcome, failureDump, readPrior, writeAtomic, runInSlot, finishedNsMs, mergeWeights, refusal, ensureBundle, bundleHash, fullNamespaces, mergeTimingLines } from './test-shards.mjs'

test('splitShards: every namespace exactly once, loads balanced by prior timing', () => {
  const nss = ['a-test', 'b-test', 'c-test', 'd-test', 'e-test']
  const ms = { 'a-test': 100, 'b-test': 60, 'c-test': 50, 'd-test': 40, 'e-test': 10 }
  const shards = splitShards(nss, ms, 2)
  assert.deepEqual(shards.flat().sort(), nss)
  const load = shards.map((s) => s.reduce((t, n) => t + ms[n], 0))
  assert.ok(Math.abs(load[0] - load[1]) <= 20, String(load))
})

test('splitShards: unknown timings still split evenly, never empty shards beyond namespace count', () => {
  const shards = splitShards(['a-test', 'b-test', 'c-test'], {}, 5)
  assert.equal(shards.length, 3)
  assert.deepEqual(shards.flat().sort(), ['a-test', 'b-test', 'c-test'])
})

test('testNamespaces finds -test ns forms under engine/test', () => {
  const nss = testNamespaces()
  assert.ok(nss.includes('engine.go-to-test'))
  assert.ok(nss.every((n) => n.endsWith('-test')))
})

test('slowest sorts timing lines descending', () => {
  const lines = ['{"var":"x/a","ms":5}', '{"var":"x/b","ms":50}', '{"peak-rss-kb":1}']
  assert.deepEqual(slowest(lines, 1), [{ var: 'x/b', ms: 50 }])
})

test('shardOutcome: exit 0 with no test logged is a failure; a failed exit keeps its code', () => {
  const ok = ['{"var":"x/a","ms":5}']
  assert.deepEqual(shardOutcome(0, ok), { ok: true })
  assert.equal(shardOutcome(0, ['{"peak-rss-kb":1}']).ok, false)
  assert.match(shardOutcome(0, []).why, /no test/)
  assert.equal(shardOutcome(1, ok).ok, false)
})

test('memSlots: (available - res-slot.json floor) / shard peak, at least 1, at most shardMax', () => {
  const { floorMb, kinds } = JSON.parse(fs.readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), 'res-slot.json'), 'utf8'))
  const peak = kinds.tests.needMb
  assert.equal(memSlots(floorMb + peak - 100), 1)
  assert.equal(memSlots(floorMb + 2 * peak + 100), 2)
  assert.equal(memSlots(floorMb + 30000 * peak), kinds.tests.shardMax)
})

test('runInSlot: a waiting heavy shard widens its slot list as memory frees, and its marker follows', async () => {
  const dir = fs.mkdtempSync(path.join(process.env.TMPDIR ?? '/tmp', 'shard-slots-'))
  const env = { RES_SLOT_DIR: dir, RES_SLOT_HEAVY: '1' }
  const saved = Object.fromEntries(Object.keys(env).map((k) => [k, process.env[k]]))
  Object.assign(process.env, env)
  const { max } = JSON.parse(fs.readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), 'res-slot.json'), 'utf8')).kinds.tests
  const reserve = max - 1
  const holder = spawn('flock', [path.join(dir, `tests.${reserve}`), 'sleep', '30'], { stdio: 'ignore' })
  try {
    await new Promise((r) => setTimeout(r, 300))
    let slots = 1
    setTimeout(() => { slots = 3 }, 1500)
    const markerAt = () => fs.readdirSync(path.join(dir, 'heavy-wait')).map((f) => JSON.parse(fs.readFileSync(path.join(dir, 'heavy-wait', f), 'utf8')).slots)
    const run = runInSlot(() => slots, ['true'], {})
    await new Promise((r) => setTimeout(r, 700))
    assert.deepEqual(markerAt(), [[reserve]])
    assert.equal((await run).code, 0)
    assert.equal(holder.exitCode, null, 'ran while the reserve slot was still held')
    assert.deepEqual(fs.readdirSync(path.join(dir, 'heavy-wait')), [])
  } finally {
    holder.kill()
    for (const [k, v] of Object.entries(saved)) v === undefined ? delete process.env[k] : process.env[k] = v
    fs.rmSync(dir, { recursive: true, force: true })
  }
})

test('testNamespaces covers every file under engine/test that contains deftest', () => {
  const root = path.join(path.dirname(new URL(import.meta.url).pathname), '..', 'engine', 'test')
  const walk = (d) => fs.readdirSync(d, { withFileTypes: true }).flatMap((e) => e.isDirectory() ? walk(path.join(d, e.name)) : e.isFile() ? [path.join(d, e.name)] : [])
  const nss = testNamespaces()
  const missing = walk(root).filter((f) => /\(deftest\s/.test(fs.readFileSync(f, 'utf8')))
    .map((f) => [f, fs.readFileSync(f, 'utf8').match(/^\(ns\s+(?:\^\S+\s+)*([^\s()]+)[\s)]/m)?.[1]])
    .filter(([, ns]) => !ns?.endsWith('-golden')) // golden namespaces are opt-in (tools/test-engine --golden)
    .filter(([, ns]) => !nss.includes(ns)).map(([f]) => f)
  assert.deepEqual(missing, [])
})

const ev = (e) => `@@test ${JSON.stringify(e)}`
const parsed = (out) => out.map((l) => JSON.parse(l.slice('@@test '.length)))

test('eventForwarder: result and phase lines pass through, other output is dropped, a line split over chunks is joined', () => {
  const out = []
  const f = eventForwarder((l) => out.push(l), [1])
  const res = ev({ event: 'result', name: 'a/b', outcome: 'passed' })
  f.feed(0, `noise\n${res.slice(0, 20)}`)
  f.feed(0, `${res.slice(20)}\nmore noise\n${ev({ event: 'phase', name: 'x' })}\n`)
  assert.deepEqual(parsed(out), [{ event: 'result', name: 'a/b', outcome: 'passed' }, { event: 'phase', name: 'x' }])
})

test('eventForwarder: a last line without a newline is forwarded by end()', () => {
  const out = []
  const f = eventForwarder((l) => out.push(l), [1])
  f.feed(0, ev({ event: 'result', name: 'z', outcome: 'passed' }))
  assert.deepEqual(parsed(out), [])
  f.end(0)
  assert.deepEqual(parsed(out), [{ event: 'result', name: 'z', outcome: 'passed' }])
})

test('eventForwarder: one plan, emitted once every shard has reported its own, with the summed total', () => {
  const out = []
  const f = eventForwarder((l) => out.push(l), [4, 6])
  f.feed(0, `${ev({ event: 'plan', total: 30 })}\n`)
  assert.deepEqual(parsed(out).filter((e) => e.event === 'plan'), [])
  f.feed(1, `${ev({ event: 'plan', total: 50 })}\n`)
  assert.deepEqual(parsed(out).filter((e) => e.event === 'plan'), [{ event: 'plan', total: 80 }])
})

test('eventForwarder: progress total covers all shards from the start, also those not started', () => {
  const out = []
  const f = eventForwarder((l) => out.push(l), [4, 6, 5])
  f.start()
  assert.deepEqual(parsed(out), [{ event: 'progress', done: 0, total: 15, unit: 'namespaces' }])
  f.feed(0, `${ev({ event: 'progress', done: 2, total: 4, unit: 'namespaces' })}\n`)
  assert.deepEqual(parsed(out).at(-1), { event: 'progress', done: 2, total: 15, unit: 'namespaces' })
})

test('eventForwarder: waiting names the shard and the slot wait is a phase line', () => {
  const out = []
  const f = eventForwarder((l) => out.push(l), [4, 6])
  f.waiting(1)
  assert.deepEqual(parsed(out), [{ event: 'phase', name: 'waiting for slot (shard 2/2)' }])
})

test('eventForwarder: shard progress lines become one progress summed over the shards', () => {
  const out = []
  const f = eventForwarder((l) => out.push(l), [4, 6])
  f.feed(0, `${ev({ event: 'progress', done: 1, total: 4, unit: 'namespaces' })}\n`)
  f.feed(1, `${ev({ event: 'progress', done: 1, total: 6, unit: 'namespaces' })}\n`)
  f.feed(0, `${ev({ event: 'progress', done: 2, total: 4, unit: 'namespaces' })}\n`)
  assert.deepEqual(parsed(out).at(-1), { event: 'progress', done: 3, total: 10, unit: 'namespaces' })
})

test('main with TEST_EVENTS=1 announces compiling before the compile runs (no early use of events)', () => {
  const script = "import('./tools/test-shards.mjs').then((m) => m.main({ compile: () => ({ status: 3 }) }))"
  const r = spawnSync('node', ['-e', script, '--input-type=module'], { cwd: path.join(path.dirname(fileURLToPath(import.meta.url)), '..'), env: { ...process.env, TEST_EVENTS: '1' }, encoding: 'utf8' })
  assert.doesNotMatch(r.stderr, /ReferenceError/)
  assert.match(r.stdout, /"name":"compiling"/)
  assert.equal(r.status, 3)
})

test('partOf: for N in 1..6 the parts are disjoint and together cover every namespace', () => {
  const nss = Array.from({ length: 23 }, (_, k) => `ns${(k * 7) % 23}-test`)
  for (let n = 1; n <= 6; n++) {
    const parts = Array.from({ length: n }, (_, i) => partOf(nss, i + 1, n, {}))
    assert.deepEqual(parts.flat().sort(), [...nss].sort(), `N=${n}`)
  }
})

test('partOf: depends on the names and weights only, not on the order given; unknown names still land somewhere', () => {
  const nss = ['d-test', 'a-test', 'c-test', 'b-test', 'e-test']
  const w = { 'a-test': 90, 'b-test': 50, 'c-test': 40, 'd-test': 10 }
  assert.deepEqual(partOf(nss, 2, 2, w), partOf([...nss].reverse(), 2, 2, w))
  const parts = [1, 2].map((i) => partOf(nss, i, 2, w))
  assert.deepEqual(parts.flat().sort(), [...nss].sort())
})

test('partOf: parts are balanced by weight, not by name position', () => {
  const nss = Array.from({ length: 12 }, (_, k) => `n${String(k).padStart(2, '0')}-test`)
  const w = Object.fromEntries(nss.map((x, k) => [x, k % 4 === 1 ? 100 : 5]))
  const loads = [1, 2, 3, 4].map((i) => partOf(nss, i, 4, w).reduce((t, x) => t + w[x], 0))
  assert.ok(Math.max(...loads) - Math.min(...loads) <= 100, String(loads))
})

test('committed engine/test-weights.json: parts over the real namespaces are disjoint, cover all and are within 15% of each other', () => {
  const w = JSON.parse(fs.readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), '..', 'engine', 'test-weights.json'), 'utf8'))
  const nss = testNamespaces()
  for (const n of [4, 6]) {
    const parts = Array.from({ length: n }, (_, i) => partOf(nss, i + 1, n, w))
    assert.deepEqual(parts.flat().sort(), [...nss].sort())
    const loads = parts.map((p) => p.reduce((t, x) => t + (w[x] ?? 0), 0))
    assert.ok(Math.max(...loads) <= Math.min(...loads) * 1.15, String(loads))
  }
})

test('finishedNsMs: namespace times come from every shard that finished ok, a failed shard adds none', () => {
  const line = (v, ms) => JSON.stringify({ var: `#'${v}`, ms })
  const ok = { ok: true, lines: [line('a-test/x', 5), line('a-test/y', 7)] }
  const bad = { ok: false, lines: [line('b-test/x', 9)] }
  assert.deepEqual(finishedNsMs([ok, bad]), { 'a-test': 12 })
})

test('mergeWeights: fresh times replace old ones, old ones stay for namespaces not rerun, deleted namespaces drop', () => {
  assert.deepEqual(mergeWeights({ a: 1, b: 2, gone: 3 }, { a: 10, c: 4 }, ['a', 'b', 'c']), { a: 10, b: 2, c: 4 })
})

test('parsePart: "i/N" with 1 <= i <= N, otherwise an error', () => {
  assert.deepEqual(parsePart('2/4'), { i: 2, n: 4 })
  for (const bad of ['0/4', '5/4', 'x', '2', '1/0']) assert.throws(() => parsePart(bad), /--part/)
})

test('failureDump: long output is cut to its tail, short output kept whole', () => {
  assert.equal(failureDump('short', 100), 'short')
  const d = failureDump('x'.repeat(500) + 'TAIL', 100)
  assert.ok(d.length < 200 && d.endsWith('TAIL'))
})

test('failureDump: drops @@test event lines (already forwarded live; a copy would count every result twice)', () => {
  const out = 'Testing a\n@@test {"event":"result","name":"a/b","outcome":"passed"}\nFAIL in (c)\n@@test {"event":"progress","done":1,"total":2,"unit":"namespaces"}\n'
  assert.equal(failureDump(out), 'Testing a\nFAIL in (c)\n')
})

test('readPrior: a missing, empty or half-written timings file is no prior timings', () => {
  const d = fs.mkdtempSync(path.join(process.env.TMPDIR || '/tmp', 'shards-prior-'))
  try {
    const f = path.join(d, 'ns.json')
    assert.deepEqual(readPrior(f), {})
    fs.writeFileSync(f, ''); assert.deepEqual(readPrior(f), {})
    fs.writeFileSync(f, '{"a-test":12'); assert.deepEqual(readPrior(f), {})
    fs.writeFileSync(f, '{"a-test":12}'); assert.deepEqual(readPrior(f), { 'a-test': 12 })
  } finally { fs.rmSync(d, { recursive: true, force: true }) }
})

test('writeAtomic: the file is replaced by rename, no temp file is left', () => {
  const d = fs.mkdtempSync(path.join(process.env.TMPDIR || '/tmp', 'shards-atomic-'))
  try {
    const f = path.join(d, 'ns.json')
    writeAtomic(f, '{"a":1}')
    const ino = fs.statSync(f).ino
    writeAtomic(f, '{"a":2}')
    assert.equal(fs.readFileSync(f, 'utf8'), '{"a":2}')
    assert.notEqual(fs.statSync(f).ino, ino)
    assert.deepEqual(fs.readdirSync(d), ['ns.json'])
  } finally { fs.rmSync(d, { recursive: true, force: true }) }
})

test('main end: a failing shard with a large output reaches a pipe reader through main\'s own exit path (tail kept, exit 1)', () => {
  const root = fs.mkdtempSync(path.join(process.env.TMPDIR || '/tmp', 'shards-main-'))
  try {
    const here = path.dirname(fileURLToPath(import.meta.url))
    fs.mkdirSync(path.join(root, 'tools'))
    for (const f of ['test-shards.mjs', 'test-shard-exec.mjs', 'res-slot.mjs', 'test-run.mjs', 'prof-top.mjs', 'res-slot.json']) fs.copyFileSync(path.join(here, f), path.join(root, 'tools', f))
    fs.writeFileSync(path.join(root, 'tools/compile'), '#!/bin/sh\nexit 0\n', { mode: 0o755 })
    fs.mkdirSync(path.join(root, 'engine/out/test/cljs-runtime'), { recursive: true })
    fs.mkdirSync(path.join(root, 'engine/node_modules'))
    fs.mkdirSync(path.join(root, 'dashboard'))
    fs.writeFileSync(path.join(root, 'dashboard/shadow-cljs.edn'), '{:source-paths ["src"]}')
    fs.mkdirSync(path.join(root, 'engine/test'))
    fs.writeFileSync(path.join(root, 'engine/test/x_test.cljs'), '(ns x-test)\n')
    fs.writeFileSync(path.join(root, 'engine/out/test.cjs'), "process.stdout.write('y'.repeat(300000) + 'END\\n'); process.exitCode = 1\n")
    const r = spawnSync(process.execPath, [path.join(root, 'tools/test-shards.mjs'), '--shards', '1', '--slots', '1'], {
      cwd: root, maxBuffer: 1e7, encoding: 'utf8', env: { ...process.env, LIVE_TESTS_NONCE: 'n0', RES_SLOT_DIR: path.join(root, 'slots'), TEST_EVENTS: '' } })
    assert.equal(r.status, 1, r.stdout + r.stderr)
    assert.match(r.stdout, /shard 0 FAILED/)
    assert.match(r.stdout, /END\n/)
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

test('refusal: a hand run is refused; run_tests (nonce) passes', () => {
  assert.match(refusal({}), /run_tests/)
  assert.doesNotMatch(refusal({}), /MC_INTEGRATION|integration-worktree/)
  assert.equal(refusal({ LIVE_TESTS_NONCE: 'n0' }), null)
})

test('test-shard-exec: a full disk names its cause', async () => {
  const { isolateOrExplain } = await import('./test-shard-exec.mjs')
  const lines = []
  const full = () => { throw Object.assign(new Error('no space'), { code: 'ENOSPC' }) }
  assert.equal(isolateOrExplain('/e', 1, full, (m) => lines.push(m)), null)
  assert.match(lines[0], /ENOSPC: disk full/)
})

test('ensureBundle: a fresh stamp skips the compile; changed sources or a failed compile do not', () => {
  const d = fs.mkdtempSync(path.join(process.env.TMPDIR || '/tmp', 'shards-stamp-'))
  try {
    const stamp = path.join(d, 'stamp'), bundle = path.join(d, 'test.cjs')
    fs.writeFileSync(bundle, '')
    let n = 0
    const ok = () => { n++; return { status: 0 } }
    ensureBundle(ok, { hash: 'h1', stamp, bundle })
    ensureBundle(ok, { hash: 'h1', stamp, bundle })
    assert.equal(n, 1)
    ensureBundle(ok, { hash: 'h2', stamp, bundle })
    assert.equal(n, 2)
    assert.equal(ensureBundle(() => ({ status: 1 }), { hash: 'h3', stamp, bundle }).status, 1)
    assert.equal(fs.existsSync(stamp), false)
  } finally { fs.rmSync(d, { recursive: true, force: true }) }
})

test('ensureBundle: a bundle rewritten after the stamp (same sources) is stale', () => {
  const d = fs.mkdtempSync(path.join(process.env.TMPDIR || '/tmp', 'shards-sig-'))
  try {
    const stamp = path.join(d, 'stamp'), bundle = path.join(d, 'test.cjs')
    fs.writeFileSync(bundle, 'a')
    let n = 0
    const ok = () => { n++; return { status: 0 } }
    ensureBundle(ok, { hash: 'h', stamp, bundle })
    fs.writeFileSync(bundle, 'narrowed bundle')
    ensureBundle(ok, { hash: 'h', stamp, bundle })
    assert.equal(n, 2)
  } finally { fs.rmSync(d, { recursive: true, force: true }) }
})

test('bundleHash covers every source path of the shadow config, the engine package files and the dashboard sources', () => {
  const root = fs.mkdtempSync(path.join(process.env.TMPDIR || '/tmp', 'shards-hash-'))
  try {
    const put = (f, t) => { fs.mkdirSync(path.dirname(path.join(root, f)), { recursive: true }); fs.writeFileSync(path.join(root, f), t) }
    put('dashboard/shadow-cljs.edn', '{:source-paths ["src" "../engine/src" "../engine/test-support"]}')
    const files = ['dashboard/src/a.cljs', 'engine/src/b.cljs', 'engine/test-support/c.cljs', 'engine/package.json', 'engine/package-lock.json', 'engine/js/d.mjs']
    for (const f of files) put(f, '1')
    const base = bundleHash(root)
    for (const f of files) {
      put(f, '2')
      assert.notEqual(bundleHash(root), base, f)
      put(f, '1')
    }
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

test('bundleHash throws when the shadow config is missing or has no source paths', () => {
  const root = fs.mkdtempSync(path.join(process.env.TMPDIR || '/tmp', 'shards-nocfg-'))
  try {
    assert.throws(() => bundleHash(root), /missing/)
    fs.mkdirSync(path.join(root, 'dashboard'))
    for (const t of ['{}', '{:source-paths []}']) {
      fs.writeFileSync(path.join(root, 'dashboard/shadow-cljs.edn'), t)
      assert.throws(() => bundleHash(root), /no :source-paths/)
    }
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

test('parts started together compile once and each runs its tests (a part with no tests fails)', async () => {
  const root = fs.mkdtempSync(path.join(process.env.TMPDIR || '/tmp', 'shards-parts-'))
  try {
    const here = path.dirname(fileURLToPath(import.meta.url))
    fs.mkdirSync(path.join(root, 'tools'))
    for (const f of ['test-shards.mjs', 'test-shard-exec.mjs', 'res-slot.mjs', 'test-run.mjs', 'prof-top.mjs', 'res-slot.json']) fs.copyFileSync(path.join(here, f), path.join(root, 'tools', f))
    fs.writeFileSync(path.join(root, 'tools/compile'), `#!/bin/sh\necho x >> ${path.join(root, 'compiles')}\nsleep 1\nexit 0\n`, { mode: 0o755 })
    fs.mkdirSync(path.join(root, 'engine/out/test/cljs-runtime'), { recursive: true })
    fs.mkdirSync(path.join(root, 'engine/node_modules'))
    fs.mkdirSync(path.join(root, 'dashboard'))
    fs.writeFileSync(path.join(root, 'dashboard/shadow-cljs.edn'), '{:source-paths ["src"]}')
    fs.mkdirSync(path.join(root, 'engine/test'))
    for (const x of ['a', 'b']) fs.writeFileSync(path.join(root, `engine/test/${x}_test.cljs`), `(ns ${x}-test)\n`)
    fs.writeFileSync(path.join(root, 'engine/out/test.cjs'), "require('fs').appendFileSync(process.env.MC_TEST_TIMINGS, JSON.stringify({var: \"#'t/t\", ms: 1}) + '\\n')\n")
    const run = (part) => new Promise((resolve) => {
      let out = ''
      const p = spawn(process.execPath, [path.join(root, 'tools/test-shards.mjs'), '--part', part, '--shards', '1', '--slots', '2'], {
        cwd: root, env: { ...process.env, LIVE_TESTS_NONCE: 'n0', RES_SLOT_DIR: path.join(root, 'slots'), TEST_EVENTS: '' } })
      p.stdout.on('data', (c) => { out += c }); p.stderr.on('data', (c) => { out += c })
      p.on('exit', (code) => resolve({ code, out }))
    })
    const rs = await Promise.all([run('1/2'), run('2/2')])
    assert.deepEqual(rs.map((r) => r.code), [0, 0], rs.map((r) => r.out).join('\n'))
    assert.equal(fs.readFileSync(path.join(root, 'compiles'), 'utf8').trim().split('\n').length, 1)
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})

test('isolateFresh: a bundle replaced while the part waited is rebuilt, then copied; one that keeps changing fails', async () => {
  const { isolateFresh } = await import('./test-shard-exec.mjs')
  const seq = ['stale', { runDir: '/r' }]
  const rebuilt = []
  assert.deepEqual(isolateFresh('/e', '/l', () => seq.shift(), (l) => { rebuilt.push(l); return { status: 0 } }, () => {}), { runDir: '/r' })
  assert.deepEqual(rebuilt, ['/l'])
  assert.equal(isolateFresh('/e', '/l', () => 'stale', () => ({ status: 0 }), () => {}), null)
})

test('bundleStale: a missing stamp or a bundle that differs from the stamped one is stale', async () => {
  const { bundleStale } = await import('./test-shard-exec.mjs')
  const dir = fs.mkdtempSync(path.join(process.env.TMPDIR ?? '/tmp', 'bstale-'))
  try {
    fs.mkdirSync(path.join(dir, 'out'))
    const bundle = path.join(dir, 'out/test.cjs'), stamp = path.join(dir, 'out/test-full.stamp')
    fs.writeFileSync(bundle, 'abc')
    assert.equal(bundleStale(dir), true)
    const st = fs.statSync(bundle)
    fs.writeFileSync(stamp, `h\n${st.mtimeMs}:${st.size}`)
    assert.equal(bundleStale(dir), false)
    fs.writeFileSync(bundle, 'abcdef')
    assert.equal(bundleStale(dir), true)
  } finally { fs.rmSync(dir, { recursive: true, force: true }) }
})

test('fullNamespaces: the full run leaves out the fuzz namespace, which has its own suite', () => {
  const nss = fullNamespaces()
  assert.ok(testNamespaces().includes('engine.job-fuzz-test'))
  assert.ok(!nss.includes('engine.job-fuzz-test'))
})

test('mergeTimingLines: a part keeps other tests\' lines and replaces those it just ran', () => {
  const l = (var_, ms) => JSON.stringify({ var: var_, ms })
  const out = mergeTimingLines([l('a/x', 1), l('b/y', 2), JSON.stringify({ 'peak-rss-kb': 5 })], [l('b/y', 9), JSON.stringify({ 'peak-rss-kb': 7 })])
  assert.deepEqual(out.map((x) => JSON.parse(x)), [{ var: 'a/x', ms: 1 }, { var: 'b/y', ms: 9 }, { 'peak-rss-kb': 7 }])
})
