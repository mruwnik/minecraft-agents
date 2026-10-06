// Why JavaScript: node --test file for tools/guard-bash.mjs.
import test from 'node:test'
import assert from 'node:assert/strict'
import { spawnSync } from 'node:child_process'
import { decide } from './guard-bash.mjs'

const bash = (command) => ({ tool_name: 'Bash', tool_input: { command } })
const suite = (s) => ({ tool_name: 'mcp__live-tests__run_tests', tool_input: { suite: s } })

const blocked = [
  'pkill -f foo', 'killall node', 'sudo pkill node', 'cd x && pkill node', 'echo hi; pkill node',
  'ls | xargs true || killall java', 'FOO=1 pkill x', 'env FOO=1 pkill x', 'timeout 5 pkill x', 'nohup pkill x &',
  'tools/test-engine engine.core-test', './tools/test-engine --full', 'cd engine && ../tools/test-engine x',
  'node --test tools/x.test.mjs', 'node --test-name-pattern=x f.mjs', 'node --no-warnings --test f.mjs',
  'npx shadow-cljs compile x', 'shadow-cljs watch x', 'npx --yes shadow-cljs release x',
  'npm test', 'npm run test', 'npm run test:unit', 'npm --prefix dashboard test', 'npm --prefix dashboard run test-all',
  'node tools/world-test.mjs f.edn', 'tools/res-slot body -- node tools/world-test.mjs f.edn',
  'tools/res-slot tests -- node --test f.mjs', 'bash tools/compile.test.sh', './tools/commit-mine.test.sh', 'x=$(pkill foo)',
  'echo `killall x`', 'bash -c "pkill foo"', 'echo a\npkill foo',
]
const allowed = [
  'cat tools/test-engine', 'grep -n \'node --test\' x', 'sed -n 1,5p tools/test-engine', 'git log --grep pkill',
  'grep -rn "pkill; killall" docs', 'echo "run npm test later"', 'ls tools/*.test.sh', 'cat tools/compile.test.sh',
  'git commit -m "fix pkill; no more npx shadow-cljs"', 'kill 1234', 'node tools/guard-bash.mjs', 'node out/test.cjs --test=x',
  'tools/compile engine test', 'tools/commit-mine --card x -m msg tools/test-engine', 'head -5 tools/world-test.mjs',
  'npm install', 'npm --prefix dashboard run restart', 'tools/res-slot status', 'git diff tools/test-engine | head',
  'cd /x && ls', 'FOO=1 ls', 'rg pkill', 'cat <<\'EOF\'\npkill x\nEOF', 'git commit -m "$(cat <<\'EOF\'\nmsg pkill\nEOF\n)"',
  'LIVE_TESTS_NONCE=abc tools/test-engine engine.x', 'ls', '',
]

for (const c of blocked) test(`blocks ${JSON.stringify(c)}`, () => {
  const d = decide(bash(c))
  assert.equal(d.block, true)
  assert.match(d.reason, /mcp__live-tests__run_tests/)
})
for (const c of allowed) test(`allows ${JSON.stringify(c)}`, () => {
  assert.equal(decide(bash(c)).block, false)
})
for (const s of ['engine-2', 'engine-3', 'world-2', 'world-3', 'world-4']) test(`blocks suite ${s}`, () => {
  const d = decide(suite(s))
  assert.equal(d.block, true)
  assert.match(d.reason, /use suite engine \/ world/)
})
for (const s of ['engine', 'world', 'tools', undefined]) test(`allows suite ${s}`, () => {
  assert.equal(decide(suite(s)).block, false)
})
test('unknown tool and malformed input are allowed', () => {
  assert.equal(decide({ tool_name: 'Read', tool_input: {} }).block, false)
  assert.equal(decide({}).block, false)
  assert.equal(decide(null).block, false)
  assert.equal(decide({ tool_name: 'Bash' }).block, false)
})

const run = (input) => spawnSync('node', [new URL('./guard-bash.mjs', import.meta.url).pathname], { input })
test('cli: exit 2 with stderr on block, 0 on allow, 0 on invalid json', () => {
  const b = run(JSON.stringify(bash('pkill x')))
  assert.equal(b.status, 2); assert.match(b.stderr.toString(), /Kill by PID only/)
  assert.equal(run(JSON.stringify(bash('ls'))).status, 0)
  assert.equal(run('not json').status, 0)
  assert.equal(run('').status, 0)
})
