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
  'bash tools/test-engine x', 'sh tools/test-engine x', 'bash -lc "pkill x"', 'if true; then pkill x; fi',
  'for i in 1 2; do pkill x; done', '{ pkill x; }', 'ls | xargs pkill', 'ls | xargs -n 1 pkill', 'find . -exec pkill x {} \\;',
  'command pkill x', 'command -p pkill -v',
  'sudo -u bob pkill x', 'env -i pkill x', 'env -u A pkill x', 'timeout -s KILL 5 pkill x', 'nohup -- pkill x',
  'node tools/test-run.mjs engine x', 'node tools/test-shards.mjs', 'tools/test-run.mjs x',
  'echo LIVE_TESTS_NONCE=1; pkill x', 'echo LIVE_TESTS_NONCE=1 && tools/test-engine x', 'LIVE_TESTS_NONCE=1 pkill x',
  'LIVE_TESTS_NONCE=1 tools/test-engine x', "{ (cd '/p' && export LIVE_TESTS_NONCE='n0' && pkill x)",
  "{ (cd '/p' && export LIVE_TESTS_NONCE='n0' && bash -c \"pkill x\")", "{ (cd '/p' && export LIVE_TESTS_NONCE='n0' && ls | xargs pkill)",
  "{ (cd '/p' && export LIVE_TESTS_NONCE='n0' && find . -exec killall x {} \\;)",
  'eval "pkill x"', 'eval pkill x', "eval 'cd d && killall x'", 'echo "$(pkill x)"', 'echo "a `pkill x` b"', 'echo "x $(echo $(pkill y))"',
  'busybox pkill x', 'exec -a name pkill x', 'env pkill x', 'nice pkill x', 'nice -n 5 pkill x', 'setsid pkill x', 'setsid -f pkill x',
  'stdbuf -oL pkill x', 'stdbuf -o L pkill x', 'sudo pkill x', 'doas pkill x', 'ionice -c 3 pkill x', 'builtin eval "pkill x"',
  'time nohup setsid timeout 5 env A=1 pkill x', 'echo $\'a\' $(pkill x)', 'FOO="$(killall x)" ls', 'ls "$(pkill x)"',
  'bash -c "echo \\"$(pkill x)\\""', 'eval "eval \\"pkill x\\""', 'p""kill x', '\\pkill x', '/usr/bin/killall x',
]
const allowed = [
  'cat tools/test-engine', 'grep -n \'node --test\' x', 'sed -n 1,5p tools/test-engine', 'git log --grep pkill',
  'grep -rn "pkill; killall" docs', 'echo "run npm test later"', 'ls tools/*.test.sh', 'cat tools/compile.test.sh',
  'git commit -m "fix pkill; no more npx shadow-cljs"', 'kill 1234', 'node tools/guard-bash.mjs', 'node out/test.cjs --test=x',
  'tools/compile engine test', 'tools/commit-mine --card x -m msg tools/test-engine', 'head -5 tools/world-test.mjs',
  'npm install', 'npm --prefix dashboard run restart', 'tools/res-slot status', 'git diff tools/test-engine | head',
  'cd /x && ls', 'FOO=1 ls', 'rg pkill', 'cat <<\'EOF\'\npkill x\nEOF', 'git commit -m "$(cat <<\'EOF\'\nmsg pkill\nEOF\n)"',
  "{ (cd '/p' && export LIVE_TESTS_NONCE='n0' CI='1' && 'node' '--test' 'a.mjs' 2>&1 | tee '/l' | grep -e x >> '/e'; exit \"${PIPESTATUS[0]}\"); c=$?; (exit $c); }",
  "(echo hi && { (cd '/p' && export LIVE_TESTS_NONCE='n0' && 'npm' 'test' 2>&1 | tee '/l'; exit 0); c=$?; (exit $c); })", 'ls', '',
  'grep xargs pkill docs', 'ls | xargs grep pkill', 'find . -name pkill', 'sudo -u bob ls', 'timeout -s KILL 5 ls',
  'command -v pkill', 'command -v test-engine', 'command -V pkill', 'command -pv pkill',
  'grep pkill file', 'git commit -m "mention pkill and killall"', "echo 'x $(pkill y)'", 'echo "eval pkill"', 'eval "ls"', 'eval ls',
  'busybox ls', 'exec -a name ls', 'env ls', 'nice ls', 'setsid ls', 'stdbuf -oL ls', 'echo "$(ls)"', 'echo "`ls`"', 'command -v pkill; command -v killall',
  'which pkill', 'type pkill', 'man pkill', 'echo "$(grep pkill f)"', 'git commit -m "$(cat <<\'EOF\'\nmsg\nEOF\n)"',
  'bash -lc "ls"', 'bash tools/compile engine x', 'if true; then ls; fi', 'cat tools/test-run.mjs',
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
