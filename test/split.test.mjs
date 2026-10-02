// The module split is a pure move: every moved name is exported from its new home and still reachable through the barrel
// it used to live in, as the same binding.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import * as lib from '../src/lib.mjs'

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..')

const MOVED = [
  ['../src/lib/jobs.mjs', lib, ['tillWarning', 'farmJobs', 'jobCall', 'jobsBill', 'shortLine', 'penProbes', 'penInside', 'openingJobs', 'penOpenRefusal', 'billShortfall', 'groundJobs']],
  ['../src/lib/help.mjs', lib, ['RENAMED', 'renamedTo', 'renamedList', 'didYouMean', 'SECTIONS', 'argsUsage', 'docText', 'helpText', 'PRIMITIVES']]
]

for (const [file, barrel, names] of MOVED) {
  test(`${file} exports what moved into it, and the barrel re-exports the same bindings`, async () => {
    const mod = await import(file)
    assert.deepEqual(names.filter(n => !(n in mod)), [])
    assert.deepEqual(names.filter(n => barrel[n] !== mod[n]), [])
  })
}

// src/bot.mjs connects the moment it runs, so its modules are linked without running any of them
test('src/bot.mjs links: every name it or a module under it imports is exported where it is imported from', () => {
  const run = spawnSync(process.execPath, ['--experimental-vm-modules', '--no-warnings', 'tools/link-check.mjs', 'src/bot.mjs'], { cwd: ROOT, encoding: 'utf8' })
  assert.equal(run.stdout, '')
  assert.equal(run.status, 0)
})

// src/body/ is one folder deeper than bot.mjs was: ROOT is still the bot folder, and HOME the body's own
const bodyModule = (file, expr) => {
  const home = fs.mkdtempSync(path.join(os.tmpdir(), 'split-home-'))
  fs.writeFileSync(path.join(home, 'config.json'), JSON.stringify({ username: 'Tester' }))
  const script = `const m = await import(${JSON.stringify(path.join(ROOT, file))}); console.log(JSON.stringify(${expr})); process.exit(0)`
  const run = spawnSync(process.execPath, ['--input-type=module', '-e', script, 'argv1', home], { cwd: home, encoding: 'utf8' })
  fs.rmSync(home, { recursive: true })
  return { home, out: run.stdout.trim() && JSON.parse(run.stdout.trim().split('\n').at(-1)), err: run.stderr }
}

test('src/body/home.mjs: ROOT is the bot folder, HOME the folder named on the command line, cfg read from it', () => {
  const { home, out } = bodyModule('src/body/home.mjs', '{ root: m.ROOT, home: m.HOME, username: m.cfg.username }')
  assert.deepEqual(out, { root: ROOT, home, username: 'Tester' })
})

test('src/body/events.mjs evaluates on its own, and emit writes to HOME/events.jsonl', () => {
  const { out } = bodyModule('src/body/events.mjs', "(m.emit('probe', { n: 1 }), m.recent.at(-1).type)")
  assert.equal(out, 'probe')
})

// bot.mjs used to be the body; its parts now import each other in cycles, which is safe only while no module reads an
// imported binding before the module that declares it has run. Which module runs first depends on which one is
// imported first, so each is imported first once here (a TDZ shows as "Cannot access 'x' before initialization").
// None of them connects: only src/bot.mjs calls connect().
const BODY_MODULES = fs.readdirSync(path.join(ROOT, 'src', 'body'), { recursive: true }).filter(f => f.endsWith('.mjs')).sort()
for (const file of BODY_MODULES) {
  test(`src/body/${file} evaluates when it is the first module imported`, () => {
    const { out, err } = bodyModule(path.join('src', 'body', file), 'Object.keys(m).length > 0')
    assert.equal(err, '')
    assert.equal(out, true)
  })
}

// an imported binding is read-only: `flee = null` in a module that imports flee parses, links and loads, and throws
// "Assignment to constant variable" only when that line runs. Reassigning is the declaring module's, through its setX
const SOURCES = ['src', 'library'].flatMap(function walk (dir) {
  return fs.readdirSync(path.join(ROOT, dir), { withFileTypes: true }).flatMap(e =>
    e.isDirectory() ? walk(path.join(dir, e.name)) : e.name.endsWith('.mjs') ? [path.join(dir, e.name)] : [])
})
const importedNames = src => [...src.matchAll(/import\s*(?:(\w+)\s*,?\s*)?(?:\{([^}]*)\})?\s*from\s*'[^']*'/g)]
  .flatMap(([, def, names]) => [def, ...(names ?? '').split(',').map(n => n.trim().split(/\s+as\s+/).pop().trim())]).filter(Boolean)
const reassigned = src => {
  const code = src.replace(/\/\*[\s\S]*?\*\//g, '').replace(/(^|[^:'"`])\/\/.*$/gm, '$1')
    .replace(/`(?:\\.|[^`\\])*`|'(?:\\.|[^'\\])*'|"(?:\\.|[^"\\])*"/g, "''")
  return importedNames(src).filter(name =>
    new RegExp(`(?:^|[{;]|\\)|=>)\\s*${name}\\s*(?:(?:[-+*/%|&]|\\?\\?|\\|\\||&&)?=(?![=>])|\\+\\+|--)`, 'm').test(code))
}
for (const [name, src, expected] of [
  ['a statement', "import { flee } from './reflexes.mjs'\nflee = null", ['flee']],
  ['after an if', "import { bedWalkFailed } from './bedtime.mjs'\nif (walked) bedWalkFailed = true", ['bedWalkFailed']],
  ['in a block', "import { fighting } from './reflexes.mjs'\nif (x) { fighting = null }", ['fighting']],
  ['a count', "import { gen } from './state.mjs'\ngen++", ['gen']],
  ['through its setter', "import { flee, setFlee } from './reflexes.mjs'\nsetFlee(null)", []],
  ['a comparison', "import { task } from './state.mjs'\nif (task === mine) setTask(null)", []],
  ['a property of it', "import { mealsEaten } from './connection.mjs'\nmealsEaten.bread = 1", []]
]) {
  test(`reassigned: ${name}`, () => assert.deepEqual(reassigned(src), expected))
}
test('no module in src/ or library/ reassigns a binding it imports', () => {
  assert.deepEqual(SOURCES.flatMap(file => reassigned(fs.readFileSync(path.join(ROOT, file), 'utf8')).map(name => `${file}: ${name}`)), [])
})
