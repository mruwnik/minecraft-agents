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
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'split-home-'))
  const home = path.join(root, 'agents', 'Tester')
  const worldDir = path.join(root, 'worlds', 'main')
  fs.mkdirSync(home, { recursive: true })
  fs.mkdirSync(worldDir, { recursive: true })
  fs.writeFileSync(path.join(worldDir, 'world.json'), JSON.stringify({ host: '127.0.0.1', port: 25568 }))
  fs.writeFileSync(path.join(home, 'config.json'), JSON.stringify({ username: 'Tester', world: 'main' }))
  const script = `const m = await import(${JSON.stringify(path.join(ROOT, file))}); console.log(JSON.stringify(${expr})); process.exit(0)`
  const run = spawnSync(process.execPath, ['--input-type=module', '-e', script, 'argv1', home], { cwd: home, encoding: 'utf8' })
  fs.rmSync(root, { recursive: true })
  return { home, worldDir, out: run.stdout.trim() && JSON.parse(run.stdout.trim().split('\n').at(-1)), err: run.stderr }
}

test('src/body/home.mjs: ROOT is the bot folder, HOME the folder named on the command line, cfg and WORLD_DIR read from it', () => {
  const { home, worldDir, out } = bodyModule('src/body/home.mjs', '{ root: m.ROOT, home: m.HOME, username: m.cfg.username, worldDir: m.WORLD_DIR }')
  assert.deepEqual(out, { root: ROOT, home, username: 'Tester', worldDir })
})

test('src/body/events.mjs evaluates on its own, and emit writes to HOME/events.jsonl', () => {
  const { out } = bodyModule('src/body/events.mjs', "(m.emit('probe', { n: 1 }), m.recent.at(-1).type)")
  assert.equal(out, 'probe')
})

test('src/body/events.mjs keeps the shared files in the world directory', () => {
  const { worldDir, out } = bodyModule('src/body/events.mjs', "(m.saveZones(), m.savePlaces([]), { gates: m.GATES_FILE, files: (await import('node:fs')).readdirSync((await import('node:path')).dirname(m.GATES_FILE)).sort() })")
  assert.deepEqual(out, { gates: path.join(worldDir, 'gates.log'), files: ['places.json', 'world.json', 'zones.json'] })
})
