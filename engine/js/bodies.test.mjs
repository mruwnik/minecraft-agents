import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { bodyDir, accountDir, listBodies, worldOfBodyDir, missingWorldError, storageRoot } from './bodies.mjs'

test('canonical worlds roots can have any basename and legacy state keeps its parent semantics', () => {
  const canonical = storageRoot({}, '/repo')
  assert.equal(bodyDir(canonical, 'w', 'Bob'), '/repo/worlds/w/agents/Bob')
  assert.equal(accountDir(canonical, 'Bob'), '/repo/worlds/.accounts/Bob')
  assert.equal(bodyDir(storageRoot({ worlds: '/data/custom-name' }, '/repo'), 'w', 'Bob'), '/data/custom-name/w/agents/Bob')
  assert.equal(bodyDir(storageRoot({ state: '/legacy' }, '/repo'), 'w', 'Bob'), '/legacy/worlds/w/agents/Bob')
  assert.throws(() => storageRoot({ worlds: '/data', state: '/legacy' }, '/repo'), /choose/)
})

test('bodyDir is worlds/<world>/agents/<name>', () => {
  assert.equal(bodyDir('/s', 'claude', 'Bob'), path.join('/s', 'worlds', 'claude', 'agents', 'Bob'))
})

test('the same name in two worlds is two folders', () => {
  assert.notEqual(bodyDir('/s', 'a', 'Bob'), bodyDir('/s', 'b', 'Bob'))
})

for (const [label, world, name, pattern] of [
  ['no world', undefined, 'Bob', /world/],
  ['empty world', '', 'Bob', /world/],
  ['no name', 'w', undefined, /name/],
  ['a world with a slash', 'a/b', 'Bob', /world/],
  ['a name climbing out', 'w', '..', /name/],
  ['a world climbing out', '..', 'Bob', /world/]
]) {
  test(`bodyDir refuses ${label}`, () => {
    assert.throws(() => bodyDir('/s', world, name), pattern)
  })
}

test('accountDir is worlds/.accounts/<name>, outside every world', () => {
  assert.equal(accountDir('/s', 'Bob'), path.join('/s', 'accounts', 'Bob'))
  assert.throws(() => accountDir('/s', '../x'), /name/)
})

test('worldOfBodyDir reads the world from where the folder is', () => {
  assert.equal(worldOfBodyDir(bodyDir('/s', 'claude', 'Bob')), 'claude')
})

test('missingWorldError names the flag', () => {
  assert.match(missingWorldError('--world'), /--world <world>/)
})

test('listBodies lists every body folder of every world, sorted, keyed by world and name', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'bodies-'))
  for (const [world, name] of [['w2', 'Bob'], ['w1', 'Bob'], ['w1', 'Ann']]) fs.mkdirSync(bodyDir(dir, world, name), { recursive: true })
  fs.mkdirSync(path.join(dir, 'worlds', 'empty'), { recursive: true })
  fs.writeFileSync(path.join(dir, 'worlds', 'w1', 'agents', 'stray.txt'), '')
  assert.deepEqual(listBodies(dir), [
    { world: 'w1', name: 'Ann', dir: bodyDir(dir, 'w1', 'Ann') },
    { world: 'w1', name: 'Bob', dir: bodyDir(dir, 'w1', 'Bob') },
    { world: 'w2', name: 'Bob', dir: bodyDir(dir, 'w2', 'Bob') }
  ])
  assert.deepEqual(listBodies(path.join(dir, 'nowhere')), [])
  fs.rmSync(dir, { recursive: true, force: true })
})

test('listBodies takes one world when given', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'bodies-'))
  for (const [world, name] of [['w2', 'Bob'], ['w1', 'Ann']]) fs.mkdirSync(bodyDir(dir, world, name), { recursive: true })
  assert.deepEqual(listBodies(dir, 'w2').map(b => b.name), ['Bob'])
  fs.rmSync(dir, { recursive: true, force: true })
})
