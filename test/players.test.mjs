// Telling a human player from an agent body, in generic terms: an agent is any name with a folder under
// state/agents/ holding a config.json; everyone else seen in a `players` map is a human
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'fs'
import os from 'os'
import path from 'path'
import { agentNames, splitPlayers } from '../src/players.mjs'

const agentTree = (agents, extras = []) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'players-'))
  fs.mkdirSync(path.join(root, 'state', 'agents'), { recursive: true })
  for (const name of agents) {
    fs.mkdirSync(path.join(root, 'state', 'agents', name), { recursive: true })
    fs.writeFileSync(path.join(root, 'state', 'agents', name, 'config.json'), '{}')
  }
  for (const name of extras) fs.mkdirSync(path.join(root, 'state', 'agents', name), { recursive: true })
  return path.join(root, 'state')
}

for (const [name, agents, extras, expected] of [
  ['every folder with a config.json counts', ['Claude', 'Chani'], [], ['Claude', 'Chani']],
  ['a folder with no config.json is not an agent', ['Claude'], ['NoConfigYet'], ['Claude']],
  ['no agents at all', [], [], []]
]) {
  test(`agentNames: ${name}`, () =>
    assert.deepEqual(agentNames(agentTree(agents, extras)).sort(), expected.sort()))
}

for (const [name, players, agents, expected] of [
  ['splits agents from humans', { Claude: { x: 1, y: 2, z: 3 }, Steve: 'out of sight' }, ['Claude'], { agents: ['Claude'], humans: ['Steve'] }],
  ['all agents, no humans', { Claude: { x: 1, y: 2, z: 3 }, Chani: 'out of sight' }, ['Claude', 'Chani'], { agents: ['Claude', 'Chani'], humans: [] }],
  ['all humans, no agents', { Steve: { x: 1, y: 2, z: 3 } }, [], { agents: [], humans: ['Steve'] }],
  ['no players seen at all', {}, ['Claude'], { agents: [], humans: [] }]
]) {
  test(`splitPlayers: ${name}`, () => assert.deepEqual(splitPlayers(players, agents), expected))
}
