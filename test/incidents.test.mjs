// ./mc incidents (autopilot card): what went wrong across every agent since a time, for a resumed lead session.
// The line shaping is pure and lives in src/cli.mjs (tools/mc.mjs may import nothing else); tools/incidents.mjs reads the folders.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import os from 'node:os'
import { incidentLines, sinceTime, noHomeError } from '../src/cli.mjs'
import { incidentsReport } from '../tools/incidents.mjs'

const now = Date.parse('2026-09-26T18:00:00Z')
const line = (t, type, data = {}) => JSON.stringify({ seq: 1, t, type, ...data }) + '\n'
const jizo = [
  line('2026-09-26T17:10:00Z', 'chat', { from: 'Steve', message: 'hello' }),
  line('2026-09-26T17:20:00Z', 'died', { pos: { x: 128, y: 61, z: -124 }, cause: 'slain by Zombie', carried: 'bread and 3 other items' }),
  line('2026-09-26T17:40:00Z', 'routine_stopped', { reason: 'health 6', step: 'farm.tidy place=a', place: 'a', advice: 'eat' }),
  line('2026-09-26T17:50:00Z', 'stuck', { pos: { x: 10, y: 64, z: -20 }, reason: 'boxed in for 3 min', advice: 'dig out' })
].join('')
const perrin = [
  line('2026-09-26T16:00:00Z', 'kicked', { reason: 'You are not whitelisted' }),
  line('2026-09-26T17:30:00Z', 'body_down', { exit: 143, advice: 'your body stopped (crash or kill): ./start it again' }),
  'not json at all\n',
  line('2026-09-26T17:45:00Z', 'task_done', { name: 'goto' })
].join('')
const logs = [{ agent: 'Jizo', text: jizo }, { agent: 'Perrin', text: perrin }]

for (const [name, since, expected] of [
  ['the last hour, both agents, newest last', 60, [
    '2026-09-26T17:20:00Z Jizo died 128,61,-124 slain by Zombie',
    '2026-09-26T17:30:00Z Perrin body_down - exit 143: your body stopped (crash or kill): ./start it again',
    '2026-09-26T17:40:00Z Jizo routine_stopped - health 6 at farm.tidy place=a',
    '2026-09-26T17:50:00Z Jizo stuck 10,64,-20 boxed in for 3 min'
  ]],
  ['the last three hours reach the kick', 180, [
    '2026-09-26T16:00:00Z Perrin kicked - You are not whitelisted',
    '2026-09-26T17:20:00Z Jizo died 128,61,-124 slain by Zombie',
    '2026-09-26T17:30:00Z Perrin body_down - exit 143: your body stopped (crash or kill): ./start it again',
    '2026-09-26T17:40:00Z Jizo routine_stopped - health 6 at farm.tidy place=a',
    '2026-09-26T17:50:00Z Jizo stuck 10,64,-20 boxed in for 3 min'
  ]],
  ['an ISO time', '2026-09-26T17:45:00Z', ['2026-09-26T17:50:00Z Jizo stuck 10,64,-20 boxed in for 3 min']],
  ['nothing since', 5, []]
]) {
  test(`incidentLines: ${name}`, () => assert.deepEqual(incidentLines(logs, since, now), expected))
}

for (const [name, since, expected] of [
  ['no since: the last hour', undefined, now - 3600000],
  ['minutes', 90, now - 5400000],
  ['an ISO time', '2026-09-26T12:00:00Z', Date.parse('2026-09-26T12:00:00Z')],
  ['a date alone', '2026-09-25', Date.parse('2026-09-25')]
]) {
  test(`sinceTime: ${name}`, () => assert.equal(sinceTime(since, now), expected))
}

test('sinceTime: words that are no time say so', () => assert.throws(() => sinceTime('yesterday', now), /since=yesterday is neither minutes nor an ISO time/))

test('noHomeError: incidents needs no agent chosen, like clock and dawn', () => assert.equal(noHomeError(undefined, 'incidents'), null))

test('incidentsReport: reads every agent folder with an events.jsonl, and says when nothing happened', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'incidents-'))
  for (const [agent, text] of [['Jizo', jizo], ['Perrin', perrin]]) {
    fs.mkdirSync(path.join(dir, agent))
    fs.writeFileSync(path.join(dir, agent, 'events.jsonl'), text)
  }
  fs.mkdirSync(path.join(dir, 'Empty'))
  assert.deepEqual([incidentsReport(dir, 45, now), incidentsReport(dir, 5, now)], [
    '2026-09-26T17:20:00Z Jizo died 128,61,-124 slain by Zombie\n2026-09-26T17:30:00Z Perrin body_down - exit 143: your body stopped (crash or kill): ./start it again\n2026-09-26T17:40:00Z Jizo routine_stopped - health 6 at farm.tidy place=a\n2026-09-26T17:50:00Z Jizo stuck 10,64,-20 boxed in for 3 min',
    'no incidents since 2026-09-26T17:55:00Z (died, body_down, kicked, routine_stopped, stuck across 2 agents)'
  ])
})
