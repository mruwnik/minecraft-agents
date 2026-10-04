import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import http from 'node:http'
import path from 'node:path'
import { spawn } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { readEDN } from './edn.mjs'

// Black-box check of the launcher and the AOT bundle over a real unix socket; the logic is tested in
// dashboard/test/agent_tools/observe_test.cljs.
const cli = fileURLToPath(new URL('../../tools/observe.mjs', import.meta.url))

test('inventory and equipment CLI emits aggregate, raw and optional slot views from the read socket', async t => {
  const state = fs.mkdtempSync(path.join(os.tmpdir(), 'observe-inventory-'))
  const socket = path.join(state, 'worlds', 'w', 'agents', 'ProbeBody', 'engine', 'events.sock')
  fs.mkdirSync(path.dirname(socket), { recursive: true })
  const server = http.createServer((_req, res) => {
    res.writeHead(200, { 'content-type': 'application/edn' })
    res.end('{:ok true :inventory [{:name "bread" :count 5 :slot 9} {:name "iron_pickaxe" :count 1 :slot 37}] :equipment {:head {:name "iron_helmet" :count 1 :durability 140}}}')
  })
  await new Promise((resolve, reject) => { server.once('error', reject); server.listen(socket, resolve) })
  t.after(async () => {
    await new Promise(resolve => server.close(resolve))
    fs.rmSync(state, { recursive: true, force: true })
  })
  const run = args => new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [cli, 'ProbeBody', '--world', 'w', '--state', state, ...args])
    let stdout = '', stderr = ''
    child.stdout.on('data', chunk => { stdout += chunk })
    child.stderr.on('data', chunk => { stderr += chunk })
    child.once('error', reject)
    child.once('close', code => resolve({ code, stdout, stderr }))
  })
  const summary = await run(['inventory'])
  assert.equal(summary.code, 0, summary.stderr)
  assert.deepEqual(readEDN(summary.stdout), {
    'total-items': 6, kinds: 2, counts: { bread: 5, iron_pickaxe: 1 },
    equipment: { head: { name: 'iron_helmet', count: 1, durability: 140 } }
  })
  const slots = await run(['inventory', '--slots'])
  assert.deepEqual(readEDN(slots.stdout).slots, [
    { name: 'bread', count: 5, slot: 9 }, { name: 'iron_pickaxe', count: 1, slot: 37 }
  ])
  const equipment = await run(['equipment'])
  assert.deepEqual(readEDN(equipment.stdout), { equipment: { head: { name: 'iron_helmet', count: 1, durability: 140 } } })
  const rawEquipment = await run(['equipment', '--raw'])
  assert.deepEqual(readEDN(rawEquipment.stdout), { equipment: { head: { name: 'iron_helmet', count: 1, durability: 140 } } })
  const raw = await run(['inventory', '--raw'])
  assert.deepEqual(readEDN(raw.stdout).inventory[0], { name: 'bread', count: 5, slot: 9 })
  assert.deepEqual(readEDN(raw.stdout).equipment.head, { name: 'iron_helmet', count: 1, durability: 140 })
})
