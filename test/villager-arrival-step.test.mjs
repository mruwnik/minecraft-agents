import { test } from 'node:test'
import assert from 'node:assert/strict'
import { arrivalStepInfo, placeArrivalStep, clearArrivalStep } from '../src/villager-arrival-step.mjs'

const uuid = '87b3392e-ae93-4f51-bf07-2f53add88880'
const cell = { x: -132, y: 62, z: -169 }
const arrival = { dock: { cell: { x: cell.x, y: cell.y + 1, z: cell.z } }, rear: { x: -134, y: 64, z: -168 } }
const key = p => `${p.x},${p.y},${p.z}`
const adult = (exact = '-130.5,64,-168.5') => ({ uuid, exact, name: 'villager', baby: false, adult: true, vehicleId: null })

function stepApi ({ rows = [adult()], body = { x: -130.5, y: 64, z: -168.5 }, boats = [] } = {}) {
  const blocks = new Map([
    [key(cell), { name: 'water', solid: false }],
    [key({ ...cell, y: cell.y - 1 }), { name: 'stone', solid: true }],
    [key({ ...cell, y: cell.y + 1 }), { name: 'air', solid: false }],
    [key({ ...cell, y: cell.y + 2 }), { name: 'air', solid: false }],
    [key({ x: -133, y: 63, z: -168 }), { name: 'oak_planks', solid: true }],
    [key({ x: -133, y: 64, z: -168 }), { name: 'air', solid: false }],
    [key({ x: -133, y: 65, z: -168 }), { name: 'air', solid: false }]
  ])
  const markers = []
  const calls = []
  const api = {
    me: () => 'Test-Bot',
    pos: () => ({ ...body }),
    block: (x, y, z) => blocks.get(`${x},${y},${z}`) ?? { name: 'air', solid: false },
    inv: () => ({ cobblestone: 2 }),
    places: () => markers,
    act: async (name, args = {}) => {
      calls.push({ name, args })
      if (name === 'entity') return { found: rows }
      if (name === 'boat_state') return { boats }
      if (name === 'mark') {
        assert.ok(String(args.note ?? '').length <= 80, 'shared place marker notes are limited to 80 characters')
        const current = markers.find(p => p.name === args.name)
        if (current) Object.assign(current, args, { by: 'Test-Bot' })
        else markers.push({ ...args, by: 'Test-Bot' })
        return {}
      }
      if (name === 'unmark') { const i = markers.findIndex(p => p.name === args.name); if (i >= 0) markers.splice(i, 1); return {} }
      if (name === 'place') { blocks.set(key(args), { name: args.item, solid: true }); return { placed: 1 } }
      if (name === 'dig') { blocks.set(key(args), { name: 'water', solid: false }); return { dug: 1 } }
      throw new Error(`unexpected action ${name}`)
    },
    report: () => {}
  }
  return { api, calls, markers }
}

test('arrival step records intent before one adult-only placement and preserves marker ownership on phase update', async () => {
  const run = stepApi()
  const info = arrivalStepInfo(run.api, arrival, ['cobblestone'])
  assert.deepEqual(info.cell, cell)
  assert.equal(info.needed, true)
  const placed = await placeArrivalStep(run.api, info, uuid)
  assert.equal(placed.owned, true)
  assert.equal(run.api.block(cell.x, cell.y, cell.z).name, 'cobblestone')
  const marker = run.markers[0]
  assert.equal(marker.name, 'test-bot-villager-step--132-62--169')
  assert.equal(marker.by, 'Test-Bot')
  assert.deepEqual({ x: marker.x, y: marker.y, z: marker.z }, cell, 'note-only update retains the coordinates that identify the owned block')
  assert.deepEqual(JSON.parse(marker.note), ['cobblestone', uuid, 'p'])
  assert.ok(marker.note.length <= 80)
  const placeAt = run.calls.findIndex(c => c.name === 'place')
  assert.ok(run.calls.findIndex(c => c.name === 'mark') < placeAt)
  assert.ok(run.calls.findIndex((c, i) => i > placeAt && c.name === 'mark') > placeAt)
})

test('arrival step refuses unknown/nonadult, occupied, overlapping-operator, or boat-occupied placements without marking', async () => {
  const cases = [
    { rows: [{ ...adult(), baby: null, adult: null }] },
    { rows: [{ ...adult(), vehicleId: 12 }] },
    { rows: [adult('-131.5,62,-168')] },
    { body: { x: -131.5, y: 62, z: -168 } },
    { boats: [{ id: 77, exact: '-131.5,62,-168.5' }] }
  ]
  for (const [index, options] of cases.entries()) {
    const run = stepApi(options)
    const info = arrivalStepInfo(run.api, arrival, ['cobblestone'])
    await assert.rejects(placeArrivalStep(run.api, info, uuid), undefined, `case ${index}`)
    assert.deepEqual(run.markers, [])
    assert.equal(run.calls.some(c => c.name === 'place'), false)
  }
})

test('arrival step ownership is retained until the exact UUID is secure, then only that block is removed', async () => {
  const run = stepApi()
  let info = arrivalStepInfo(run.api, arrival, ['cobblestone'])
  info = await placeArrivalStep(run.api, info, uuid)
  await assert.rejects(clearArrivalStep(run.api, info, []), /keep the recorded temporary step/)
  assert.equal(run.api.block(cell.x, cell.y, cell.z).name, 'cobblestone')
  assert.equal(run.markers.length, 1)
  assert.equal(run.calls.some(c => c.name === 'dig' || c.name === 'unmark'), false)

  assert.equal(await clearArrivalStep(run.api, info, [uuid]), true)
  assert.equal(run.api.block(cell.x, cell.y, cell.z).name, 'water')
  assert.equal(run.markers.length, 0)
  assert.deepEqual(run.calls.slice(-2).map(c => c.name), ['dig', 'unmark'])
})
