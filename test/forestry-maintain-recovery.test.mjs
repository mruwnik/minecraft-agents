import test from 'node:test'
import assert from 'node:assert/strict'
import maintain from '../library/forestry/maintain.mjs'
import { maintainTreeServices } from '../src/tree/services.mjs'
import { migratePlan, planCells, parsePlacePlan } from '../src/lib/plan.mjs'
import { scaffoldId } from '../src/scaffold/access.mjs'

const root = { x: 0, y: 0, z: 0 }
function fixture ({ fail, roots = [root] } = {}) {
  const world = new Map(), items = { oak_sapling: roots.length }, calls = [], records = new Map(), acknowledged = []
  const width = Math.max(...roots.map(p => p.x)) + 1
  const row = Array.from({ length: width }, (_, x) => roots.some(p => p.x === x) ? 'x' : '.').join('')
  const plan = migratePlan({ name: 'trees', by: 'Tester', kind: 'farm', ...root, plan: row, legend: { x: { kind: 'tree', species: 'oak' } } })
  const api = {
    plan: () => ({ ...plan, cells: planCells(plan), parsed: parsePlacePlan(plan) }),
    places: () => [plan], me: () => 'Tester', inv: () => items, pos: () => ({ x: 2.5, y: 1, z: 2.5 }),
    block: (x, y, z) => {
      const name = world.get(`${x},${y},${z}`) ?? (y <= 0 ? 'dirt' : 'air')
      return { name, solid: !['air', 'oak_sapling'].includes(name), properties: {} }
    },
    forestry: (id, value) => {
      if (value === undefined) return records.get(id) ?? null
      if (value === null) { records.delete(id); return null }
      records.set(id, value); return value
    },
    acknowledgeFailure: name => { acknowledged.push(name); return true },
    act: async (name, args = {}) => {
      calls.push({ name, args })
      await fail?.(name, args)
      if (name === 'zones') return { zones: [] }
      if (name === 'goto') return {}
      if (name === 'collect') return { picked: { oak_sapling: 1 } }
      if (name === 'place') {
        assert.ok(items[args.item] > 0, `fixture has no ${args.item}`)
        items[args.item]--
        world.set(`${args.x},${args.y},${args.z}`, args.item)
      }
      if (name === 'dig') world.set(`${args.x},${args.y},${args.z}`, 'air')
      return {}
    },
    checkpoint: async () => {}, pause: async () => {}, clock: () => ({ elapsedDays: 0, day: true, night: false }),
    until: async () => true, drops: () => [], freeSlots: () => 27, emit: () => {}, report: () => {}, note: () => {}
  }
  return { api, world, items, calls, records, acknowledged }
}
const record = () => ({ root, species: 'oak', form: 'single', wood: [], phase: 'decaying', readyAt: Date.now() - 1 })

test('forestry retries a saved decay record by collecting drops and replanting before clearing it', async () => {
  const f = fixture(), id = scaffoldId(root)
  f.api.forestry(id, record())
  const result = await maintain.run(f.api, { place: 'trees', deposit: false })
  assert.equal(f.records.has(id), false, JSON.stringify({ record: f.records.get(id), world: [...f.world], calls: f.calls, result, items: f.items }))
  assert.equal(f.world.get('0,1,0'), 'oak_sapling')
  assert.equal(result.decay_collected, 1)
  assert.ok(f.calls.findIndex(c => c.name === 'collect') < f.calls.findIndex(c => c.name === 'place'))
  assert.equal(f.calls.filter(c => c.name === 'dig').length, 0)
})

test('forestry keeps a saved record when collection fails and skips another harvest at that site', async () => {
  const f = fixture({ fail: name => { if (name === 'collect') throw Error('no path to the dropped sapling') } }), id = scaffoldId(root)
  f.api.forestry(id, record())
  const result = await maintain.run(f.api, { place: 'trees', deposit: false })
  assert.equal(f.records.get(id).phase, 'decaying')
  assert.equal(f.records.get(id).species, 'oak')
  assert.equal(f.calls.filter(c => c.name === 'dig').length, 0)
  assert.equal(f.world.get('0,1,0') ?? 'air', 'air')
})

test('forestry keeps a saved record when replanting cannot be verified', async () => {
  const f = fixture({ fail: (name, args) => { if (name === 'place' && args.item === 'oak_sapling') throw Error('placed nothing: 1 nothing to place against (first 0,1,0)') } }), id = scaffoldId(root)
  f.api.forestry(id, record())
  const result = await maintain.run(f.api, { place: 'trees', deposit: false })
  assert.equal(f.records.get(id).phase, 'decaying')
})

test('forestry acknowledges an inaccessible pending site and still collects at the next planned tree', async () => {
  const second = { x: 16, y: 0, z: 0 }
  const f = fixture({ roots: [root, second], fail: (name, args) => {
    if (name === 'goto' && args.x === root.x) throw Error('no path to the goal')
  } })
  const firstId = scaffoldId(root), secondId = scaffoldId(second)
  f.api.forestry(firstId, record())
  f.api.forestry(secondId, { ...record(), root: second })
  const result = await maintain.run(f.api, { place: 'trees', deposit: false })
  assert.equal(f.records.get(firstId).phase, 'decaying')
  assert.equal(f.records.has(secondId), false, JSON.stringify({ result, calls: f.calls, record: f.records.get(secondId), acknowledged: f.acknowledged }))
  assert.equal(f.world.get('16,1,0'), 'oak_sapling')
  assert.deepEqual(f.acknowledged, ['goto'])
  assert.equal(f.calls.filter(c => c.name === 'collect').length, 1)
})

test('forestry service helper acknowledges a handled goto so later checkpoints can continue', async () => {
  const acknowledged = [], report = { attention: [] }
  const api = {
    block: () => null, inv: () => ({}), me: () => 'Tester', checkpoint: async () => {},
    acknowledgeFailure: name => { acknowledged.push(name); return true },
    act: async name => {
      if (name === 'zones') return { zones: [] }
      throw Error('no path to the goal')
    }
  }
  await maintainTreeServices(api, [{ x: 0, y: 0, z: 0, spec: { kind: 'flower', item: 'dandelion' } }], report)
  assert.deepEqual(acknowledged, ['goto'])
  assert.ok(report.attention.some(s => /no path/.test(s)))
})

test('multi-day forestry resweeps the whole plot after a bounded wait', async () => {
  const f = fixture(), waits=[]
  let elapsedDays=0
  f.api.clock=()=>({elapsedDays,day:true,night:false})
  f.api.until=async (_condition,options)=>{waits.push(options);if(waits.length===2)elapsedDays=2;return true}
  const result=await maintain.run(f.api,{place:'trees',deposit:false,days:2})
  assert.equal(result.sweeps,2)
  assert.deepEqual(waits.map(w=>[w.timeout,w.every,w.what]),[[60,5,'next forestry sweep'],[60,5,'next forestry sweep']])
  assert.equal(f.calls.filter(c=>c.name==='zones').length>=2,true)
})
