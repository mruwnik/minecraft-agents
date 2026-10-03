import { test } from 'node:test'
import assert from 'node:assert/strict'
import { moveWindow, slotKey, mod, keyOf } from '../tools/view/web/scene.mjs'

const radius = 1
const N = 2 * radius + 1
const start = () => {
  const owners = new Map()
  const { columns, fresh } = moveWindow({ ccx: 0, ccz: 0, radius, columns: new Map(), owners })
  return { columns, owners, fresh }
}

test('a first window holds (2r+1)^2 fresh pending columns, each owning its own slot', () => {
  const { columns, owners, fresh } = start()
  assert.equal(columns.size, 9)
  assert.equal(fresh.length, 9)
  assert.equal(owners.size, 9)
  assert.ok([...columns.values()].every(c => c.status === 'pending'))
})

test('distance is from the eye chunk', () => {
  const { columns } = start()
  assert.equal(columns.get('0.0').dist, 0)
  assert.equal(columns.get('1.1').dist, Math.SQRT2)
})

const moves = [
  ['one east', 1, 0, 3, ['-1.-1', '-1.0', '-1.1']],
  ['one south', 0, 1, 3, ['-1.-1', '0.-1', '1.-1']],
  ['stay', 0, 0, 0, []],
  ['a jump', 10, -7, 9, ['-1.0', '0.0', '1.0']]
]
for (const [name, dx, dz, freshCount, gone] of moves) {
  test(`moving ${name}: ${freshCount} fresh columns, the rest kept as the same objects, left columns dropped`, () => {
    const { columns, owners } = start()
    const moved = moveWindow({ ccx: dx, ccz: dz, radius, columns, owners })
    assert.equal(moved.columns.size, 9)
    assert.equal(moved.fresh.length, freshCount)
    for (const [key, column] of moved.columns) assert.equal(columns.get(key) === column, !moved.fresh.includes(column), key)
    if (freshCount === 3) for (const key of gone) assert.ok(!moved.columns.has(key), key)
  })
}

test('the slot of a departed column is taken over by the column that wraps onto it', () => {
  const { columns, owners } = start()
  const slot = slotKey(-1, 0, N)
  assert.equal(owners.get(slot), '-1.0')
  const moved = moveWindow({ ccx: 1, ccz: 0, radius, columns, owners })
  assert.equal(slotKey(2, 0, N), slot)
  assert.equal(owners.get(slot), keyOf(2, 0))
  assert.ok(moved.fresh.some(c => c.cx === 2 && c.cz === 0))
})

test('every slot is owned by exactly one wanted column after any move', () => {
  let { columns, owners } = start()
  for (const [x, z] of [[1, 0], [1, 1], [-3, 2], [-3, 2], [0, 0]]) {
    columns = moveWindow({ ccx: x, ccz: z, radius, columns, owners }).columns
    const slots = [...columns.values()].map(c => slotKey(c.cx, c.cz, N))
    assert.equal(new Set(slots).size, 9)
    for (const column of columns.values()) assert.equal(owners.get(slotKey(column.cx, column.cz, N)), keyOf(column.cx, column.cz))
  }
})

test('mod wraps negatives', () => assert.deepEqual([mod(-1, 5), mod(5, 5), mod(-6, 5)], [4, 0, 4]))
