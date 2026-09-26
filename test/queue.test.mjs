// ./mc <action> queue=true: a chore that runs after the current task instead of superseding it (card 6cf481c0: a side
// craft cancelled a running routine, `task_cancelled why=superseded by craft`). The order is pure; the body holds it.
import test from 'node:test'
import assert from 'node:assert/strict'
import { enqueue, dequeue, queuedReply, droppedLine } from '../src/queue.mjs'

const chore = (name, id) => ({ id, name, args: { queue: true } })

test('enqueue: chores keep the order they came in, each with its own id', () => {
  const q1 = enqueue([], { name: 'craft', args: { item: 'stone_hoe', queue: true } }, 7)
  const q2 = enqueue(q1, { name: 'deposit', args: { items: 'wheat', queue: true } }, 8)
  assert.deepEqual(q2.map(c => [c.id, c.name]), [[7, 'craft'], [8, 'deposit']])
})

test('enqueue: queue= itself is not handed to the action (it is not an argument there)', () => {
  const [c] = enqueue([], { name: 'craft', args: { item: 'stone_hoe', queue: true } }, 1)
  assert.deepEqual(c.args, { item: 'stone_hoe' })
})

test('dequeue: the first in is the next out, and the rest keep their order', () => {
  const { next, rest } = dequeue([chore('craft', 1), chore('deposit', 2), chore('eat', 3)])
  assert.deepEqual([next.id, rest.map(c => c.id)], [1, [2, 3]])
})

test('dequeue: an empty queue has no next', () => assert.deepEqual(dequeue([]), { next: null, rest: [] }))

for (const [name, id, after, position, expected] of [
  ['first in line', 4, { id: 3, name: 'routine' }, 1, { ok: true, queued: 4, after: 'routine (task 3)', position: 1, note: 'runs when task 3 ends; its result comes as a task_done event in ./mc wait' }],
  ['third in line', 9, { id: 3, name: 'routine' }, 3, { ok: true, queued: 9, after: 'routine (task 3)', position: 3, note: 'runs when task 3 ends and 2 queued chores are done; its result comes as a task_done event in ./mc wait' }]
]) {
  test(`queuedReply: ${name}`, () => assert.deepEqual(queuedReply(id, after, position), expected))
}

test('droppedLine: ./mc stop names the chores it dropped, or nothing', () => {
  assert.deepEqual([droppedLine([chore('craft', 1), chore('deposit', 2)]), droppedLine([])], ['craft (queued 1), deposit (queued 2)', null])
})
