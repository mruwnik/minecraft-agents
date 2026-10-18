// Why JavaScript: node test over the JS sources; checks each status a primitive returns is classified in engine.primitive.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { readdirSync, readFileSync } from 'node:fs'

const dir = new URL('.', import.meta.url)
const quoted = file => [...readFileSync(new URL(file, import.meta.url), 'utf8').matchAll(/"([a-z-]+)"/g)].map(m => m[1])
const classified = new Set(['../src/engine/primitive.cljs'].flatMap(quoted))
// 'online' is the view pose's liveness, not an act status
const notActs = new Set(['online'])
const literal = /(?:status: |(?:result|refuse|outcome)\()'([a-z-]+)'/g

test('every status the primitives return is named in engine.primitive (a done set, failure-kinds or the cut lines)', () => {
  const statuses = readdirSync(dir)
    .filter(f => f.endsWith('.mjs') && !f.includes('test'))
    .flatMap(f => [...readFileSync(new URL(f, dir), 'utf8').matchAll(literal)].map(m => m[1]))
  const unnamed = [...new Set(statuses)].filter(s => !classified.has(s) && !notActs.has(s)).sort()
  assert.deepEqual(unnamed, [])
})
