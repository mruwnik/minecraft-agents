// planner.mjs's exports, each answer checked against the ClojureScript port (dev and :advanced builds) before it is returned.
// With COMPARE_LOG set, every comparison appends a line "<status> <move codes>" to that file (the gate reads it).
import fs from 'node:fs'
import assert from 'node:assert/strict'
import * as real from '../js/path/planner.mjs'
import { loadVersions, view } from './versions.mjs'

export const { MOVE, DEFAULT_COSTS } = real
const [, ...ports] = loadVersions()

const note = result => {
  if (!process.env.COMPARE_LOG) return
  const moves = [...new Set((result.path?.steps ?? []).map(s => s.move))].join(',')
  fs.appendFileSync(process.env.COMPARE_LOG, `${result.status}/${result.reason} ${moves}\n`)
}

const agree = (expected, got, port) => assert.deepStrictEqual(port.view(got), view(expected), `${port.name} differs from the JS planner`)

export function plan (snapshot, query, options) {
  const expected = real.plan(snapshot, query, options)
  ports.forEach(port => agree(expected, port.plan(snapshot, query, options), port))
  note(expected)
  return expected
}

export function createSearch (snapshot, query, options) {
  const search = real.createSearch(snapshot, query, options)
  const others = ports.map(port => ({ port, search: port.createSearch(snapshot, query, options) }))
  return {
    step: max => {
      const done = search.step(max)
      others.forEach(({ search: other }) => assert.equal(other.step(max), done))
      return done
    },
    result: () => {
      const expected = search.result()
      others.forEach(({ port, search: other }) => agree(expected, other.result(), port))
      note(expected)
      return expected
    }
  }
}
