// Bench adapter for the A* planner in planner.mjs: maps its result onto the statuses the bench compares.
import { plan as search } from './planner.mjs'
import { defaultStateTable } from './blocks.mjs'

const STATUS = { found: 'success', partial: 'partial', none: 'noPath' }

const table = defaultStateTable() // once per process

export function plan (snapshot, query, options = {}) {
  const r = search(snapshot, query, { table, ...options })
  return {
    status: STATUS[r.status],
    reason: r.reason,
    ms: r.ms,
    expanded: r.expanded,
    path: (r.path?.steps ?? []).map(({ x, y, z }) => ({ x, y, z }))
  }
}
