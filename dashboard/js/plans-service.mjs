// The plans endpoints' data: plans of a world (places with a structure) compared with the dumped chunks.
// Summaries are cached per world for ttlMs; the full comparison of one plan is computed on request.
import fs from 'node:fs'
import path from 'node:path'
import { comparePlan, createWorldBlocks, listPlans } from './plans.mjs'
import { parsePlacePlan, planBill } from '../../src/lib/plan.mjs'

const summaryOf = ({ total, match, missing, wrong, unknown, percent }) => ({ total, match, missing, wrong, unknown, percent })

export function createPlansService ({ stateDir, ttlMs = 10000, now = Date.now, onCompare = () => {} }) {
  const blocks = new Map()
  const summaries = new Map()
  const blocksFor = world => {
    if (!blocks.has(world)) blocks.set(world, createWorldBlocks({ stateDir, world }))
    return blocks.get(world)
  }
  const readPlaces = world => {
    try { return JSON.parse(fs.readFileSync(path.join(stateDir, 'worlds', world, 'places.json'), 'utf8')) } catch { return [] }
  }
  const compare = (world, place) => {
    onCompare(place.name)
    return comparePlan({ place, blockAt: blocksFor(world).blockAt })
  }
  const list = world => {
    const cached = summaries.get(world)
    if (cached && now() - cached.at < ttlMs) return cached.value
    const places = readPlaces(world)
    const byName = new Map(places.map(p => [p.name, p]))
    const plans = listPlans(places).map(p => ({ ...p, summary: summaryOf(compare(world, byName.get(p.name))) }))
    const value = { world, plans }
    summaries.set(world, { at: now(), value })
    return value
  }
  const get = (world, name) => {
    const place = readPlaces(world).find(p => p.name === name)
    const listed = place && listPlans([place])[0]
    if (!listed) return null
    return { ...listed, ...compare(world, place), bill: planBill(parsePlacePlan(place)) }
  }
  return { list, get }
}
