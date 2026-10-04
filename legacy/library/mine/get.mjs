// Get that many of a block: find them, dig them one at a time (dig picks up its own drop), and leave the place tidy.
// What it will not touch: anything inside a protected zone (someone's build, mine included), anything in or beside
// water (bodies drown fetching sand off a lake bed), and the ground it stands on when animals are penned around it.
import { mineTargets, inAnyZone, holesLeft, penShaftRefusal, isTreeLog, feetCell, canPlaceFromHere } from '../../src/lib.mjs'
import { cellOf } from '../../src/farm/field.mjs'
import { dryStandable } from '../../src/navigation/walk.mjs'
import { farmApi, recoverFarm } from '../../src/farm/attention.mjs'

const MAX = 48
const ROUNDS = 6
const SCAN_EXTRA = 256
const AROUND = [-2, -1, 0, 1, 2]

export default {
  doc: 'mine.get block= count= [maxDistance=48] [wet=] [rounds=6]: find, dig and pick up that many of a block, leaving protected builds and anything wet alone',
  stops: 'count= reached, nothing left it may take, six rounds, or a full inventory',
  args: { block: 'string!', count: 'number', maxDistance: 'number', wet: 'boolean', rounds: 'number', force: 'boolean' },

  async run (api, a) {
    api = farmApi(api)
    const want = a.count ?? 1
    const maxDistance = a.maxDistance ?? MAX
    const what = `${a.block} within ${maxDistance} blocks`
    // dig picks its own drop up, and a pickup with nowhere to go leaves the block lying in the pit
    if (!api.freeSlots()) throw new Error('inventory full: mine.get needs one free slot. Drop or deposit something first')
    const pen = api.pen()
    const shaft = penShaftRefusal(pen?.enclosed ? pen.census?.inside : undefined, a.force)
    if (shaft) throw new Error(shaft)

    // the ground around the start, to mend the shaft mouth afterwards: a pit at my own front door is nobody's idea of tidy
    const start = api.pos()
    const startFeet = feetCell(start, true)
    const groundCells = AROUND.flatMap(dx => AROUND.flatMap(dz => [-2, -1].map(dy => ({ x: startFeet.x + dx, y: startFeet.y + dy, z: startFeet.z + dz }))))
    const ground = () => Object.fromEntries(groundCells
      .map(c => [`${c.x},${c.y},${c.z}`, api.block(c.x, c.y, c.z)?.name])
      .filter(([, name]) => name !== a.block))
    const groundBefore = ground()

    const nameAt = p => api.block(p.x, p.y, p.z)?.name
    // water beside it, or anywhere in the 3 blocks above: sand under a lake bed is as wet as the bed itself
    const wet = p => [[0, 1, 0], [0, 2, 0], [0, 3, 0], [1, 0, 0], [-1, 0, 0], [0, 0, 1], [0, 0, -1]]
      .some(([dx, dy, dz]) => nameAt({ x: p.x + dx, y: p.y + dy, z: p.z + dz }) === 'water')

    let got = 0
    let rounds = 0
    let skippedWet = 0
    let skippedPlaced = 0
    // logs come off trees only: a post or a beam in somebody's house is not a tree (a *_log run cut a cherry village post)
    const notTree = /_log$/.test(a.block) ? p => !isTreeLog(p, nameAt) : undefined
    let gaveUp = null
    while (got < want && rounds < (a.rounds ?? ROUNDS)) {
      rounds++
      // ask for plenty extra: find_blocks answers only the nearest count=, and near home those are all builds (the 21 nearest
      // logs were the base and the hut, with trees 40 blocks off), or wet, or out of reach
      const { positions = [] } = await api.act('find_blocks', { block: a.block, maxDistance, count: want - got + SCAN_EXTRA })
      if (!positions.length && !rounds - 1) throw new Error(`no ${what}`)
      const { zones = [] } = await api.act('zones')
      const choice = mineTargets({ nearby: positions, wanted: want - got, inZone: p => inAnyZone(zones, p), wet, allowWet: a.wet === true, what, placed: notTree })
      if (choice.error) { gaveUp = choice.error; break }
      skippedWet = Math.max(skippedWet, choice.skippedWet ?? 0)
      skippedPlaced = Math.max(skippedPlaced, choice.skippedPlaced ?? 0)
      let dug = 0
      for (const p of choice.found) {
        const failed = await api.act('dig', { x: p.x, y: p.y, z: p.z, dig: true, ...(a.wet === true ? { wet: true } : {}) }).then(() => null, recoverFarm(e => e.message))
        gaveUp = failed ? gaveUp ?? failed : gaveUp
        if (!failed && nameAt(p) !== a.block) { got++; dug++ }
        api.report({ got })
        await api.checkpoint()
      }
      // a whole round that moved nothing: looking again will find the same blocks and fail the same way
      if (!dug) break
      gaveUp = got >= want ? null : gaveUp
    }

    const summary = {
      got,
      rounds,
      gaveUp: got >= want ? undefined : gaveUp ?? `nothing more within ${maxDistance} blocks`,
      skippedWet: skippedWet ? `${skippedWet} lay in or by water and were left alone (wet=true takes them; safer: dig x= y= z= from the shore)` : undefined,
      skippedPlaced: skippedPlaced ? `${skippedPlaced} were placed wood (posts and beams in somebody's build), not trees, and were left alone` : undefined
    }

    // A path preview saying success is not a walk. Reach a real surface cell
    // before closing anything: range=2 used to accept the shaft two blocks below
    // home, and the subsequent batch could wall the miner in while saying mended.
    const cellAt = (x, y, z) => cellOf(api.block(x, y, z))
    const sameCell = (a, b) => a.x === b.x && a.y === b.y && a.z === b.z
    const candidates = Array.from({ length: 7 }, (_, i) => i - 3).flatMap(dx => Array.from({ length: 7 }, (_, i) => i - 3)
      .map(dz => ({ x: startFeet.x + dx, y: startFeet.y, z: startFeet.z + dz })))
      .filter(p => dryStandable(cellAt, p))
      .sort((a, b) => Math.hypot(a.x - startFeet.x, a.z - startFeet.z) - Math.hypot(b.x - startFeet.x, b.z - startFeet.z))
    const surface = candidates[0]
    const safelyThere = () => surface && sameCell(feetCell(api.pos(), true), surface) && dryStandable(cellAt, surface)
    if (!surface) return { ...summary, pit: 'no loaded safe surface cell near the start; no ground was filled: inspect a safe exit or climb first' }
    if (!safelyThere()) {
      const home = { ...surface, range: 0 }
      const walked = await api.act('goto', home).then(safelyThere, recoverFarm(() => false))
      const escaped = walked || await api.act('goto', { ...home, dig: true }).then(safelyThere, recoverFarm(() => false))
      if (!escaped) return { ...summary, pit: 'you are in the pit you dug and could not get back out, even digging: pillar_up, or climb; no ground was filled' }
      summary.climbedOut = walked ? 'verified on safe ground near the start' : 'verified on safe ground near the start (dug my way up)'
    }

    // Never give placement a batch that can walk back down for a distant hole.
    // Only restore below-foot cells already within the primitive's no-walk reach.
    // The fallback item identifies remaining holes even with no filler carried.
    const holes = holesLeft(groundBefore, ground(), [...Object.keys(api.inv()), 'dirt'], api.solid).sort((p, q) => p.y - q.y)
    if (!holes.length) return summary
    let placed = 0
    for (const hole of holes) {
      if (!safelyThere()) break
      if (hole.y >= surface.y || !canPlaceFromHere(api.pos(), hole) || !(api.inv()[hole.item] > 0)) continue
      await api.act('place', { item: hole.item, x: hole.x, y: hole.y, z: hole.z }).catch(recoverFarm(() => {}))
      if (api.block(hole.x, hole.y, hole.z)?.solid) placed++
      api.report({ ...summary, mended: `${placed} of ${holes.length} blocks of the ground at the start put back` })
    }
    const left = holes.filter(hole => !api.block(hole.x, hole.y, hole.z)?.solid)
    return {
      ...summary,
      mended: `${placed} of ${holes.length} blocks of the ground you broke open at the start put back`,
      ...(!safelyThere() ? { pit: 'left the verified surface position during repairs; further filling stopped: inspect the exit before continuing' } : {}),
      ...(left.length ? { cleanup_left: `${left.length} ground cells remain open (first ${left[0].x},${left[0].y},${left[0].z}): repair from safe surface footing with filler; mining cleanup never walks back into the excavation` } : {})
    }
  }
}
