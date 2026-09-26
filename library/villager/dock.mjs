import { inAnyZone, villagerBoatStatus, villagerDockPlan, villagerUuid, workRefusal } from '../../src/lib.mjs'

const key = p => `${p.x},${p.y},${p.z}`
const pos = text => {
  const [x, y, z] = text.split(',').map(Number)
  return { x, y, z }
}
const wet = name => name === 'water' || name === 'bubble_column'
const empty = name => ['air', 'cave_air', 'void_air', 'short_grass', 'tall_grass', 'seagrass', 'tall_seagrass', 'torch', 'wall_torch'].includes(name)
const waterOrEmpty = name => wet(name) || empty(name)
const inside = (plan, p) => Math.abs(p.x - (plan.cell.x + 0.5)) < 1.5 && Math.abs(p.z - (plan.cell.z + 0.5)) < 1.5
const boatFits = (plan, p) => Math.abs(p.x - (plan.cell.x + 0.5)) <= 0.8 && Math.abs(p.z - (plan.cell.z + 0.5)) <= 0.8
const fullBlock = name => /^(cobblestone|cobbled_deepslate|dirt|stone|(?:oak|spruce|birch|jungle|acacia|dark_oak|mangrove|cherry|bamboo|crimson|warped)_planks)$/.test(name)

export default {
  doc: 'villager.dock x= y= z= riverX= riverZ= [block=cobblestone | materials=cobbled_deepslate,cherry_planks] [prepare=true | uuid= boat=] [insideX= insideY= insideZ=] [deferCapX= deferCapZ=]: prepare a safe dock, or close its entire portal before releasing the exact passenger; a dry interior stance keeps the operator inside; only a freshly verified adult may use a single deferred upper rear cap under a full roof over a two-high solid barrier, reported as adult-only containment',
  stops: 'the landing is prepared with its boat gate open, or the exact villager is on foot inside the closed landing',
  args: { uuid: 'string', boat: 'number', prepare: 'boolean', x: 'number!', y: 'number!', z: 'number!', riverX: 'number!', riverZ: 'number!', block: 'string', materials: 'string', insideX: 'number', insideY: 'number', insideZ: 'number', deferCapX: 'number', deferCapZ: 'number' },

  async run (api, a) {
    const preparing = a.prepare === true
    if (!preparing && !villagerUuid(a.uuid)) throw new Error('uuid= must be the observed villager UUID')
    if (!preparing && !Number.isInteger(a.boat)) throw new Error('boat= must be the observed boat ID')
    if (![a.x, a.y, a.z, a.riverX, a.riverZ].every(Number.isInteger)) throw new Error('dock coordinates must be integers')
    if (preparing && (a.uuid !== undefined || a.boat !== undefined)) throw new Error('prepare=true needs an empty dock site, without uuid= or boat=')
    const cell = { x: a.x, y: a.y, z: a.z }
    const plan = villagerDockPlan(cell, { x: a.riverX, z: a.riverZ })
    const insideGiven = [a.insideX, a.insideY, a.insideZ].some(v => v !== undefined)
    const insideStand = insideGiven ? { x: a.insideX, y: a.insideY, z: a.insideZ } : null
    const deferGiven = a.deferCapX !== undefined || a.deferCapZ !== undefined
    const deferredCap = deferGiven ? { x: a.deferCapX, y: cell.y + 3, z: a.deferCapZ } : null
    if (deferredCap && (!insideStand || !Number.isInteger(deferredCap.x) || !Number.isInteger(deferredCap.z) || (deferredCap.x - cell.x) * a.riverX + (deferredCap.z - cell.z) * a.riverZ !== -2 || Math.abs((deferredCap.x - cell.x) * -a.riverZ + (deferredCap.z - cell.z) * a.riverX) > 1 || !fullBlock(api.block(deferredCap.x, deferredCap.y + 1, deferredCap.z)?.name))) throw new Error('deferred cap requires an interior release stance and a full roof over one upper rear wall cell')
    const capGap = p => deferredCap && key(p) === key(deferredCap) && !api.block(p.x, p.y, p.z)?.solid
    if (insideGiven) {
      if (preparing || !Object.values(insideStand).every(Number.isInteger) || !plan.interior.some(p => p.x === insideStand.x && p.z === insideStand.z) || insideStand.y < cell.y || insideStand.y > cell.y + 1) throw new Error('insideX/Y/Z must name a dry interior release stance at dock foot height or one cell above')
      const floor = api.block(insideStand.x, insideStand.y - 1, insideStand.z)
      if (!floor?.solid || /(?:_slab|_stairs|_fence|_wall|_bed)$|^(?:magma_block|cactus|campfire|soul_campfire)$/.test(floor.name) || !empty(api.block(insideStand.x, insideStand.y, insideStand.z)?.name) || !empty(api.block(insideStand.x, insideStand.y + 1, insideStand.z)?.name)) throw new Error('interior release stance needs safe solid support and two dry clear body cells')
    }
    if (a.materials !== undefined && a.block !== undefined) throw new Error('give either materials= or block=, not both')
    const palette = [...new Set(String(a.materials ?? a.block ?? 'cobblestone').split(',').map(s => s.trim()))]
    if (!palette.length || palette.some(name => !fullBlock(name))) throw new Error('dock materials must be full solid blocks such as cobbled_deepslate,cherry_planks,oak_planks')
    const built = name => palette.includes(name)
    const state = () => api.act('boat_state', { id: a.boat })
    const checked = async () => {
      const s = await state()
      const issue = villagerBoatStatus(s, a.boat, a.uuid)
      if (issue) throw new Error(issue)
      const boat = s.boats.find(b => b.id === a.boat)
      if (boat.passengers.length !== 1) throw new Error(`boat ${a.boat} must carry only villager ${a.uuid}`)
      if (s.mounted === a.boat) throw new Error(`dismount boat ${a.boat} before building its dock`)
      return { s, boat }
    }
    let s = null; let boat = null
    if (!preparing) {
      ;({ s, boat } = await checked())
      if (!boatFits(plan, pos(boat.exact))) throw new Error(`boat ${a.boat} at ${boat.exact} is too close to the dock wall centered on ${key(cell)}; bring it within 0.8 block of center before building`)
      const center = pos(boat.exact)
      if (center.y < cell.y - 1.5 || center.y > cell.y + 1.5) throw new Error(`boat ${a.boat} is at the wrong height for dock ${key(cell)}`)
      if (boat.leashHolderId !== null && boat.leashHolderId !== s.selfId) throw new Error(`boat ${a.boat} is leashed to somebody else`)
    }

    // Preserve natural shore ground as the lower wall layer. In the shallows,
    // replace only one water layer over solid riverbed before building upward.
    const deepFloor = []
    const foundation = []
    const needed = []
    for (const base of plan.ring) {
      const at = api.block(base.x, base.y, base.z)
      if (plan.gateBase.some(g => key(g) === key(base)) && at?.solid && !built(at.name)) throw new Error(`dock gate base ${key(base)} is blocked by ${at.name}`)
      if (!built(at?.name) && !at?.solid) {
        if (!waterOrEmpty(at?.name)) throw new Error(`dock edge ${key(base)} contains ${at?.name ?? 'unloaded'}`)
        const below = { ...base, y: base.y - 1 }
        const under = api.block(below.x, below.y, below.z)
        if (wet(under?.name)) {
          const bed = { ...below, y: below.y - 1 }
          const bedBlock = api.block(bed.x, bed.y, bed.z)
          if (wet(bedBlock?.name)) {
            if (!api.block(bed.x, bed.y - 1, bed.z)?.solid) throw new Error(`dock water is deeper than two blocks at ${key(bed)}`)
            // Both gate columns still need a solid bed: otherwise an adult
            // could swim through two clear cells under a closed y-level gate.
            deepFloor.push(bed)
          } else if (!bedBlock?.solid) throw new Error(`dock needs a solid riverbed under ${key(below)}`)
          // One gate base has a temporary underwater sill: with its y-level
          // service cell open, the adult gets only one clear vertical block.
          // The second base stays water for the return boat channel.
          if (!plan.gateBase.some(g => key(g) === key(base)) || (!preparing && key(base) === key(plan.service))) foundation.push(below)
        } else if (!under?.solid) throw new Error(`dock edge at ${key(base)} has no solid support`)
        if (!plan.gate.some(g => key(g) === key(base))) needed.push(base)
      }
      for (let n = 1; n <= 3; n++) {
        const p = { ...base, y: base.y + n }
        const existing = api.block(p.x, p.y, p.z)
        if (built(existing?.name)) continue
        if (!plan.gate.some(g => key(g) === key(p)) && key(p) !== key(plan.service)) {
          if (!waterOrEmpty(existing?.name)) throw new Error(`dock wall at ${key(p)} is blocked by ${existing?.name ?? 'unloaded'}`)
          if (!capGap(p)) needed.push(p)
        } else if (!waterOrEmpty(existing?.name)) throw new Error(`dock opening at ${key(p)} is blocked by ${existing?.name ?? 'unloaded'}`)
      }
    }
    const gateNeeded = preparing ? [] : plan.gate.filter(p => (insideStand || key(p) !== key(plan.service)) && !built(api.block(p.x, p.y, p.z)?.name))
    // Even a previously sealed slot is reopened for interaction, then needs
    // one carried replacement; the dug block may drift away in river water.
    const serviceNeeded = preparing || insideStand ? [] : [plan.service]
    for (const p of plan.interior) {
      const feet = api.block(p.x, p.y, p.z)
      const below = api.block(p.x, p.y - 1, p.z)
      if (feet?.solid) {
        if (!waterOrEmpty(api.block(p.x, p.y + 1, p.z)?.name)) throw new Error(`dock interior at ${key(p)} is obstructed`)
      } else if (below?.solid) {
        if (!empty(feet?.name) || !empty(api.block(p.x, p.y + 1, p.z)?.name)) throw new Error(`dock dry interior at ${key(p)} needs two clear body cells`)
      } else if (!wet(below?.name) || !api.block(p.x, p.y - 2, p.z)?.solid) {
        if (!wet(below?.name)) throw new Error(`dock interior at ${key(p)} needs dry ground or water`)
        const bed = { ...p, y: p.y - 2 }
        if (!wet(api.block(bed.x, bed.y, bed.z)?.name) || !api.block(bed.x, bed.y - 1, bed.z)?.solid) throw new Error(`dock water is deeper than two blocks at ${key(bed)}`)
        deepFloor.push(bed)
      }
    }
    if (!plan.interior.some(p => {
      const feet = api.block(p.x, p.y, p.z)
      const below = api.block(p.x, p.y - 1, p.z)
      return (feet?.solid && empty(api.block(p.x, p.y + 1, p.z)?.name)) || (below?.solid && empty(feet?.name) && empty(api.block(p.x, p.y + 1, p.z)?.name))
    })) throw new Error('dock needs at least one dry interior landing cell')
    const stand = plan.serviceStand
    if (!wet(api.block(stand.x, stand.y, stand.z)?.name) || !empty(api.block(stand.x, stand.y + 1, stand.z)?.name)) throw new Error(`dock service stand at ${key(stand)} needs swimmable water with head clearance`)
    const standFloor = { ...stand, y: stand.y - 1 }
    const standBelow = api.block(standFloor.x, standFloor.y, standFloor.z)
    if (wet(standBelow?.name)) {
      if (!api.block(standFloor.x, standFloor.y - 1, standFloor.z)?.solid) throw new Error(`dock service stand is deeper than two blocks at ${key(standFloor)}`)
      deepFloor.push(standFloor)
    } else if (!standBelow?.solid) throw new Error(`dock service stand needs a solid floor at ${key(standFloor)}`)
    const reopen = preparing ? [...plan.gate, ...plan.gateBase.map(p => ({ ...p, y: p.y - 1 }))].filter(p => built(api.block(p.x, p.y, p.z)?.name)) : []
    if (preparing) {
      const obstructedGate = [...plan.gate, ...plan.gateBase.map(p => ({ ...p, y: p.y - 1 }))].find(p => !waterOrEmpty(api.block(p.x, p.y, p.z)?.name) && !built(api.block(p.x, p.y, p.z)?.name))
      if (obstructedGate) throw new Error(`dock preparation needs a clear two-wide boat gate at ${key(obstructedGate)}`)
    }
    const changes = [...deepFloor, ...foundation, ...needed, ...gateNeeded, ...serviceNeeded]
    const foreign = api.zones().find(z => [...changes, ...reopen, ...(preparing ? [] : [plan.service])].some(p => inAnyZone([z], p)) && !new RegExp(`^(${api.me().toLowerCase()}|starter)-`).test(z.name.toLowerCase()))
    if (foreign) throw new Error(`dock crosses protected zone ${foreign.name}`)
    const owner = api.places().filter(p => Math.hypot(p.x - cell.x, p.z - cell.z) <= (p.radius ?? 8) + 3).map(p => workRefusal(p, api.me())).find(Boolean)
    if (owner) throw new Error(owner)
    const remaining = Object.fromEntries(palette.map(name => [name, api.inv()[name] ?? 0]))
    const allocation = new Map()
    const bill = {}
    for (const p of changes) {
      const item = palette.find(name => remaining[name] > 0)
      if (!item) throw new Error(`dock needs ${changes.length} blocks from ${palette.join(',')}; carrying ${palette.reduce((n, name) => n + (api.inv()[name] ?? 0), 0)}`)
      remaining[item]--
      bill[item] = (bill[item] ?? 0) + 1
      allocation.set(key(p), item)
    }
    const residents = (await api.act('entity', { name: 'villager', count: 100, uuid: true })).found
    const others = residents.filter(e => (preparing || e.uuid !== a.uuid) && inside(plan, pos(e.exact)))
    if (others.length) throw new Error(`dock interior already contains another villager ${others[0].uuid ?? others[0].id}`)
    const adultProof = rows => rows.some(e => e.uuid === a.uuid && e.baby === false && e.adult === true)
    if (deferredCap && capGap(deferredCap) && !adultProof(residents)) throw new Error('one-high rear cap gap requires the exact passenger freshly observed as an adult')
    if (preparing) {
      const boats = (await api.act('boat_state', {})).boats ?? []
      const occupied = boats.find(b => inside(plan, pos(b.exact)))
      if (occupied) throw new Error(`dock interior already contains boat ${occupied.id}`)
    }

    const ensure = async p => {
      if (!built(api.block(p.x, p.y, p.z)?.name)) await api.act('place', { ...p, item: allocation.get(key(p)) })
      if (!built(api.block(p.x, p.y, p.z)?.name)) throw new Error(`dock block did not take at ${key(p)}`)
    }
    api.report({ dock: preparing ? 'preparing' : 'building', ...(preparing ? {} : { boat: a.boat, villagerUuid: a.uuid }), need: changes.length, bill })
    try {
      if (insideStand) await api.act('goto', { ...insideStand, range: 0, into: true })
      for (const p of deepFloor) await ensure(p)
      for (const p of foundation) await ensure(p)
      // A temporary one-cell rear pedestrian exit has two missing wall
      // blocks above its intact floor. Repair the supported upper block
      // first; repairing its lower block first closes the passage and can
      // strand the bot beside the occupied boat before it reaches up again.
      const upperFirst = preparing ? [] : needed.filter(p => p.y === cell.y + 2 &&
        needed.some(q => q.x === p.x && q.z === p.z && q.y === p.y - 1) &&
        api.block(p.x, p.y - 2, p.z)?.solid &&
        [[1, 0], [-1, 0], [0, 1], [0, -1]].some(([dx, dz]) => api.block(p.x + dx, p.y, p.z + dz)?.solid))
      const upperKeys = new Set(upperFirst.map(key))
      const wallOrder = insideStand ? [...needed].sort((a, b) => a.y - b.y) : [...upperFirst, ...needed.filter(p => !upperKeys.has(key(p)))]
      for (const p of wallOrder) await ensure(p)
      if (preparing) {
        const gap = plan.walls.find(p => !api.block(p.x, p.y, p.z)?.solid)
        if (gap) throw new Error(`prepared dock wall remains open at ${key(gap)}`)
        for (const p of reopen.slice().sort((a, b) => b.y - a.y)) {
          const residents = (await api.act('entity', { name: 'villager', uuid: true, count: 100 })).found ?? []
          if (residents.some(e => inside(plan, pos(e.exact)))) throw new Error('dock acquired a resident before opening; keep the boat portal closed')
          if (!built(api.block(p.x, p.y, p.z)?.name)) throw new Error(`planned dock portal changed at ${key(p)}`)
          await api.act('dig', p)
          if (!waterOrEmpty(api.block(p.x, p.y, p.z)?.name)) throw new Error(`dock portal did not clear at ${key(p)}`)
        }
        api.report({ dock: 'prepared', gateOpen: true })
        return { dock: key(cell), prepared: true, gateOpen: true, need: changes.length, bill }
      }
      // Leave the river gate walkable until the bot is outside. One lower
      // gate cell stays open over its temporary solid sill for interaction.
      await api.act('goto', { ...(insideStand ?? plan.serviceStand), range: 0, ...(insideStand ? { into: true } : {}) })
      ;({ s, boat } = await checked())
      if (!boatFits(plan, pos(boat.exact))) throw new Error(`boat ${a.boat} shifted against the dock wall before its gate was closed`)
      for (const p of [...gateNeeded].sort((a, b) => insideStand ? a.y - b.y : b.y - a.y)) await ensure(p)
      const gap = [...plan.walls.filter(p => !capGap(p)), ...plan.gate.filter(p => insideStand || key(p) !== key(plan.service)), plan.serviceFoundation].find(p => !api.block(p.x, p.y, p.z)?.solid)
      if (gap) throw new Error(`dock wall or gate remains open at ${key(gap)}`)
      if (deferredCap && capGap(deferredCap)) {
        if (![deferredCap.y - 1, deferredCap.y - 2, deferredCap.y + 1].every(y => fullBlock(api.block(deferredCap.x, y, deferredCap.z)?.name)) || !['air', 'cave_air', 'void_air'].includes(api.block(deferredCap.x, deferredCap.y, deferredCap.z)?.name)) throw new Error('deferred rear cap lacks its two-high solid adult barrier or full roof')
        const fresh = (await api.act('entity', { name: 'villager', uuid: true, count: 100 })).found
        if (!adultProof(fresh)) throw new Error('exact adult passenger proof changed before release through the contained rear cap gap')
      }
      if (!insideStand) {
        if (api.block(plan.service.x, plan.service.y, plan.service.z)?.solid) await api.act('dig', plan.service)
        if (api.block(plan.service.x, plan.service.y, plan.service.z)?.solid) throw new Error(`dock service slot did not open at ${key(plan.service)}`)
      }
      api.report({ dock: 'closed', boat: a.boat })
      if (boat.leashHolderId === s.selfId) await api.act('boat_unleash', { id: a.boat })
      const released = await api.act('boat_release', { id: a.boat, passengerUuid: a.uuid })
      const onFoot = released.onFoot
      if (released.released !== a.uuid || onFoot?.uuid !== a.uuid || onFoot.vehicleId !== null) throw new Error(`boat release did not verify villager ${a.uuid} on foot`)
      let observed = null
      const observe = async () => {
        observed = (await api.act('entity', { name: 'villager', count: 100, uuid: true })).found.find(e => e.uuid === a.uuid)
        return observed?.vehicleId === null && inside(plan, pos(observed.exact))
      }
      if (!(await observe())) await api.until(observe, { timeout: 5, every: 0.25, what: `villager ${a.uuid} was not observed on foot inside the closed dock` })
      await ensure(plan.service)
      if (!api.block(plan.service.x, plan.service.y, plan.service.z)?.solid) throw new Error(`dock service slot remains open at ${key(plan.service)}`)
      api.report({ dock: 'secured', villagerUuid: a.uuid })
      return { dock: key(cell), villagerUuid: a.uuid, onFoot: observed.exact, gateClosed: true, secure: true, ...(deferredCap && capGap(deferredCap) ? { adultOnly: true, fullySealed: false, deferredCap: key(deferredCap) } : {}) }
    } catch (error) {
      if (preparing) {
        const missingWall = plan.walls.filter(p => !api.block(p.x, p.y, p.z)?.solid).map(key)
        api.report({ dock: 'preparation pending', wallPending: missingWall.join(' '), error: error.message })
        if (missingWall.length) throw new Error(`${error.message}; dock preparation pending at ${missingWall.join(' ')}`)
        throw error
      }
      // A one-high service slot is safe, but seal it as soon as a release or
      // observation fails if the rest of the wall and gate already stand.
      const enclosed = [...plan.walls, ...plan.gate.filter(p => key(p) !== key(plan.service)), plan.serviceFoundation].every(p => api.block(p.x, p.y, p.z)?.solid)
      if (enclosed && !api.block(plan.service.x, plan.service.y, plan.service.z)?.solid) {
        try { await ensure(plan.service) } catch (_) {}
      }
      const open = [...plan.walls, ...plan.gate].filter(p => !api.block(p.x, p.y, p.z)?.solid).map(key)
      api.report({ dock: 'pending', boat: a.boat, villagerUuid: a.uuid, open: open.join(' '), service: key(plan.service), error: error.message })
      if (open.length) throw new Error(`${error.message}; dock closure pending at ${open.join(' ')}`)
      throw error
    }
  }
}
