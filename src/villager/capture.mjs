// Optional capture geometry and lifecycle; trade selection is deliberately separate.
import { inAnyZone, villagerPenPlan } from '../lib.mjs'
import { villagerObservation } from './observation.mjs'
const distance = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)
const professionOf = raw => villagerObservation({ metadata: raw }).profession
export function chooseVillagerCapture (api, { a, cell, target, nearby, tries }) {
  const penBlock = a.penBlock ?? 'cobblestone'
  const penIssue = pen => {
    const passable = name => !name || ['air', 'short_grass', 'tall_grass', 'grass', 'fern', 'large_fern', 'snow'].includes(name)
    const allCells = [cell, ...pen.floor]
    const badFloor = allCells.find(p => !api.block(p.x, p.y - 1, p.z)?.solid)
    if (badFloor) return `pen needs flat solid ground under ${badFloor.x},${badFloor.y},${badFloor.z}`
    const structure = [...pen.walls, ...pen.guard, ...pen.service]
    const badWall = structure.find(p => { const name = api.block(p.x, p.y, p.z)?.name; return name !== penBlock && !passable(name) })
    if (badWall) return `pen wall at ${badWall.x},${badWall.y},${badWall.z} is occupied by ${api.block(badWall.x, badWall.y, badWall.z)?.name}`
    const standing = [...pen.interior, pen.gate, pen.outside, pen.serviceStand]
    const blocked = standing.flatMap(p => [p, { ...p, y: p.y + 1 }]).find(p => !passable(api.block(p.x, p.y, p.z)?.name) && !(p.x === pen.gate.x && p.z === pen.gate.z && api.block(p.x, p.y, p.z)?.name === penBlock))
    if (blocked) return `pen walking route is blocked at ${blocked.x},${blocked.y},${blocked.z} by ${api.block(blocked.x, blocked.y, blocked.z)?.name}`
    const gateCells = [pen.gate, { ...pen.gate, y: pen.gate.y + 1 }, { ...pen.gate, y: pen.gate.y + 2 }]
    // The top gate block is a permanent lintel. Only the two blocks in the
    // walking opening can trap a villager or prevent a fresh capture.
    if (gateCells.slice(0, 2).some(p => api.block(p.x, p.y, p.z)?.name === penBlock) && !nearby.some(e => pen.interior.some(p => Math.floor(e.pos.x) === p.x && Math.floor(e.pos.y) === p.y && Math.floor(e.pos.z) === p.z) && (a.id === undefined || e.id === a.id))) return 'pen entry is already blocked with no villager inside: open it before luring'
    const needed = [...structure, ...gateCells].filter(p => api.block(p.x, p.y, p.z)?.name !== penBlock).length
    const reservePen = tries > 1 ? 2 : 0
    if ((api.inv()[penBlock] ?? 0) < needed + reservePen) return `pen needs ${needed} ${penBlock} to build and ${reservePen} spare to reseal its service route: carrying ${api.inv()[penBlock] ?? 0}`
    if ([...structure, ...gateCells].some(p => api.zones().some(z => inAnyZone([z], p) && !new RegExp(`^(${api.me().toLowerCase()}|starter)-`).test(z.name.toLowerCase())))) return 'pen wall crosses somebody else\'s protected zone'
    return null
  }
  const directions = [{ x: 1, z: 0 }, { x: -1, z: 0 }, { x: 0, z: 1 }, { x: 0, z: -1 }]
  const orientations = directions.map(d => {
    const plan = villagerPenPlan(cell, { x: cell.x + d.x, z: cell.z + d.z })
    return { plan, score: plan.walls.filter(p => api.block(p.x, p.y, p.z)?.name === penBlock).length, issue: penIssue(plan) }
  })
  const existing = [...orientations].sort((a, b) => b.score - a.score)[0]
  const preferred = [...orientations].sort((a, b) => distance(a.plan.outside, target.pos) - distance(b.plan.outside, target.pos))
  const chosen = existing.score >= 10 ? existing : preferred.find(p => !p.issue) ?? preferred[0]
  const pen = chosen.plan
  if (chosen.issue) throw new Error(chosen.issue)
  return { pen, penBlock }
}

export function makeVillagerCapture (api, { a, cell, pen, penBlock, trackedUuid, freshIds, job, initialId }) {
  let id = initialId
  let serviceOpen = false
  const serviceSills = pen?.service.filter(p => p.y === cell.y) ?? []
  const sealService = async (cleanup = false) => {
    if (!serviceOpen) return
    const act = cleanup && api.cleanupAct ? api.cleanupAct : api.act
    await act('goto', { ...pen.serviceStand, range: 0 })
    for (const p of serviceSills) {
      if (api.block(p.x, p.y, p.z)?.name !== penBlock) await act('place', { ...p, item: penBlock })
    }
    const hole = serviceSills.find(p => api.block(p.x, p.y, p.z)?.name !== penBlock)
    if (hole) throw new Error(`service route still open at ${hole.x},${hole.y},${hole.z}`)
    serviceOpen = false
  }
  return {
    pen, serviceSills,
    get serviceOpen () { return serviceOpen },
    openService () { serviceOpen = true },
    sealService,
    async capture (ensureBlock) {
      {
        const walls = [...pen.walls, ...pen.guard].filter(p => api.block(p.x, p.y, p.z)?.name !== penBlock).map(p => ({ ...p, item: penBlock }))
        if (walls.length) await api.act('place', { blocks: walls })
        for (const p of [...pen.walls, ...pen.guard]) {
          if (api.block(p.x, p.y, p.z)?.name !== penBlock) await api.act('place', { ...p, item: penBlock })
        }
        let hole = [...pen.walls, ...pen.guard].find(p => api.block(p.x, p.y, p.z)?.name !== penBlock)
        if (hole) throw new Error(`pen wall still open at ${hole.x},${hole.y},${hole.z}`)
        await api.act('goto', { ...pen.serviceStand, range: 0 })
        // The high outside lintel is out of reach from the first wall-building stance.
        // Build it from the service side after the lectern roof provides an attachment.
        for (const p of pen.service) {
          if (api.block(p.x, p.y, p.z)?.name !== penBlock) await api.act('place', { ...p, item: penBlock })
        }
        hole = pen.service.find(p => api.block(p.x, p.y, p.z)?.name !== penBlock)
        if (hole) throw new Error(`pen service wall still open at ${hole.x},${hole.y},${hole.z}`)
        api.report({ pen: 'entrance open', penAt: `${pen.center.x},${pen.center.y},${pen.center.z}` })
      }
      await ensureBlock()
      {
        await api.act('goto', { ...pen.outside, range: 1 })
        const inside = async () => {
          const found = (await api.act('entity', { name: 'villager', uuid: true, count: 100 })).found.find(e => {
            if (trackedUuid && e.uuid !== trackedUuid) return false
            const p = e.exact.split(',').map(Number)
            // Wait for the back cell. A villager at the front can still have its
            // hitbox inside the entrance even though its feet floor to "inside".
            if (Math.floor(p[0]) !== pen.center.x || Math.floor(p[1]) !== pen.center.y || Math.floor(p[2]) !== pen.center.z) return false
            if (a.id !== undefined) return e.id === a.id
            const raw = villagerObservation(e).metadata
            return (freshIds.has(e.id) || (trackedUuid && e.uuid === trackedUuid)) && villagerObservation(e).baby !== true && ['unemployed', job].includes(professionOf(raw))
          })
          if (found) id = found.id
          return Boolean(found)
        }
        const top = { ...pen.gate, y: pen.gate.y + 1 }
        const cap = { ...pen.gate, y: pen.gate.y + 2 }
        if (api.block(cap.x, cap.y, cap.z)?.name !== penBlock) await api.act('place', { ...cap, item: penBlock })
        if (api.block(cap.x, cap.y, cap.z)?.name !== penBlock) throw new Error('pen entry roof did not close')
        for (let attempt = 0; attempt < 3; attempt++) {
          await api.until(inside, { timeout: 60, every: 0.25, what: 'the villager did not enter the pen through its open side' })
          try { await api.act('place', { ...top, item: penBlock }) }
          catch (error) {
            if (!/server refused|stand in it/.test(error.message) || attempt === 2) throw error
            await api.pause(0.5)
            continue
          }
          if (await inside()) break
          if (api.block(top.x, top.y, top.z)?.name === penBlock) await api.act('dig', top)
          if (attempt === 2) throw new Error('villager left the pen before the entry could be closed')
        }
        await api.act('place', { ...pen.gate, item: penBlock })
        if ([pen.gate, top, cap].some(p => api.block(p.x, p.y, p.z)?.name !== penBlock)) throw new Error('pen entry did not close')
        const held = (await api.act('entity', { name: 'villager', uuid: true, count: 100 })).found.some(e => {
          if (trackedUuid ? e.uuid !== trackedUuid : e.id !== id) return false
          const p = e.exact.split(',').map(Number)
          return pen.interior.some(c => Math.floor(p[0]) === c.x && Math.floor(p[1]) === c.y && Math.floor(p[2]) === c.z)
        })
        if (!held) throw new Error(`pen entry closed but villager ${id} is not inside`)
        api.report({ pen: 'closed', capturedId: id })
      }
      return id
    }
  }
}
