import { arrivalPlan, arrivalPreflight } from '../../src/villager-arrival.mjs'
import { breedPlan, breedPreflight, breedBill, breedCensus, breedKey, breedMaterial } from '../../src/villager-breed.mjs'
import { buildHabitat, habitatOwnership, habitatThreats, habitatMetadata } from '../../src/villager-habitat.mjs'

export default {
  doc: 'villager.prepare target= x= y= z= [size block=cobblestone gate=oak_fence_gate bed=white_bed entryX= entryZ= dockX= dockY= dockZ= riverX=1 riverZ=0 plan=true]: prepare a lit roofed habitat with target beds before transporting villagers; x/y/z is the southwest interior foot cell',
  stops: 'the empty or occupied habitat is prepared and closed, or the whole preflight refuses the site or supplies',
  args: { target: 'number!', x: 'number!', y: 'number!', z: 'number!', size: 'number', block: 'string', gate: 'string', bed: 'string', entryX: 'number', entryZ: 'number', airlock: 'boolean', dockX: 'number', dockY: 'number', dockZ: 'number', riverX: 'number', riverZ: 'number', plan: 'boolean' },
  async run (api, a) {
    const plan = breedPlan(a)
    const material = a.block ?? 'cobblestone'; const gateItem = a.gate ?? 'oak_fence_gate'; const bedItem = a.bed ?? 'white_bed'
    await habitatThreats(api, plan)
    const preflight = breedPreflight(plan, api.block, material, gateItem)
    const arrival = ['dockX', 'dockY', 'dockZ', 'riverX', 'riverZ'].some(k => a[k] !== undefined) ? arrivalPlan(plan, a) : null
    const joinNeeded = arrival ? [...arrivalPreflight(arrival, api.block), ...arrival.opening.filter(p => !api.block(p.x, p.y, p.z)?.solid)] : []
    const join = [...new Map(joinNeeded.map(p => [breedKey(p), { ...p, item: material }])).values()]
    habitatOwnership(api, { ...plan, shell: [...plan.shell, ...(arrival ? [...arrival.opening, arrival.roof, ...arrival.sides] : [])] }, preflight)
    preflight.needed.push(...join)
    const { bill, shortages } = breedBill(preflight, api.inv(), 0, 'bread', bedItem)
    const observed = async () => {
      const found = (await api.act('entity', { name: 'villager', uuid: true, count: 1000 })).found ?? []
      if (found.length >= 1000) throw new Error('villager census reached its observation limit')
      return breedCensus(plan, found)
    }
    let residents = await observed()
    const known = new Set(residents.map(e => e.uuid))
    const chainArgs = { target: a.target, x: a.x, y: a.y, z: a.z, size: plan.width, airlock: plan.airlock, block: material, gate: gateItem, bed: bedItem, ...(plan.entry ? { entryX: plan.entry.x, entryZ: plan.entry.z } : {}) }
    const metadata = { ...habitatMetadata(plan), ...(arrival ? { arrival: { ...arrival.dockArgs, landing: arrival.landing, rear: arrival.rear } } : {}) }
    if (a.plan === true) return { ...metadata, plan: true, bill, shortages, clears: preflight.clear.map(breedKey), population: residents.length, ready: !shortages.length }
    if (shortages.length) throw new Error(`habitat supplies short: ${shortages.join('; ')}`)
    const refresh = async () => {
      await habitatThreats(api, plan)
      residents = await observed()
      for (const uuid of known) if (!residents.some(e => e.uuid === uuid)) throw new Error(`known villager ${uuid} left the habitat while preparing`)
      for (const e of residents) known.add(e.uuid)
      api.report({ population: residents.length, prepared: false })
    }
    await buildHabitat(api, plan, preflight, { material, gateItem, bedItem }, refresh)
    if (arrival && arrivalPreflight(arrival, api.block).length) throw new Error('arrival boundary placement not confirmed')
    return { ...metadata, prepared: true, secure: true, population: residents.length, uuids: residents.map(e => e.uuid), breedArgs: chainArgs, ...(arrival ? { receiveArgs: { ...chainArgs, materials: [...new Set([material, ...arrival.dock.walls.map(p => api.block(p.x, p.y, p.z)?.name).filter(breedMaterial)])].sort().join(','), dockX: a.dockX, dockY: a.dockY, dockZ: a.dockZ, riverX: 1, riverZ: 0 } } : {}) }
  }
}
