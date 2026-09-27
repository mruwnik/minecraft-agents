import { turnsFor, stateOf } from './format.mjs'
import { compileBlueprintStructure } from './compiler.mjs'
export function blueprintOperationalCells (manifest) {
  const ir = compileBlueprintStructure(manifest.source), turns = turnsFor(ir.document.front ?? 'south', manifest.facing)
  return ir.objects.filter(o => o.initialState.open !== undefined).map(o => {
    let [x, y, z] = o.footprint[0].at, width = ir.width, depth = ir.depth
    for (let n = 0; n < turns; n++) { [x, z] = [depth - 1 - z, x]; [width, depth] = [depth, width] }
    return { x: manifest.at.x + x, y: manifest.at.y + y, z: manifest.at.z + z, name: manifest.allocation.assignments[o.id], open: o.initialState.open }
  })
}
export async function establishBlueprintOperations (api, manifest) {
  for (const cell of blueprintOperationalCells(manifest)) {
    const block = api.block(cell.x, cell.y, cell.z)
    if (block?.name !== cell.name) throw new Error(`blueprint operation target changed at ${cell.x},${cell.y},${cell.z}`)
    const open = stateOf(block).open
    if (open === undefined) throw new Error(`blueprint cannot observe open state at ${cell.x},${cell.y},${cell.z}`)
    if ((open === true || open === 'true') !== cell.open) await api.act('toggle', { x: cell.x, y: cell.y, z: cell.z, open: cell.open })
    const after = stateOf(api.block(cell.x, cell.y, cell.z)).open
    if ((after === true || after === 'true') !== cell.open) throw new Error(`blueprint initial open state was not established at ${cell.x},${cell.y},${cell.z}`)
  }
}

export function verifyBlueprintGuarantees (api, manifest) {
  if (!manifest.source.guarantees?.includes('source_water')) return
  const ir = compileBlueprintStructure(manifest.source), turns = turnsFor(ir.document.front ?? 'south', manifest.facing)
  for (const object of ir.objects) {
    const name = manifest.allocation.assignments[object.id]
    if (name !== 'water' && object.states.waterlogged !== 'true') continue
    let [x, y, z] = object.footprint[0].at, width = ir.width, depth = ir.depth
    for (let n = 0; n < turns; n++) { [x, z] = [depth - 1 - z, x]; [width, depth] = [depth, width] }
    const block = api.block(manifest.at.x + x, manifest.at.y + y, manifest.at.z + z), states = stateOf(block)
    if (name === 'water' ? block?.name !== 'water' || Number(states.level) !== 0 : block?.name !== name || ![true, 'true'].includes(states.waterlogged)) throw new Error(`blueprint source-water guarantee failed at ${x},${y},${z}`)
  }
}
