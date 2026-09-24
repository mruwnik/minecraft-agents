// The map of a place: an ASCII plan of a farm, saved on the shared map under its name. The plan is the truth of what
// SHOULD be there; the world is the truth of what is, and farm.maintain closes the gap between them.
// Legend: crops w c p b s m k B, ~ water, . path, # fence, G gate, T torch, C chest, K composter, F flower, t sapling,
// A crafting table, space = outside the plan. Rows run south (z), columns east (x) from x,y,z, the NORTH-WEST corner at GROUND level: the
// farmland, floor or path itself, with crops, fences and chests standing at y+1 and a water source at y. A ~ cell is built
// COVERED: a bottom oak slab is laid into the source cell, waterlogged, so it still hydrates its four neighbours and is walkable.
// It only reads the map and writes it back, so it never takes the body over.
import { parsePlan, planAnchor, planCells, planErrors, planLane, planBill, planSummary, compact } from '../../src/lib.mjs'

export default {
  doc: "farm.plan [name=] [map=] [check=true] [kind=] [x= y= z=] [note=]: check a plan and save it on the shared map, or print the one saved under that name. check=true runs every check and saves nothing, so a map can be argued with before every agent sees it",
  stops: 'nothing: it reads the map and writes it back, without moving',
  instant: true,
  args: { name: 'string', map: 'string', kind: 'string', note: 'string', x: 'number', y: 'number', z: 'number', check: 'boolean' },

  async run (api, a) {
    const saved = a.name ? api.places().find(p => p.name === a.name) : null
    if (a.map === undefined) {
      if (!a.name) throw new Error("nothing to read and nothing to check: pass name= to print a saved plan, or map= with check=true to check a map")
      if (!saved?.plan) throw new Error(`no plan called ${a.name}: save one with ./mc farm.plan name=${a.name} kind=farm x= y= z= map='...'`)
      const known = parsePlan(saved.plan)
      return { text: `${saved.name} ${saved.kind} @${saved.x},${saved.y},${saved.z}\n${saved.plan}\n${planSummary(known)} needs ${compact(planBill(known))}` }
    }
    // a map without a name has nowhere to be saved, and guessing a name would put it on the shared map under a word
    // nobody chose. Checking one is the other thing the caller might have meant, so the refusal offers it (#137)
    if (!a.name && !a.check) throw new Error('a plan is saved under a name: pass name=, or check=true to check this map without writing it to the map every agent reads')
    const parsed = parsePlan(a.map)
    const errors = planErrors(parsed)
    if (errors.length) throw new Error(errors.join('; '))
    const at = a.x === undefined ? (saved ?? api.pos()) : a
    const where = { x: Math.floor(at.x), y: Math.floor(at.y), z: Math.floor(at.z) }
    if (!a.check) {
      await api.act('mark', {
        name: a.name,
        kind: a.kind ?? saved?.kind ?? 'farm',
        note: String(a.note ?? saved?.note ?? planSummary(parsed)).slice(0, 80),
        map: parsed.rows.join('\n'),
        ...where
      })
    }
    // y is the GROUND level: a plan saved at the level you stand on has its whole build laid one block too high, so the
    // world is asked here, while the person who wrote the map is still listening
    const { note } = planAnchor(planCells({ ...where, plan: parsed.rows.join('\n') }), api.block)
    // and whether there is anything to walk on between the gate and the rows. A field with none cannot be worked at
    // all - every `goto` into it answers "no walkable path", because a walk steps AROUND planted cells rather than
    // trample them - but the shape is not a contradiction, so it is saved with the fault named rather than refused
    // (Chani took her own carrot patch's missing lane for a tool bug, BUGS.md 09-23)
    const warn = [planLane(parsed.cells).noLane, note].filter(Boolean).join('; ')
    // a check answers everything a save answers, in the same words: an answer that differed from the real one would be
    // worth less than no check at all. It only says `checked` where a save says `saved`, and how to save it
    const said = { at: where, is: planSummary(parsed), needs: planBill(parsed), ...(warn ? { warn } : {}) }
    if (!a.check) return { saved: a.name, ...said }
    return { checked: true, ...said, save: `nothing was written: send the same map with name=${a.name ?? '<a name>'} and no check= to save it` }
  }
}
