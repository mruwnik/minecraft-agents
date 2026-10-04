// The shared map everyone reads (help section map).
import { hasPlan, parseStructurePlan, legacyPlanStructure } from '../../lib/plan.mjs'
import { markMove, planStands, planCells, planErrors, mapRefusal, describePlaces, describePlace, markFields, matchPlaces, compact } from '../../lib.mjs'
import { zones, saveZones, readPlaces, savePlaces } from '../events.mjs'
import { Vec3, bot } from '../state.mjs'

export const mapQuick = {
  // shared points of interest: mark name= kind=<base|mine|farm|village|danger|resource|...> note= [x= y= z=, default: here]
  // map= saves an ASCII plan with the place (see farm.plan, which is what validates one). Marking a place again keeps
  // the plan, the OWNER and anything else already saved under that name: only what you pass is replaced (backlog #141).
  mark (a) {
    if (!a.name) throw new Error('mark needs name= (and ideally kind= and note=)')
    const saved = readPlaces().find(p => p.name === a.name)
    // moving somebody else's place, re-planning it or calling it something else overwrites THEIR record of it.
    // Adding to its note is how agents leave each other word and stays open (markFields keeps the owner through it)
    const rewrites = a.structure !== undefined || a.legend !== undefined || a.map !== undefined || a.x !== undefined || (a.kind !== undefined && a.kind !== saved?.kind)
    const refusal = rewrites ? mapRefusal(saved, bot.username) : null
    if (refusal) throw new Error(refusal)
    // a note-only mark used to move the place to my feet (BUGS.md 09-24 12:42Z): markMove keeps the anchor, says a move
    // out loud, and refuses one off a plan that still stands where it was marked
    const stands = hasPlan(saved) ? planStands(planCells(saved), (x, y, z) => bot.blockAt(new Vec3(x, y, z))) : false
    const where = markMove({ saved, args: a, here: bot.entity.position, stands })
    if (where.error) throw new Error(where.error)
    const at = where.at
    if (a.map !== undefined && a.structure !== undefined) throw new Error('choose structure= or legacy map= import, not both')
    if (a.legend !== undefined && a.map === undefined) throw new Error('legend= is only accepted with legacy map= import; update structure.legend instead')
    const structure = a.structure !== undefined ? parseStructurePlan(a.structure).structure : a.map !== undefined ? legacyPlanStructure(a.map, a.legend) : saved?.structure
    const parsed = a.structure !== undefined ? parseStructurePlan(a.structure) : structure ? parseStructurePlan(structure) : null
    const errors = parsed ? planErrors(parsed) : []
    if (errors.length) throw new Error(errors.join('; '))
    const fields = markFields({ saved, by: bot.username, note: a.note })
    if (fields.error) throw new Error(fields.error)
    const place = { ...saved, name: String(a.name), kind: a.kind ?? saved?.kind ?? 'place', x: Math.floor(at.x), y: Math.floor(at.y), z: Math.floor(at.z), by: fields.by, note: fields.note, structure }
    delete place.plan
    delete place.legend
    savePlaces([...readPlaces().filter(p => p.name !== place.name), place])
    return { marked: place.name, at: `${place.x},${place.y},${place.z}`, moved: where.moved, plan: parsed ? `${parsed.width}x${parsed.maxY - parsed.minY + 1}x${parsed.height}` : undefined }
  },
  // deleting an entry off the shared map is never leaving word: whoever marked it is the only one who can take it off
  unmark (a) {
    const refusal = mapRefusal(readPlaces().find(p => p.name === a.name), bot.username)
    if (refusal) throw new Error(refusal)
    savePlaces(readPlaces().filter(p => p.name !== a.name))
    return {}
  },
  places (a) {
    const all = readPlaces()
    const from = bot.entity.position
    if (a.name !== undefined) {
      const one = describePlace(all, String(a.name), from)
      if (!one) throw new Error(`no place called ${a.name}: search for it with places q=${String(a.name).slice(0, 12)}`)
      return one
    }
    // places.json is shared by every body and grows without limit: a list that silently stopped at 12 sent agents to
    // read the file. The filters are the search, and the tail says what they did not see
    const search = { q: a.q, by: a.by, kind: a.kind, within: a.within }
    const found = matchPlaces(all, from, search)
    const lines = describePlaces(all, from, { ...search, limit: a.limit })
    const asked = compact(Object.fromEntries(Object.entries(search).filter(([, v]) => v !== undefined)), false)
    if (!lines.length) return { text: all.length ? `no place matches ${asked || 'that'}: ${all.length} are marked, try a shorter q= or drop within=` : 'no places marked yet' }
    const more = found.length - lines.length
    return { text: [...lines, more > 0 ? `... and ${more} more of ${all.length} marked: narrow it with q= by= kind= within=, or raise limit=` : ''].filter(Boolean).join('\n') }
  },

  zones () { return { zones } },
  protect (a) {
    const zone = Object.fromEntries(['name', 'x1', 'y1', 'z1', 'x2', 'y2', 'z2'].map(k => [k, a[k]]))
    if (Object.values(zone).some(v => v === undefined)) throw new Error('protect needs name,x1,y1,z1,x2,y2,z2')
    zones.splice(0, zones.length, ...zones.filter(z => z.name !== zone.name), zone)
    saveZones()
    return { zones: zones.length }
  },
  unprotect (a) {
    zones.splice(0, zones.length, ...zones.filter(z => z.name !== a.name))
    saveZones()
    return { zones: zones.length }
  }
}
