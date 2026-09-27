// Farm work can skip an unavailable resource or an inaccessible cell; it cannot skip safety, ownership, or a
// programming error. Keep this policy shared by the farm commands and their helpers.
import { CompositeHandBack } from '../composite.mjs'

const NORMAL_STOPS = new Set(['done', 'days', 'count', 'until'])
// The dig primitive's exact missing-tool refusal is a supply problem, not a
// reason to abandon reachable beds elsewhere in the same field.
const REQUIRED_DIG_TOOL = /\b[a-z_]+ needs a [a-z_]+_(?:pickaxe|axe|shovel|hoe) or better: you carry none, craft one first\b/i
const NO_PLACE_SUPPORT = /\bplaced nothing: \d+ nothing to place against \(first -?\d+,-?\d+,-?\d+\)/i
const PLACE_OCCUPIED = /\bplaced nothing: \d+ [a-z_]+ is already there \(first -?\d+,-?\d+,-?\d+\)/i
const FILL_EMPTY = /\bthe bucket is still empty: stand on the shore 1-2 blocks from the source with a clear view of it, not in the water, and fill again\b/i
const TILL_UNCHANGED = /\btilled nothing: \d+ still (?:dirt|grass_block|dirt_path|coarse_dirt|rooted_dirt): is there a block on top of it\? \(first -?\d+,-?\d+,-?\d+\)/i
const HARD_ERROR = new RegExp(String.raw`\bcancel(?:led|ed)\b|\baborted\b|\bsuperseded\b|\bdied\b|\bdead\b|\boffline\b|\bdisconnected\b|\bhealth\b|\bsleeping\b|spoken to|night and no bed|nothing edible|does not invite work|protected zone|not where its plan says|no (?:place|plan) called|no action called|not a function|not defined|cannot (?:read|set) propert`, 'i')
const RECOVERABLE = new RegExp(String.raw`no (?:walkable |path|seed|hoe|water|bucket|chest|composter|cell|standing|recipe|cobblestone|stick|plank|pen|enclosure)|nowhere to stand|not a spot to stand on|ran out of time|too far|out of reach|unreachable|(?:inventory|chest|storage)(?: is)? full|chest.*empty|chest has less than asked|empty (?:bucket|chest)|short by \d+|missing (?:tool|seed|hoe|bucket|chest|material)|cannot (?:reach|stand|place|dig|see)|could not (?:reach|find)|not enough (?:seed|material|dirt|wood|cobblestone|stick|plank)|nothing (?:left|found|to (?:harvest|collect|plant|feed))|did not take|placing .* failed|place.*refused|no .* (?:carried|within|left|available)|neither a block nor an entity|no first move|the search found nothing|stone where farmland should be`, 'i')

export function assertFarmRecoverable (error) {
  if (error instanceof CompositeHandBack || error?.reason || error instanceof TypeError || error instanceof SyntaxError || error instanceof ReferenceError || HARD_ERROR.test(error?.message ?? '') || (!RECOVERABLE.test(error?.message ?? '') && !REQUIRED_DIG_TOOL.test(error?.message ?? '') && !NO_PLACE_SUPPORT.test(error?.message ?? '') && !PLACE_OCCUPIED.test(error?.message ?? '') && !TILL_UNCHANGED.test(error?.message ?? '') && !FILL_EMPTY.test(error?.message ?? ''))) throw error
}

export const recoverFarm = handler => error => {
  assertFarmRecoverable(error)
  return handler(error)
}

// Nested composites resolve their hand-backs as results. Preserve the runner's hand-back identity so the outer
// command ends with the same stopped= reason instead of continuing mutation after an interrupted harvest.
export async function farmAct (api, name, args) {
  const result = await api.act(name, args)
  if (result?.stopped && !NORMAL_STOPS.has(result.stopped)) throw Object.assign(new CompositeHandBack(result.stopped), { report: result })
  return result
}

export const farmApi = api => ({ ...api, act: (name, args) => farmAct(api, name, args) })

export const FARM_ISSUE_FIELDS = ['storage_full', 'chest_missing', 'kit_short', 'missing', 'bare', 'stuck', 'unfinished', 'skipped', 'lost', 'unreachable', 'inventoryFull', 'inWater', 'stalksOutOfReach', 'notReplanted', 'left', 'inZone', 'gaveUp', 'blocked', 'kept', 'compost', 'parking', 'leftAlone', 'clutter', 'overhead_tree', 'bone_meal_attention', 'attention']
export const farmIssues = summary => Object.fromEntries(FARM_ISSUE_FIELDS.filter(key => Array.isArray(summary[key]) ? summary[key].length > 0 : summary[key]).map(key => [key, summary[key]]))
export function reportFarmAttention (api, { action, place, summary = {}, carried, reasons }) {
  const issues = reasons ?? farmIssues(summary)
  if (!Object.keys(issues).length) return
  const storage = issues.storage_full || issues.chest_missing
  api.emit('farm_attention', {
    action,
    place: place ?? null,
    reasons: issues,
    ...(carried ? { carried } : {}),
    advice: storage
      ? 'Storage needs a decision: repair or add storage, make room, or choose an authorized deposit= destination. The farm keeps working and carries what could not be stored.'
      : 'Inspect these farm issues and choose a repair or supply change. Available work continues; no destination or plan is changed automatically.'
  })
  return true
}
