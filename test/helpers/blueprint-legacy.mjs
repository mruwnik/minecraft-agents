// Synthetic pre-v2 fixtures exercise concrete placement without a production catalog parser path.
import { parseBlueprint, resolve, bill, lint, counts } from '../../src/blueprint/format.mjs'
import { previewCells, blueprintBuilds } from '../../tools/dashboard/lib.mjs'
// ---------------------------------------------------------------- farms as blueprints

// today's farm map (lib.mjs PLAN_LEGEND), one character a column, as the three-layer blueprint of section 2
const FARM_COLUMNS = {
  w: ['farmland #ground', 'wheat[age=0] #crop'],
  c: ['farmland #ground', 'carrots #crop'],
  p: ['farmland #ground', 'potatoes #crop'],
  b: ['farmland #ground', 'beetroots #crop'],
  m: ['farmland #ground', 'melon_stem #crop'],
  k: ['farmland #ground', 'pumpkin_stem #crop'],
  s: ['sand #ground', 'sugar_cane #crop'],
  B: ['dirt #ground', 'bamboo_sapling #crop'],
  '~': ['oak_slab[type=top,waterlogged=true] #ground #cover #lane', 'air #lane'],
  '.': ['dirt|grass_block #ground', 'air #lane'],
  '#': ['dirt|@solid', 'oak_fence'],
  G: ['dirt|@solid', 'oak_fence_gate'],
  T: ['dirt|@solid', 'oak_fence', 'torch'],
  C: ['dirt|@solid', 'chest'],
  K: ['dirt|@solid', 'composter'],
  A: ['dirt|@solid', 'crafting_table'],
  F: ['grass_block', 'dandelion #lane'],
  t: ['dirt', 'oak_sapling #lane']
}
export function farmPlanToBlueprint (map, { name = 'farm', front = 'south' } = {}) {
  const rows = String(map).split('\n').map(r => r.trim()).filter(Boolean)
  const tokens = new Map()
  const tokenFor = spec => {
    if (!tokens.has(spec)) tokens.set(spec, String.fromCharCode(97 + tokens.size + (tokens.size >= 26 ? 39 : 0)))
    return tokens.get(spec)
  }
  const grid = dy => rows.map(row => [...row].map(ch => {
    const column = FARM_COLUMNS[ch]
    const spec = column?.[dy + 1]
    return spec ? tokenFor(spec) : '_'
  }).join(''))
  const grids = [-1, 0, 1].map(dy => `## y${dy}\n\n\`\`\`layer\n${grid(dy).join('\n')}\n\`\`\``).join('\n\n')
  const legend = [...tokens.entries()].map(([spec, token]) => `${token}  ${spec}`).join('\n')
  const text = `---\nname: ${name}\ntitle: ${name}\ndescription: a farm plan as a blueprint\ntags: farm\nfront: ${front}\nfoundation: flat\n---\n\n\`\`\`legend\n${legend}\n\`\`\`\n\n${grids}\n`
  return parseBlueprint(text)
}

export function legacyBlueprintDetail ({ name, text, hash }, places = []) {
  const builds = blueprintBuilds(name, hash, places)
  const parsed = parseBlueprint(text)
  if (parsed.errors.length) return { name, hash, bp: null, bill: null, lint: null, counts: null, errors: parsed.errors, builds }
  const bp = resolve(parsed)
  let checked
  try { checked = lint(bp) } catch (error) { checked = { errors: [`lint failed: ${error.message}`], warnings: [] } }
  return { name, hash, bp, preview: previewCells(bp), bill: bill(bp), lint: checked, counts: counts(bp), errors: [], builds }
}
