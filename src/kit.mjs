// The kit a routine day needs, decided from what is carried, what the plot's chest holds and what the crafting grid can
// make of either (card 6cf481c0): a stone hoe broke mid-routine with no spare, the side craft that would have replaced
// it superseded the routine, and on autopilot nobody is there to craft. Pure: library/kit.mjs walks and clicks.
const TIERS = ['wooden', 'stone', 'iron', 'golden', 'diamond', 'netherite']
const KINDS = ['hoe', 'axe', 'pickaxe', 'shovel', 'sword']
// the never-eat list the body keeps (BANNED_FOOD in src/bot.mjs): rations in a chest are counted the same way
export const NEVER_EAT = ['rotten_flesh', 'spider_eye', 'poisonous_potato', 'pufferfish', 'chicken']
// heads per tool and sticks per tool, the game's recipes; shears are two ingots and no handle
const TOOL_RECIPES = { hoe: { head: 2, stick: 2 }, axe: { head: 3, stick: 2 }, pickaxe: { head: 3, stick: 2 }, shovel: { head: 1, stick: 2 }, sword: { head: 2, stick: 1 } }
const STONE_HEADS = ['cobblestone', 'cobbled_deepslate', 'blackstone']
const HEADS = {
  wooden: { is: name => name.endsWith('_planks'), word: 'planks' },
  stone: { is: name => STONE_HEADS.includes(name), word: `cobblestone (or ${STONE_HEADS.slice(1).join(', ')})` },
  iron: { is: name => name === 'iron_ingot', word: 'iron_ingot' },
  golden: { is: name => name === 'gold_ingot', word: 'gold_ingot' },
  diamond: { is: name => name === 'diamond', word: 'diamond' }
}
const isPlanks = name => name.endsWith('_planks')
const isLog = name => /_log$/.test(name)
const STICKS_PER_CRAFT = 4 // 2 planks
const PLANKS_PER_LOG = 4

// tools=stone_hoe,shears as the command line gives it, or a list already
export const toolList = tools => (Array.isArray(tools) ? tools : String(tools ?? '').split(',')).map(t => String(t).trim()).filter(Boolean)

// stone_hoe -> hoe, shears -> shears, anything else -> null
export const kindOf = name => {
  if (name === 'shears') return 'shears'
  const tier = TIERS.find(t => name.startsWith(`${t}_`))
  const kind = tier ? name.slice(tier.length + 1) : null
  return kind && KINDS.includes(kind) ? kind : null
}
const ofKind = (name, kind) => kindOf(name) === kind || name === kind
export const carriedOfKind = (items, kind) => Object.entries(items ?? {}).filter(([name, count]) => count > 0 && ofKind(name, kind)).reduce((sum, [, count]) => sum + count, 0)

// how many of what is wanted the pools hold between them (carried first, then the chest)
const available = (pools, is) => pools.reduce((sum, pool) => sum + Object.entries(pool).filter(([name, count]) => count > 0 && is(name)).reduce((s, [, c]) => s + c, 0), 0)
// take `count` matching items, carried before chest (materials), or from the chest alone (a tool or a ration already
// counted as carried is not taken twice); what came out of the chest goes on the take list
const draw = (pools, is, count, take, chestOnly = false) => {
  let left = count
  pools.forEach((pool, i) => {
    if (chestOnly && i === 0) return
    for (const [name, have] of Object.entries(pool)) {
      if (left <= 0 || have <= 0 || !is(name)) continue
      const n = Math.min(have, left)
      pool[name] -= n
      left -= n
      if (i === 1) take[name] = (take[name] ?? 0) + n
    }
  })
  return count - left
}
// the name planks take from their log
const planksOf = log => log.replace(/_log$/, '_planks')

// sticks: carried or in the chest, else made from planks, else from a log; how many can be had at most
const sticksObtainable = pools => available(pools, n => n === 'stick') + Math.floor(available(pools, isPlanks) / 2) * STICKS_PER_CRAFT + available(pools, isLog) * PLANKS_PER_LOG / 2 * STICKS_PER_CRAFT
const drawSticks = (pools, count, take, craft) => {
  let left = count - draw(pools, n => n === 'stick', count, take)
  if (left <= 0) return
  const crafts = Math.ceil(left / STICKS_PER_CRAFT)
  const planksWanted = crafts * 2
  let planksShort = planksWanted - draw(pools, isPlanks, planksWanted, take)
  if (planksShort > 0) {
    const logs = Math.ceil(planksShort / PLANKS_PER_LOG)
    const log = pools.flatMap(pool => Object.keys(pool).filter(n => isLog(n) && pool[n] > 0))[0]
    draw(pools, n => n === log, logs, take)
    craft.push({ item: planksOf(log), count: logs * PLANKS_PER_LOG })
    const planks = pools[0]
    planks[planksOf(log)] = (planks[planksOf(log)] ?? 0) + logs * PLANKS_PER_LOG - planksShort
    planksShort = 0
  }
  craft.push({ item: 'stick', count: crafts * STICKS_PER_CRAFT })
  pools[0].stick = (pools[0].stick ?? 0) + crafts * STICKS_PER_CRAFT - left
}

const toolPlan = (tool, spare, pools, chestAt, take, craft) => {
  const kind = kindOf(tool) ?? tool
  const wanted = 1 + spare
  const have = carriedOfKind(pools[0], kind)
  let need = wanted - have
  if (need <= 0) return null
  need -= draw(pools, n => ofKind(n, kind), need, take, true)
  if (need <= 0) return null
  const tier = TIERS.find(t => tool.startsWith(`${t}_`))
  const recipe = kind === 'shears' ? { head: 2, stick: 0 } : TOOL_RECIPES[kind]
  const head = kind === 'shears' ? HEADS.iron : HEADS[tier]
  const where = chestAt ? `carried or in the chest at ${chestAt}` : 'carried, and no chest'
  if (!recipe || !head) return `${tool}: ${have} carried, ${wanted} wanted: no recipe known here, craft it by hand`
  const byHeads = Math.floor(available(pools, head.is) / recipe.head)
  const bySticks = recipe.stick ? Math.floor(sticksObtainable(pools) / recipe.stick) : need
  const made = Math.min(need, byHeads, bySticks)
  if (made > 0) {
    draw(pools, head.is, made * recipe.head, take)
    if (recipe.stick) drawSticks(pools, made * recipe.stick, take, craft)
    craft.push({ item: tool, count: made })
  }
  if (made >= need) return null
  const lacking = byHeads <= bySticks ? head.word : 'stick (or planks, a log)'
  return `${tool}: ${have} carried, ${wanted} wanted${made ? `, ${made} made` : ''}: no ${lacking} ${where}`
}

const foodPlan = (food, pools, chestAt, isFood, take, craft) => {
  const edible = name => isFood(name) && !NEVER_EAT.includes(name)
  const carried = available([pools[0]], edible)
  let need = food - carried
  if (need <= 0) return null
  // the chest's biggest stack first: one kind of ration is easier to count than four
  const stacks = Object.entries(pools[1] ?? {}).filter(([name, count]) => count > 0 && edible(name)).sort((a, b) => b[1] - a[1])
  let taken = 0
  for (const [name] of stacks) {
    const n = draw(pools, m => m === name, need, take, true)
    taken += n
    need -= n
    if (need <= 0) return null
  }
  const loaves = Math.min(need, Math.floor(available(pools, n => n === 'wheat') / 3))
  if (loaves > 0) {
    draw(pools, n => n === 'wheat', loaves * 3, take)
    craft.push({ item: 'bread', count: loaves })
    need -= loaves
  }
  if (need <= 0) return null
  const got = [taken ? `${taken} taken` : '', loaves ? `${loaves} baked` : ''].filter(Boolean).join(', ')
  if (!chestAt) return `food: ${carried} carried, ${food} wanted${got ? `, ${got}` : ''}, and no chest`
  return `food: ${carried} carried, ${food} wanted${got ? `, ${got}` : ''}: the chest at ${chestAt} has ${taken ? 'no more' : 'none'}`
}

// what to take out of the chest, what to craft (in order), and what stays short, said so a driver can fix it
export function kitPlan ({ tools = [], spare = 1, food = 0, carried = {}, chest = null, chestAt = null, isFood = () => false }) {
  const pools = [{ ...carried }, { ...(chest ?? {}) }]
  const at = chest ? chestAt : null
  const take = {}
  const craft = []
  const short = []
  for (const tool of tools) {
    const gap = toolPlan(tool, spare, pools, at, take, craft)
    if (gap) short.push(gap)
  }
  const hungry = foodPlan(food, pools, at, isFood, take, craft)
  if (hungry) short.push(hungry)
  return { take, craft, short }
}

// kit=hoe:2 shears:1 food:12, from what is carried after the kit ran
export const kitLine = (tools, items, isFood) => {
  const kinds = [...new Set(tools.map(t => kindOf(t) ?? t))]
  const food = available([items], name => isFood(name) && !NEVER_EAT.includes(name))
  return [...kinds.map(kind => `${kind}:${carriedOfKind(items, kind)}`), `food:${food}`].join(' ')
}

// the kinds a step ended with fewer of than it began: a tool wore out under it
export const toolsLost = (before, after, tools) => [...new Set(tools.map(t => kindOf(t) ?? t))].filter(kind => carriedOfKind(after, kind) < carriedOfKind(before, kind))
