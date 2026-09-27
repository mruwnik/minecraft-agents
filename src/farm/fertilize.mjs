// Optional crop growth assistance. One click per matching immature bed, bounded
// by both the eligible beds and the bone meal actually carried.
import { provisionBoneMeal } from './bone-meal.mjs'
import { PLAN_LEGEND, planSpec, planCropMatches } from '../lib/plan.mjs'
import { ripeCrop } from '../lib/farm.mjs'
import { assertFarmRecoverable, farmApi } from './attention.mjs'

const CROPS = new Set(['wheat', 'carrots', 'potatoes', 'beetroots'])
const immature = (api, cell) => {
  const crop = api.block(cell.x, cell.y + 1, cell.z)
  const age = Number(crop?.properties?.age)
  return api.block(cell.x, cell.y, cell.z)?.name === 'farmland' && CROPS.has(crop?.name) &&
    planCropMatches(planSpec(cell), crop.name) && Number.isInteger(age) && age >= 0 && !ripeCrop(crop.name, age)
}

export async function fertilizePlanned (api, cells, report = () => {}, pause = () => api.checkpoint(), source = null) {
  api = farmApi(api)
  const candidates = cells.filter(cell => immature(api, cell))
  const budget = candidates.length
  const supplied = await provisionBoneMeal(api, source, Math.min(budget, candidates.length))
  const result = { bone_meal_fetched: supplied.got, bone_meal_used: 0, bone_meal_attempted: 0, fertilized: 0 }
  const failures = []
  const say = short => {
    const issues = [...(short ? supplied.problems : []), ...(short ? [`short of bone meal for ${short} requested crop attempt${short === 1 ? '' : 's'}: supply bone_meal or set bone_meal=false`] : []), ...failures]
    if (issues.length) result.bone_meal_attention = issues.join('; ')
    report(result)
  }
  for (let i = 0; i < candidates.length && result.bone_meal_attempted < budget; i++) {
    const cell = candidates[i]
    if (!immature(api, cell)) continue
    const before = api.inv().bone_meal ?? 0
    if (before < 1) {
      say(Math.min(budget - result.bone_meal_attempted, candidates.slice(i).filter(c => immature(api, c)).length))
      return result
    }
    const at = { x: cell.x, y: cell.y + 1, z: cell.z }
    result.bone_meal_attempted++
    let error = null
    try { await api.act('fertilize', at) } catch (e) { error = e }
    const used = Math.max(0, before - (api.inv().bone_meal ?? 0))
    result.bone_meal_used += used
    if (used > 0) result.fertilized++
    if (error || !used) failures.push(`${at.x},${at.y},${at.z}: ${error?.message ?? 'bone meal did not take'}`)
    say(0)
    if (error) assertFarmRecoverable(error)
    await pause()
  }
  say(0)
  return result
}
