import { planScaffoldAccess, buildScaffoldAccess, addScaffoldAccess, cleanupScaffold, scaffoldId, reachTreePlatform } from '../scaffold/access.mjs'
import { checkTree, inspectTree, harvestStands, key, treeRoot } from './inspect.mjs'
import { treeProfile } from './profiles.mjs'
import { farmApi, recoverFarm } from '../farm/attention.mjs'
import { foreignZone } from '../../library/farm/shared/clutter.mjs'
import { workRefusal, planCells, planSpec, hasPlan } from '../lib.mjs'
import { isAir, isGroundCover } from '../lib/world.mjs'

export const TREE_ARGS = { x: 'number!', y: 'number!', z: 'number!', species: 'string', form: 'string', place: 'string', flower: 'string', scaffold: 'boolean' }
export function treeAttention (api, action, report) {
  if (report.attention?.length) api.emit('forestry_attention', { action, at: report.root, species: report.species, reasons: report.attention, ...(report.cleanup_left?.length ? { cleanup_left: report.cleanup_left } : {}), ...(report.remaining?.length ? { remaining: report.remaining } : {}), advice: 'Inspect the named tree/site, supply missing resources or safe access, and retry. Protected and ambiguous trees are retained.' })
  return report
}
export function treeContext (api, a) {
  const root = treeRoot(a)
  const place = a.place ? api.places().find(p => p.name === a.place) : null
  if (a.place && !place) throw new Error(`no place called ${a.place}`)
  const refusal = place && workRefusal(place, api.me?.())
  if (refusal) throw new Error(refusal)
  return { root, cells: hasPlan(place) ? planCells(place) : [] }
}
const publicReport = t => ({ root: t.root, species: t.species, form: t.form, state: t.state, wood: t.wood.length, canopy: t.leaves.length, protected: t.protected, attention: [...t.attention] })
async function guard (api, tree) {
  const zones = (await api.act('zones')).zones ?? []
  const positions = [...(tree.footprint ?? []), ...tree.blocks]
  const foreign = positions.find(p => foreignZone(zones, api.me?.(), p))
  if (foreign) throw new Error(`protected zone at ${key(foreign)} does not invite tree work`)
}
const attempt = (api, action, args, report) => api.act(action, args).then(() => true, recoverFarm(e => { report.attention.push(e.message); return false }))
const checkpoint = api => api.checkpoint?.() ?? Promise.resolve()

export async function runTree (api, a, action) {
  if (a.flower && !/^(dandelion|poppy|blue_orchid|allium|azure_bluet|oxeye_daisy|cornflower|lily_of_the_valley|(?:red|orange|white|pink)_tulip)$/.test(a.flower)) throw new Error('flower must be a supported small flower block ID')
  api = farmApi(api)
  const { root, cells } = treeContext(api, a)
  if (action === 'harvest' && api.scaffolds?.(scaffoldId(root))) {
    const prior = { root, attention: [] }
    await cleanupScaffold(api, scaffoldId(root), prior)
    if (prior.cleanup_left?.length || prior.attention.length) return treeAttention(api, 'tree.harvest', prior)
  }
  const read = () => checkTree(api.block, root, a.species, a.form ?? 'auto', cells)
  let tree = read()
  const report = publicReport(tree)
  if (action === 'inspect' || action === 'check') return action === 'inspect' ? { ...report, blocks: tree.blocks, footprint: tree.footprint } : report
  await guard(api, tree)
  const finish = () => treeAttention(api, `tree.${action}`, report)
  if (report.attention.length) return finish()
  if (action === 'harvest') {
    if (tree.state !== 'mature') return finish()
    const access = harvestStands(tree, api.block)
    const inaccessible = tree.blocks.filter(b => !access.get(key(b))?.length)
    let scaffold = null
    if (inaccessible.length) {
      if (a.scaffold === false) {
        report.attention.push(`whole-tree access missing for ${inaccessible.length} blocks (${inaccessible.slice(0, 8).map(key).join(' ')}); provide a safe elevated platform before harvest; tree retained`)
        return finish()
      }
      const zones = (await api.act('zones')).zones ?? []
      const plan = planScaffoldAccess(api, tree, access, zones)
      report.scaffold_needed = plan.count
      if (plan.attention.length) { report.attention.push(...plan.attention); return finish() }
      try { scaffold = await buildScaffoldAccess(api, tree, plan, report) } catch (e) {
        report.attention.push('scaffold construction interrupted; recorded cleanup_left must be recovered before more tree work')
        treeAttention(api, 'tree.harvest', report)
        throw e
      }
      if (report.attention.length) {
        if (scaffold) await cleanupScaffold(api, scaffold.id, report)
        return finish()
      }
      addScaffoldAccess(access, tree, scaffold)
    }
    let interrupted = false
    try {
    report.harvested = 0
    report.remaining = tree.blocks.map(b => ({ x: b.x, y: b.y, z: b.z, name: b.name }))
    api.report?.(report)
    // Top down: no base is cut while upper wood remains. Every actual block and
    // target is checked again; the server, not the click count, establishes removal.
    const ordered = [...tree.blocks].sort((a, b) => b.y - a.y || a.x - b.x || a.z - b.z)
    for (const b of ordered) {
      await checkpoint(api)
      const actual = api.block(b.x, b.y, b.z)
      if (actual && isAir(actual.name)) continue
      if (actual?.name !== b.name) { report.attention.push(`tree changed at ${key(b)}; stopped before touching ${actual?.name ?? 'unloaded'}`); break }
      // Reinspect protected blocks that may have appeared since preflight.
      const current = inspectTree(api.block, root, tree.species, tree.form)
      if (current.protected?.length) { report.attention.push('protected nest/hive/heart appeared during harvest; stopped'); break }
      const spot = await reachTreePlatform(api, access.get(key(b)), report, scaffold?.columns ?? [])
      if (!spot) break
      const pos = api.pos()
      if (Math.hypot(pos.x - (spot.x + 0.5), pos.z - (spot.z + 0.5)) > 0.8 || Math.abs(pos.y - spot.y) > 0.6) { report.attention.push(`did not reach verified work platform ${key(spot)}`); break }
      if (!await attempt(api, 'dig', { x: b.x, y: b.y, z: b.z, batch: true }, report)) break
      const after = api.block(b.x, b.y, b.z)
      if (!after || !isAir(after.name)) { report.attention.push(`dig left ${key(b)} standing or unloaded`); break }
      report.harvested++
      report.remaining = report.remaining.filter(p => key(p) !== key(b))
      api.report?.(report)
    }
    report.remaining = tree.blocks.filter(b => api.block(b.x, b.y, b.z)?.name === b.name).map(b => ({ x: b.x, y: b.y, z: b.z, name: b.name }))
    if (report.remaining.length && !report.attention.length) report.attention.push(`${report.remaining.length} tree blocks remain; harvest incomplete`)
    if (report.harvested) await attempt(api, 'collect', { range: Math.min(16, tree.profile.radius + 2) }, report)
    return finish()
    } catch (e) {
      interrupted = true
      if (scaffold) { report.attention.push('tree work interrupted; scaffold cleanup remains recorded'); treeAttention(api, 'tree.harvest', report) }
      throw e
    } finally {
      if (scaffold && !interrupted) { await cleanupScaffold(api, scaffold.id, report); treeAttention(api, 'tree.harvest', report) }
    }
  }
  if (tree.state === 'mature') { report.attention.push('mature tree retained; harvest it before preparing or replanting'); return finish() }
  if (tree.state === 'blocked') { report.attention.push(`planting footprint occupied at ${key(root)}; inspect before clearing`); return finish() }
  report.cleared = 0
  for (const pos of tree.footprint) {
    const b = api.block(pos.x, pos.y, pos.z)
    if (b && isGroundCover(b.name)) {
      await checkpoint(api)
      if (!await attempt(api, 'dig', { ...pos, batch: true }, report)) continue
      if (isAir(api.block(pos.x, pos.y, pos.z)?.name)) report.cleared++
    }
  }
  if (action === 'prepare') return finish()
  // A requested flower is established before any sapling. Never pick an arbitrary
  // occupied neighboring cell or overwrite a different planned feature.
  if (a.flower) {
    if (!/^(dandelion|poppy|blue_orchid|allium|azure_bluet|oxeye_daisy|cornflower|lily_of_the_valley|(?:red|orange|white|pink)_tulip)$/.test(a.flower)) throw new Error('flower must be a supported small flower block ID')
    const planned = cells.filter(c => planSpec(c)?.kind === 'flower' && planSpec(c).item === a.flower && c.y === root.y && Math.max(Math.abs(c.x - root.x), Math.abs(c.z - root.z)) <= 2).map(c => ({ x: c.x, y: c.y + 1, z: c.z }))
    const choices = planned.length ? planned : [[-1, 0], [0, -1], [tree.profile.width, 0], [0, tree.profile.width]].map(([dx, dz]) => ({ x: root.x + dx, y: root.y + 1, z: root.z + dz }))
    if (!choices.some(p => api.block(p.x, p.y, p.z)?.name === a.flower)) {
      const at = choices.find(p => ['dirt', 'grass_block'].includes(api.block(p.x, p.y - 1, p.z)?.name) && isAir(api.block(p.x, p.y, p.z)?.name) && !cells.some(c => c.x === p.x && c.z === p.z && c.y + 1 === p.y && planSpec(c)?.kind !== 'flower'))
      if (!at) report.attention.push('no free adjacent flower site; reserve one beside the tree')
      else if (!(api.inv()[a.flower] > 0)) report.attention.push(`missing flower ${a.flower}; tree planting waits until the flower is established`)
      else {
        const zones = (await api.act('zones')).zones ?? []
        if (foreignZone(zones, api.me?.(), at)) throw new Error(`protected zone at ${key(at)}`)
        await checkpoint(api)
        if (!['dirt', 'grass_block'].includes(api.block(at.x, at.y - 1, at.z)?.name)) report.attention.push(`flower needs dirt or grass support at ${at.x},${at.y - 1},${at.z}`)
        else if (await attempt(api, 'place', { ...at, item: a.flower }, report) && api.block(at.x, at.y, at.z)?.name !== a.flower) report.attention.push(`flower placement unverified at ${key(at)}`)
      }
    }
  }
  if (report.attention.length) return finish()
  const p = treeProfile(tree.species, tree.form)
  const missing = tree.footprint.filter(pos => ![p.plant, ...(p.species === 'azalea' ? ['flowering_azalea'] : [])].includes(api.block(pos.x, pos.y, pos.z)?.name))
  if ((api.inv()[p.plant] ?? 0) < missing.length) { report.attention.push(`missing ${missing.length - (api.inv()[p.plant] ?? 0)} ${p.plant}; ${p.width}x${p.width} planting requires the complete footprint`); return finish() }
  report.planted = 0
  for (const pos of missing) {
    await checkpoint(api)
    const before = api.block(pos.x, pos.y, pos.z)
    if (!before || !isAir(before.name)) { report.attention.push(`planting cell changed at ${key(pos)}`); break }
    if (!await attempt(api, 'place', { ...pos, item: p.plant }, report)) break
    if (api.block(pos.x, pos.y, pos.z)?.name !== p.plant) { report.attention.push(`planting unverified at ${key(pos)}`); break }
    report.planted++
  }
  return finish()
}
