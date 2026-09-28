import { treeHarvestSequence, visibleTreeBlock } from '../scaffold/reach.mjs'
import { planTreeAccess, planScaffoldAccess, buildScaffoldAccess, addScaffoldAccess, cleanupScaffold, scaffoldId, reachTreePlatform } from '../scaffold/access.mjs'
import { checkTree, inspectTree, harvestStands, key, treeRoot } from './inspect.mjs'
import { treeProfile } from './profiles.mjs'
import { farmApi, recoverFarm } from '../farm/attention.mjs'
import { foreignZone } from '../../library/farm/shared/clutter.mjs'
import { workRefusal, planCells, planSpec, hasPlan, parsePlacePlan } from '../lib.mjs'
import { isAir, isGroundCover } from '../lib/world.mjs'
import { validateForestryRecord, forestryRecordMatchesSite, isLegacyPlannedStump, legacyPlannedStumpRefusal } from './journal.mjs'
import { handleCenterWorkRefusal, cleanupScaffoldRecoverably } from '../scaffold/recovery.mjs'
import { allowClaimedHives, forestHiveClaim, hiveSmokeCampfire } from './hives.mjs'

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
  return { root, place, cells: hasPlan(place) ? planCells(place) : [] }
}
const publicReport = t => ({ root: t.root, species: t.species, form: t.form, state: t.state, wood: t.wood.length, canopy: t.leaves.length, protected: t.protected, attention: [...t.attention] })

export function observedTreeProgress (blocks, blockAt, scaffold=null) {
  const supports = new Map((scaffold?.cells ?? []).map(p => [key(p), p.item]))
  const remaining = blocks.filter(p => {
    const current = blockAt(p.x, p.y, p.z)
    if (!current || !isAir(current.name)) {
      if (current && supports.get(key(p)) === current.name) return false
      // A changed non-air block is unresolved, not proof that the original
      // target was harvested. Keep it in the report until inspected.
      return true
    }
    return false
  }).map(({ x, y, z, name }) => ({ x, y, z, name }))
  return { remaining, harvested: blocks.length - remaining.length }
}
async function guard (api, tree) {
  const zones = (await api.act('zones')).zones ?? []
  const positions = [...(tree.footprint ?? []), ...tree.blocks, ...(tree.protected ?? [])]
  const foreign = positions.find(p => foreignZone(zones, api.me?.(), p))
  if (foreign) throw new Error(`protected zone at ${key(foreign)} does not invite tree work`)
}
const attempt = (api, action, args, report) => api.act(action, args).then(() => true, recoverFarm(e => {
  if (action === 'goto' || action === 'collect') api.acknowledgeFailure?.(action)
  report.attention.push(e.message)
  return false
}))
const checkpoint = api => api.checkpoint?.() ?? Promise.resolve()

export async function runTree (api, a, action, { pillar = false, decayRecovery = false, allowForestHives = false } = {}) {
  if (a.flower && !/^(dandelion|poppy|blue_orchid|allium|azure_bluet|oxeye_daisy|cornflower|lily_of_the_valley|(?:red|orange|white|pink)_tulip)$/.test(a.flower)) throw new Error('flower must be a supported small flower block ID')
  api = farmApi(api)
  const { root, place, cells } = treeContext(api, a)
  const parsedForest = action === 'harvest' && allowForestHives && place?.kind === 'forest' && place.by === api.me?.()
    ? parsePlacePlan(place)
    : null
  const forestScope = parsedForest && !parsedForest.error
    ? { place: place.name, owner: place.by, x: place.x, z: place.z, width: parsedForest.width, height: parsedForest.height }
    : null
  // The user has authorized clearing any obstruction on the claimed forest.
  // Persist a narrow, revalidated plot scope so legacy scaffold cleanup can
  // reclaim an adjacent unjournaled scaffold there, without extending that
  // authority to similarly named or neighboring plots.
  if (forestScope) {
    const id = scaffoldId(root), priorRecord = api.scaffolds?.(id)
    if (priorRecord) api.scaffolds(id, { ...priorRecord, forestScope })
  }
  if (action === 'harvest' && api.scaffolds?.(scaffoldId(root))) {
    const prior = { root, attention: [] }
    await cleanupScaffoldRecoverably(api, cleanupScaffold, scaffoldId(root), prior)
    if (prior.cleanup_left?.length || prior.attention.length) return treeAttention(api, 'tree.harvest', prior)
  }
  const read = () => {
    const observed = checkTree(api.block, root, a.species, a.form ?? 'auto', cells)
    if (!allowForestHives) return observed
    const policy = allowClaimedHives(observed, place, api.me?.(), api.hasSilkTouch?.())
    if (api.hasSilkTouch?.()) return policy.tree
    // check/inspect must not block maintain's chance to place a campfire.
    // Actual removal below independently refuses unless smoke is verified.
    const verifiedSmoke = policy.hives.map(h => hiveSmokeCampfire(api.block, h)).filter(Boolean)
    return { ...policy.tree, attention: policy.tree.attention.filter(reason => {
      if (reason.startsWith('Silk Touch or a verified campfire')) return false
      const match = reason.match(/^blocks adjoining trunk need inspection: (.+)$/)
      if (!match) return true
      const cells = match[1].split(' ')
      const knownSmoke = new Set(verifiedSmoke.map(p => `campfire@${p.x},${p.y},${p.z}`))
      return !cells.every(cell => knownSmoke.has(cell))
    }) }
  }
  const journalId = scaffoldId(root)
  let forestry = api.forestry?.(journalId) ?? null
  let tree = read()
  let resumedHarvest = false
  if (forestry) {
    if (!forestryRecordMatchesSite(forestry, tree)) {
      tree.attention.push('forestry journal root/species/form does not match this planned tree site')
    } else if (forestry.phase === 'harvesting') {
      const resumed = validateForestryRecord(forestry, tree, api.block)
      if (resumed.ok) { tree = resumed.tree; resumedHarvest = true }
      else tree.attention.push(resumed.reason)
    } else if (forestry.phase === 'decaying' && ['harvest', 'plant'].includes(action) && !(action === 'plant' && decayRecovery)) {
      tree.attention.push('tree has a pending decay collection and replant record; forestry.maintain must finish it first')
    } else if (!['harvesting', 'decaying'].includes(forestry.phase)) {
      tree.attention.push('forestry journal phase is invalid; inspect the saved tree record before work')
    }
  } else if (api.forestry && isLegacyPlannedStump(tree, cells)) {
    tree.attention = tree.attention.filter(s => s !== 'wood has no attributable canopy: possible build or incomplete tree; inspect manually')
  } else if (api.forestry && tree.attention.includes('wood has no attributable canopy: possible build or incomplete tree; inspect manually')) {
    tree.attention.push(`legacy stump recovery refused: ${legacyPlannedStumpRefusal(tree, cells)}`)
  }
  const report = publicReport(tree)
  const refreshRemaining = scaffold => {
    Object.assign(report, observedTreeProgress(tree.blocks, api.block, scaffold))
  }
  if (action === 'inspect' || action === 'check') return action === 'inspect' ? { ...report, blocks: tree.blocks, footprint: tree.footprint } : report
  await guard(api, tree)
  const finish = () => { api.report?.(report); return treeAttention(api, `tree.${action}`, report) }
  if (report.attention.length) return finish()
  if (action === 'harvest') {
    if (tree.state !== 'mature') {
      if (resumedHarvest && forestry?.wood.every(p => isAir(api.block(p.x, p.y, p.z)?.name))) {
        forestry = { ...forestry, phase: 'decaying', readyAt: forestry.readyAt ?? Date.now() + 120000 }
        api.forestry(journalId, forestry)
        report.harvested = forestry.wood.length
        report.remaining = []
        report.decay_wait = Math.max(0, Math.ceil((forestry.readyAt - Date.now()) / 1000))
      }
      return finish()
    }
    // An owned forest's nest is moved intact before its supporting logs are
    // touched. The primitive independently checks the exact claim and equips
    // the actual enchanted tool; an ordinary fast-tool dig would destroy it.
    const hives = allowForestHives ? allowClaimedHives(tree, place, api.me?.(), true).hives : []
    for (const hive of hives) {
      await checkpoint(api)
      if (api.block(hive.x, hive.y, hive.z)?.name !== hive.name) {
        report.attention.push(`hive changed at ${key(hive)} before safe pickup`)
        return finish()
      }
      const silk = Boolean(api.hasSilkTouch?.())
      let smoke = null, cleanupCampfire = false
      if (!silk) {
        smoke = hiveSmokeCampfire(api.block, hive)
        if (!smoke) {
          const at = { x: hive.x, y: hive.y - 1, z: hive.z }
          if (api.inv().campfire > 0 && isAir(api.block(at.x, at.y, at.z)?.name) && forestHiveClaim(place, api.me?.(), at, 'beehive')) {
            if (!await attempt(api, 'place', { item: 'campfire', ...at }, report)) {
              cleanupCampfire = api.block(at.x, at.y, at.z)?.name === 'campfire'
              smoke = cleanupCampfire ? at : null
            }
            cleanupCampfire = api.block(at.x, at.y, at.z)?.name === 'campfire'
            smoke = hiveSmokeCampfire(api.block, hive)
          }
        }
        if (!smoke) {
          report.attention.push(`no verifiable campfire smoke below ${hive.name}@${key(hive)}; hive retained (provide Silk Touch or a campfire with a clear 1–5 block smoke column)`)
          if (cleanupCampfire) {
            await attempt(api, 'dig', { x: hive.x, y: hive.y - 1, z: hive.z }, report)
            if (!isAir(api.block(hive.x, hive.y - 1, hive.z)?.name)) report.attention.push(`temporary campfire remains at ${hive.x},${hive.y - 1},${hive.z}; cleanup required`)
          }
          return finish()
        }
        // Forest-plan blocks may be cleared by this job. Removing the verified
        // smoker also prevents it from becoming an ambiguous tree obstruction.
        cleanupCampfire = forestHiveClaim(place, api.me?.(), smoke, 'beehive')
      }
      const before = api.inv()[hive.name] ?? 0
      const cleanSmoke = async () => {
        if (!cleanupCampfire || !smoke || isAir(api.block(smoke.x, smoke.y, smoke.z)?.name)) return true
        const removed = await attempt(api, 'dig', { ...smoke }, report)
        if (!removed || !isAir(api.block(smoke.x, smoke.y, smoke.z)?.name)) {
          report.attention.push(`temporary campfire remains at ${key(smoke)}; cleanup required`)
          return false
        }
        return true
      }
      if (!await attempt(api, 'dig', { x: hive.x, y: hive.y, z: hive.z, ...(silk ? { silk_touch: true } : { safe_hive: true, smoke }), place: a.place }, report)) {
        await cleanSmoke()
        return finish()
      }
      if (!isAir(api.block(hive.x, hive.y, hive.z)?.name)) {
        report.attention.push(`hive remained at ${key(hive)} after attempted safe removal`)
        await cleanSmoke()
        return finish()
      }
      if (!await cleanSmoke()) return finish()
      if (!silk) {
        report.hivesDestroyed = [...(report.hivesDestroyed ?? []), { ...hive }]
        tree = read()
        report.protected = tree.protected
        if (tree.attention.length) { report.attention.push(...tree.attention); return finish() }
        continue
      }
      if (!await attempt(api, 'collect', { range: 6 }, report)) return finish()
      if ((api.inv()[hive.name] ?? 0) <= before) {
        report.attention.push(`moved ${hive.name}@${key(hive)} but its Silk Touch drop was not recovered; retain tree until collected`)
        return finish()
      }
      report.hivesMoved = [...(report.hivesMoved ?? []), { ...hive }]
      tree = read()
      report.protected = tree.protected
      if (tree.attention.length) { report.attention.push(...tree.attention); return finish() }
    }
    // Natural leaves decay after the last log is gone; only wood is work.
    tree = { ...tree, blocks: tree.wood }
    const access = harvestStands(tree, api.block)
    const inaccessible = treeHarvestSequence(tree,access,api.block).missing
    let scaffold = null
    if (inaccessible.length) {
      if (a.scaffold === false) {
        report.attention.push(`whole-tree access missing for ${inaccessible.length} blocks (${inaccessible.slice(0, 8).map(key).join(' ')}); provide a safe elevated platform before harvest; tree retained`)
        return finish()
      }
      const zones = (await api.act('zones')).zones ?? []
      const plan = pillar ? planScaffoldAccess(api, tree, access, zones, true) : planTreeAccess(api, tree, access, zones)
      report.scaffold_needed = plan.count
      if (plan.attention.length) { report.attention.push(...plan.attention); return finish() }
      if (api.forestry && !forestry) {
        forestry = { root: { ...root }, species: tree.species, form: tree.form,
          wood: tree.wood.map(({ x, y, z, name }) => ({ x, y, z, name })),
          leaves: tree.leaves.map(({ x, y, z, name }) => ({ x, y, z, name })),
          phase: 'harvesting', readyAt: null }
        api.forestry(journalId, forestry)
      }
      try { scaffold = await buildScaffoldAccess(api, tree, plan, report) } catch (e) {
        report.attention.push('scaffold construction interrupted; recorded cleanup_left must be recovered before more tree work')
        treeAttention(api, 'tree.harvest', report)
        throw e
      }
      if (scaffold && forestScope) {
        scaffold = { ...scaffold, forestScope }
        api.scaffolds?.(scaffold.id, scaffold)
      }
      if (report.attention.length) {
        if (scaffold) await cleanupScaffoldRecoverably(api, cleanupScaffold, scaffold.id, report)
        if(!pillar&&plan.kind!=='pillar'&&!report.cleanup_left?.length&&report.attention.some(s=>/no walkable path|no first move|did not reach|no verified tree work platform|not visibly reachable|scaffold platform placement not verified|scaffold_side: placing scaffolding did not take/.test(s))){
          const alternate=planScaffoldAccess(api,tree,harvestStands(tree,api.block),zones,true)
          if(!alternate.attention.length)return runTree(api,a,action,{pillar:true,allowForestHives})
        }
        return finish()
      }
      addScaffoldAccess(access, tree, scaffold)
    }
    let interrupted = false
    try {
    if (api.forestry && !forestry) {
      forestry = { root: { ...root }, species: tree.species, form: tree.form,
        wood: tree.wood.map(({ x, y, z, name }) => ({ x, y, z, name })),
        leaves: tree.leaves.map(({ x, y, z, name }) => ({ x, y, z, name })),
        phase: 'harvesting', readyAt: null }
      api.forestry(journalId, forestry)
    }
    report.harvested = 0
    report.remaining = tree.blocks.map(b => ({ x: b.x, y: b.y, z: b.z, name: b.name }))
    api.report?.(report)
    // Prefer upper wood first; a verified trunk climb may clear lower logs for
    // access. The server, not the click count, establishes removal.
    const workView = (x,y,z) => scaffold?.clearance?.some(p=>p.x===x&&p.y===y&&p.z===z) ? {name:'air'} : api.block(x,y,z)
    // Natural leaves belonging to this tree are valid intervening blocks for a
    // coordinate-targeted wood dig. They remain untouched; every target is
    // still rechecked as the expected log and verified as air afterwards.
    const transparentCanopy = new Set((tree.leaves ?? []).map(key))
    const sequence = treeHarvestSequence(tree, access, workView)
    if (sequence.missing.length) { report.attention.push(`whole-tree visible access missing for ${sequence.missing.length} blocks; tree retained`); return finish() }
    for (const {block:b,spots} of sequence.steps) {
      await checkpoint(api)
      const actual = api.block(b.x, b.y, b.z)
      if (actual && (isAir(actual.name)||scaffold?.cells.some(p=>key(p)===key(b)))) continue
      if (actual?.name !== b.name) { report.attention.push(`tree changed at ${key(b)}; stopped before touching ${actual?.name ?? 'unloaded'}`); break }
      // Reinspect protected blocks that may have appeared since preflight.
      const current = inspectTree(api.block, root, tree.species, tree.form)
      if (current.protected?.length) { report.attention.push('protected nest/hive/heart appeared during harvest; stopped'); break }
      const candidates = [...new Map(spots.map(p=>[key(p),p])).values()].slice(0,3)
      let spot = null
      const failures = []
      for (const candidate of candidates) {
        const local = { attention: [] }
        let reached
        const reachStructure = { ...(scaffold ?? { columns: [], platforms: [] }), ...(forestScope ? { forestScope } : {}) }
        try { reached = await reachTreePlatform(api, [candidate], local, reachStructure) }
        catch (error) {
          if (!handleCenterWorkRefusal(api, error, local, `work platform ${key(candidate)} refused centering`)) throw error
          failures.push(...local.attention)
          continue
        }
        if (!reached) { failures.push(...local.attention); continue }
        const current = api.block(b.x,b.y,b.z)
        if (current && isAir(current.name)) { spot = reached; break }
        if (!current || current.name !== b.name) {
          failures.push(`tree changed at ${key(b)}; stopped before touching ${current?.name ?? 'unloaded'}`)
          break
        }
        const pos = api.pos()
        if (Math.hypot(pos.x - (reached.x + 0.5), pos.z - (reached.z + 0.5)) > 0.8 || Math.abs(pos.y - reached.y) > 0.6) {
          failures.push(`did not reach verified work platform ${key(reached)}`)
          continue
        }
        if (!visibleTreeBlock(api.block, pos, b, new Set(), transparentCanopy)) {
          failures.push(`tree target ${key(b)} is not visibly reachable from verified platform ${key(reached)}`)
          continue
        }
        spot = reached
        break
      }
      if (!spot) {
        report.attention.push(...new Set(failures.length ? failures : [`tree target ${key(b)} is not visibly reachable from any verified platform`]))
        break
      }
      if(isAir(api.block(b.x,b.y,b.z)?.name))continue
      if (!await attempt(api, 'dig', { x: b.x, y: b.y, z: b.z, batch: true }, report)) break
      const after = api.block(b.x, b.y, b.z)
      if (!after || !isAir(after.name)) { report.attention.push(`dig left ${key(b)} standing or unloaded`); break }
      report.harvested = tree.blocks.filter(p=>isAir(api.block(p.x,p.y,p.z)?.name)).length
      report.remaining = report.remaining.filter(p => key(p) !== key(b))
      api.report?.(report)
    }
    if(scaffold?.kind==='pillar'){
      await cleanupScaffoldRecoverably(api,cleanupScaffold,scaffold.id,report)
      if(report.cleanup_left?.length){refreshRemaining(scaffold);return finish()}
    }
    report.harvested = tree.blocks.filter(p=>isAir(api.block(p.x,p.y,p.z)?.name)).length
    report.remaining = tree.blocks.filter(b => api.block(b.x, b.y, b.z)?.name === b.name).map(b => ({ x: b.x, y: b.y, z: b.z, name: b.name }))
    if (report.remaining.length && !report.attention.length) report.attention.push(`${report.remaining.length} tree blocks remain; harvest incomplete`)
    const originalStatus = forestry?.wood.map(p => api.block(p.x, p.y, p.z)) ?? []
    const originalRemain = forestry?.wood.filter((p, i) => originalStatus[i]?.name === p.name) ?? []
    const originalObserved = originalStatus.every(b => b && isAir(b.name))
    if (forestry && !report.attention.length && !report.remaining.length && !originalRemain.length && originalObserved && !tree.blocks.some(p => !isAir(api.block(p.x, p.y, p.z)?.name))) {
      forestry = { ...forestry, phase: 'decaying', readyAt: Date.now() + 120000 }
      api.forestry?.(journalId, forestry)
    }
    if(!pillar&&scaffold&&scaffold.kind!=='pillar'&&!report.harvested&&report.attention.some(s=>/no walkable path|no first move|did not reach|no verified tree work platform|not visibly reachable|work platform .* refused centering/.test(s))){
      await cleanupScaffoldRecoverably(api,cleanupScaffold,scaffold.id,report)
      if(!report.cleanup_left?.length){
        const zones=(await api.act('zones')).zones??[]
        const alternate=planScaffoldAccess(api,tree,harvestStands(tree,api.block),zones,true)
        if(!alternate.attention.length)return await runTree(api,a,action,{pillar:true,allowForestHives})
      }
    }
    if(report.harvested&&!report.remaining.length)report.decay_wait=120
    if (report.harvested) await attempt(api, 'collect', { range: Math.min(16, tree.profile.radius + 2) }, report)
    return finish()
    } catch (e) {
      interrupted = true
      refreshRemaining(scaffold)
      api.report?.(report)
      if (scaffold) { report.attention.push('tree work interrupted; scaffold cleanup remains recorded'); treeAttention(api, 'tree.harvest', report) }
      throw e
    } finally {
      if (scaffold && !interrupted) { await cleanupScaffoldRecoverably(api, cleanupScaffold, scaffold.id, report); treeAttention(api, 'tree.harvest', report) }
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
