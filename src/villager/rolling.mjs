import { WORKSTATION_CLAIM_BASIS } from './population.mjs'
import { verifyCrowdedClaimants, verifyWorkstationEnclosure } from './claim.mjs'
import { chooseVillagerCapture, makeVillagerCapture } from './capture.mjs'
import { villagerObservation } from './observation.mjs'
import { parseWant, rollVerdict, rollRefusal, tradeLine, inAnyZone, workRefusal, cheapestLockOffer, matchesVillagerOutput, JOB_BLOCK_PROFESSION } from '../lib.mjs'

const distance = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)
const professionOf = raw => villagerObservation({ metadata: raw }).profession

export default {
  doc: 'villager.roll x= y= z= [block=lectern] [want=efficiency:3 | output=arrow [enchant=sharpness level=3]] [uuid=] [id=] [pen=true] [penBlock=cobblestone] [maxPrice=64] [tries=40] [buy=false]: reroll one villager at a workstation until its offers match; optionally buy the cheapest affordable offer to lock the job; proveStation=true requires an observed unemployed target and records a sole-station causal claim',
  stops: 'a matching offer is found, tries run out, night falls, or the villager leaves',
  args: { want: 'string', output: 'string', enchant: 'string', level: 'number', atLeast: 'boolean', uuid: 'string', id: 'number', block: 'string', pen: 'boolean', penBlock: 'string', maxPrice: 'number', tries: 'number', buy: 'boolean', proveStation:'boolean',claimHabitat:'any', x: 'number!', y: 'number!', z: 'number!' },

  async run (api, a) {
    if (a.want && (a.output || a.enchant || a.level !== undefined)) throw new Error('use want= for an enchanted book or output= with optional enchant=/level=; do not combine selectors')
    if (a.want && a.block && a.block !== 'lectern') throw new Error('want= enchanted books is only supported with block=lectern')
    const block = a.block ?? (a.want ? 'lectern' : null)
    if (!block || !JOB_BLOCK_PROFESSION[block]) throw new Error(`block= must be a supported villager workstation${block ? `; unsupported ${block}` : ''}`)
    if (a.output && !/^[a-z0-9_:-]+$/i.test(a.output)) throw new Error('output= must be an item name such as arrow')
    const wants = a.want ? parseWant(a.want) : []
    if (typeof wants === 'string') throw new Error(wants)
    if ((a.enchant || a.level !== undefined || a.atLeast) && !a.enchant) throw new Error('level= and atLeast= require enchant=')
    if (a.atLeast && a.level === undefined) throw new Error('atLeast=true requires level=')
    if (a.level !== undefined && (!Number.isInteger(a.level) || a.level < 1 || a.level > 255)) throw new Error('level= must be an integer from 1 to 255')
    const enchant = a.enchant ? parseWant(`${a.enchant}${a.level !== undefined ? `:${a.level}${a.atLeast ? '+' : ''}` : ''}`) : []
    if (typeof enchant === 'string') throw new Error(enchant)
    const selector = { wants, output: a.output, enchant: enchant[0]?.enchant, level: enchant[0]?.level, atLeast: Boolean(enchant[0]?.atLeast) }
    const maxPrice = a.maxPrice ?? 64
    const tries = a.tries ?? 40
    if (!Number.isInteger(tries) || tries < 1 || tries > 200 || !Number.isInteger(maxPrice) || maxPrice < 1 || maxPrice > 64) throw new Error('tries= must be 1..200 and maxPrice= must be 1..64')
    if (a.id !== undefined && a.uuid) throw new Error('use either uuid= or id=, not both')
    const cell = { x: a.x, y: a.y, z: a.z }
    const owner = api.places().filter(p => distance(p, cell) <= (p.radius ?? 8)).map(p => workRefusal(p, api.me())).find(Boolean)
    if (owner) throw new Error(owner)
    const foreignZone = api.zones().find(z => inAnyZone([z], cell) && !new RegExp(`^(${api.me().toLowerCase()}|starter)-`).test(z.name.toLowerCase()))
    if (foreignZone) throw new Error(`${cell.x},${cell.y},${cell.z} is in protected zone ${foreignZone.name}`)

    await api.act('goto', { ...cell, range: 5 })

    const entities = await api.act('entity', { name: 'villager', uuid: true, count: 100 })
    const visible = entities.found.map(e => ({ ...e, pos: villagerObservation(e).position, raw: villagerObservation(e).metadata })).filter(e => e.pos)
    const nearby = visible.filter(e => distance(e.pos, cell) <= 8)
    const freshIds = new Set(nearby.filter(e => villagerObservation(e).baby !== true && professionOf(e.raw) === 'unemployed').map(e => e.id))
    // A named villager can wander while the pen is built. Keep the ordinary
    // eight-block isolation radius, but let a pen wait for that exact villager
    // from farther away instead of refusing before construction starts.
    const namedRange = a.pen === false ? 8 : 16
    const target = a.uuid ? visible.find(e => e.uuid === a.uuid && distance(e.pos, cell) <= namedRange) : a.id !== undefined ? visible.find(e => e.id === a.id && distance(e.pos, cell) <= namedRange) : nearby
      .filter(e => freshIds.has(e.id))
      .sort((a, b) => distance(a.pos, cell) - distance(b.pos, cell))[0]
    if ((a.id !== undefined || a.uuid) && !target) throw new Error(`villager ${a.uuid ?? a.id} is not near ${a.x},${a.y},${a.z}`)
    if (!target) throw new Error(`no adult unemployed villager near ${a.x},${a.y},${a.z}; use uuid= to inspect an exact villager`)
    const placed = api.block(a.x, a.y, a.z)?.name === block
    const blocks = (await api.act('find_blocks', { block, maxDistance: 24, count: 100 })).positions ?? []
    const spareBlocks = blocks.filter(p => distance(p, cell) <= 16 && (p.x !== a.x || p.y !== a.y || p.z !== a.z)).length
    const job = JOB_BLOCK_PROFESSION[block]
    const crowded = a.pen === false && nearby.length > 1
    const verifyClaimants = async () => {
      if(a.proveStation===true)await verifyWorkstationEnclosure(api,{uuid:a.uuid??target.uuid,cell,block,habitat:a.claimHabitat,allowAbsent:api.block(cell.x,cell.y,cell.z)?.name!==block})
      if (a.pen !== false) return
      const rows = (await api.act('entity', { name: 'villager', uuid: true, count: 100 })).found
      if (rows.filter(e => { const p = villagerObservation(e).position; return p && distance(p, cell) <= 16 }).length > 1) await verifyCrowdedClaimants(api, { uuid: a.uuid, cell, block })
    }
    await verifyClaimants()
    const refusal = rollRefusal({ villagers: nearby.length || ((a.id !== undefined || a.uuid) && a.pen !== false ? 1 : 0), allowCrowd: a.pen !== false || crowded, profession: professionOf(target?.raw), adult: villagerObservation(target).baby !== true, nitwit: professionOf(target?.raw) === 'nitwit', day: api.clock().day, carried: api.inv(), placed, block, spareBlocks, buy: a.buy === true && block === 'lectern', maxPrice })
    if (refusal) throw new Error(refusal)
    const reserve = tries > 1 ? (placed ? 1 : 2) : 0
    if ((api.inv()[block] ?? 0) < reserve) throw new Error(`rerolling needs ${reserve} carried ${block}${reserve === 1 ? '' : 's'} so a spare can restore the job block if its drop stays inside the pen`)
    const { pen, penBlock } = a.pen !== false ? chooseVillagerCapture(api, { a, cell, target, nearby, tries }) : { pen: null, penBlock: a.penBlock ?? 'cobblestone' }
    let id = target.id
    const trackedUuid = a.uuid ?? target.uuid
    let claimPending=false, workstationClaim=null
    let rounds = 0
    let best = null
    let lastLevel = null
    let missingBlock = false
    let exitError = null
    const report = () => api.report({ rounds, seen: seen ?? 'none', best: best ? `${best.enchant} ${best.level} at ${best.price}` : null })
    let seen = 'none'
    api.report({ rounds: 0, seen, best: null, found: null, locked: false })
    const present = () => api.block(a.x, a.y, a.z)?.name === block
    const refreshTarget = async () => {
      const current = (await api.act('entity', { name: 'villager', uuid: true, count: 100 })).found.find(e => trackedUuid ? e.uuid === trackedUuid : e.id === id)
      if (!current) throw new Error(`villager ${trackedUuid ?? id} is no longer visible; stopping without changing its workstation`)
      const p = current.exact.split(',').map(Number)
      if (distance({ x: p[0], y: p[1], z: p[2] }, cell) > 8) throw new Error(`villager ${trackedUuid ?? id} left the workstation area`)
      id = current.id
      return current
    }
    const readTrades = async () => { await refreshTarget(); return api.act('trades', { id, uuid: trackedUuid, ...cell }) }
    const status = async () => (await readTrades()).profession
    const ensureBlock = async (cleanup = false) => {
      if (present()) { missingBlock = false; return }
      if (!cleanup) await verifyClaimants()
      await (cleanup && api.cleanupAct ? api.cleanupAct('place', { item: block, ...cell }) : api.act('place', { item: block, ...cell }))
      if (!present()) throw new Error(`could not restore ${block} at ${a.x},${a.y},${a.z}`)
      missingBlock = false
    }
    const capture = pen ? makeVillagerCapture(api, { a, cell, pen, penBlock, trackedUuid, freshIds, job, initialId: id }) : null
    const serviceSills = capture?.serviceSills ?? []
    if (professionOf(target.raw) === job) {
      if (!placed) throw new Error(`villager ${trackedUuid ?? id} already has profession ${job} but ${block} is not at the requested cell; inspect its current workstation before changing anything`)
      const current = await readTrades()
      const wantedNow = wants.length ? rollVerdict(current.offers, wants, maxPrice).found : current.offers.some(o => matchesVillagerOutput(o, selector))
      if (!wantedNow && (Number(current.level ?? 1) > 1 || current.offers.some(o => (o.nbTradeUses ?? 0) > 0))) throw new Error(`villager ${trackedUuid ?? id} has traded as ${job}; its profession is locked and this command will not break ${block}`)
    }
    try {
      if(a.proveStation===true){
        const initial=await readTrades()
        if(Number(initial.level)>1||initial.offers.some(o=>Number(o.nbTradeUses)>0))throw new Error('workstation association is unknown for this locked trader; no workstation will be removed')
        await verifyClaimants()
        if(initial.profession!=='unemployed')throw new Error('workstation association cannot be safely confirmed for an already-employed trader; only a positively observed unemployed UUID may claim this workspace')
        claimPending=true
      }
      if (capture) id = await capture.capture(ensureBlock)
      else if (!present()) await ensureBlock()
      for (rounds = 1; rounds <= tries; rounds++) {
        if (!api.clock().day) return { rounds: rounds - 1, found: 'none', best: best ? `${best.enchant} ${best.level} at ${best.price}` : null, stopped: `night fell; ${block} left placed` }
        await refreshTarget()
        await api.until(async () => (await status()) === job, { timeout: 60, every: 2, what: `the villager did not claim the ${block}` })
        if(claimPending){
          await verifyClaimants()
          if(!present()||(await status())!==job)throw new Error('workstation claim changed before confirmation')
          workstationClaim={...cell,block,profession:job,observedAt:new Date().toISOString(),basis:WORKSTATION_CLAIM_BASIS}
          claimPending=false
        }
        const read = await readTrades()
        lastLevel = Number.isFinite(Number(read.level)) ? Number(read.level) : null
        const verdict = wants.length ? rollVerdict(read.offers, wants, maxPrice) : { found: read.offers.map((offer, n) => matchesVillagerOutput(offer, selector) ? { offer, index: offer.index ?? n + 1 } : null).find(Boolean) ?? null, best: null }
        if (verdict.best && (!best || verdict.best.price < best.price)) best = verdict.best
        seen = read.offers.map((o, n) => tradeLine(o, n + 1)).find((s, n) => wants.length ? s.includes('enchanted_book') : matchesVillagerOutput(read.offers[n], selector)) ?? 'none'
        report()
        api.note(`round=${rounds} seen=${seen} best=${best ? `${best.enchant} ${best.level} at ${best.price}` : 'none'}`)
        if (verdict.found) {
          const found = verdict.found
          let locked = false
          let boughtOffer = null
          if (a.buy === true) {
            const cheapest = cheapestLockOffer(read.offers, api.inv())
            if (!cheapest) throw new Error(wants.length ? 'found the book, but no offer is affordable to lock the villager' : `found the requested offer, but no villager trade is affordable to lock ${job}`)
            boughtOffer = cheapest.index
            await refreshTarget()
            await verifyClaimants()
            await refreshTarget()
            await api.act('trade', { id, uuid: trackedUuid, ...cell, offer: boughtOffer, times: 1 })
            const after = await readTrades()
            const stillMatches = after.offers.some(o => matchesVillagerOutput(o, selector, { includeDisabled: true }))
            locked = after.profession === job && stillMatches
            if (!locked) throw new Error(`trade completed but the requested ${wants.length ? 'book' : 'output'} was not confirmed afterwards for UUID ${trackedUuid ?? 'unknown'}`)
          }
          return { rounds, uuid: trackedUuid, profession: job,...(workstationClaim?{workstationClaim}:{}), found: wants.length ? `${found.enchant} ${found.level}` : found.offer.outputItem?.name ?? 'matching offer', ...(wants.length ? { price: found.price, offer: found.index } : { offer: found.index }), ...(boughtOffer ? { boughtOffer } : {}), locked }
        }
        if (rounds === tries) break
        if ((api.inv()[block] ?? 0) < 1) return { rounds, uuid: trackedUuid, profession: job, level: lastLevel, found: 'none', best: best ? `${best.enchant} ${best.level} at ${best.price}` : null, locked: false, stopped: `out of spare ${block}; current job block left placed` }
        if (pen) await api.act('goto', { ...pen.serviceStand, range: 0 })
        const lecternsBefore = api.inv()[block] ?? 0
        const beforeReroll = await readTrades()
        if (Number(beforeReroll.level ?? 1) > 1 || beforeReroll.offers.some(o => Number(o.nbTradeUses) > 0)) throw new Error('this exact villager is trade-locked; its workstation will not be removed')
        await verifyClaimants()
        await refreshTarget()
        missingBlock = true
        await api.act('dig', cell)
        if (pen && (api.inv()[block] ?? 0) <= lecternsBefore) {
          capture.openService()
          for (const p of [...serviceSills].reverse()) await api.act('dig', p)
          const nook = serviceSills[0]
          await api.act('goto', { ...nook, range: 0 })
          await api.until(() => (api.inv()[block] ?? 0) > lecternsBefore, { timeout: 5, every: 0.25, what: 'the lectern drop stayed beyond pickup reach' }).catch(() => {})
          if ((api.inv()[block] ?? 0) <= lecternsBefore) api.note(`lectern drop at ${cell.x},${cell.y},${cell.z} was not recovered; using a spare`)
          await capture.sealService()
        }
        await api.until(async () => (await status()) === 'unemployed', { timeout: 20, every: 2, what: `this villager is locked already: it kept ${job} with no ${block}; use a fresh one` })
        claimPending=a.proveStation===true
        await ensureBlock()
        await api.checkpoint()
      }
      return { rounds: tries, uuid: trackedUuid, profession: job, level: lastLevel, found: 'none', best: best ? `${best.enchant} ${best.level} at ${best.price}` : null, locked: false, stopped: `no matching result in ${tries} offer set${lastLevel == null ? '' : ` at villager level ${lastLevel}`}; this command rerolls jobs but does not level villagers, so a higher-tier result must be unlocked separately` }
    } catch (error) {
      exitError = error
      if (/no villager|villager walked off/.test(error.message)) return { rounds: Math.max(0, rounds - 1), found: 'none', best: best ? `${best.enchant} ${best.level} at ${best.price}` : null, stopped: `villager ${id} left the block` }
      throw error
    } finally {
      let serviceError = null
      if (capture?.serviceOpen) {
        try { await capture.sealService(true) }
        catch (error) {
          serviceError = error
          api.report({ servicePending: serviceSills.filter(p => api.block(p.x, p.y, p.z)?.name !== penBlock).map(p => `${p.x},${p.y},${p.z}`).join(' '), serviceError: error.message })
        }
      }
      if (missingBlock && !present()) {
        try { await ensureBlock(true) }
        catch (error) {
          const where = `${cell.x},${cell.y},${cell.z}`
          api.report({ restorationPending: where, restorationError: error.message })
          throw new Error(`${exitError ? `${exitError.message}; ` : ''}${serviceError ? `service route restoration pending: ${serviceError.message}; ` : ''}${block} restoration pending at ${where}: ${error.message}`)
        }
      }
      if (serviceError) throw new Error(`${exitError ? `${exitError.message}; ` : ''}service route restoration pending: ${serviceError.message}`)
    }
  }
}
