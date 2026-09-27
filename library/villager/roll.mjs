import { parseWant, rollVerdict, rollRefusal, tradeLine, inAnyZone, workRefusal, cheapestLockOffer, matchesVillagerOutput, JOB_BLOCK_PROFESSION, villagerPenPlan } from '../../src/lib.mjs'

const distance = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)
const professionOf = raw => {
  const data = raw?.[19] ?? raw?.[18]
  const names = 'unemployed armorer butcher cartographer cleric farmer fisherman fletcher leatherworker librarian mason nitwit shepherd toolsmith weaponsmith'.split(' ')
  return typeof data?.profession === 'string' ? data.profession.replace(/^minecraft:/, '') : names[data?.villagerProfession ?? data?.profession ?? data?.[1] ?? 0] ?? 'unknown'
}

export default {
  doc: 'villager.roll x= y= z= [block=lectern] [want=efficiency:3 | output=arrow [enchant=sharpness level=3]] [uuid=] [id=] [pen=true] [penBlock=cobblestone] [maxPrice=64] [tries=40] [buy=false]: reroll one villager at a workstation until its offers match; optionally buy the cheapest affordable offer to lock the job',
  stops: 'a matching offer is found, tries run out, night falls, or the villager leaves',
  args: { want: 'string', output: 'string', enchant: 'string', level: 'number', atLeast: 'boolean', uuid: 'string', id: 'number', block: 'string', pen: 'boolean', penBlock: 'string', maxPrice: 'number', tries: 'number', buy: 'boolean', x: 'number!', y: 'number!', z: 'number!' },

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
    const visible = entities.found.map(e => ({ ...e, pos: Object.fromEntries(['x', 'y', 'z'].map((k, i) => [k, Number(e.exact.split(',')[i])])), raw: JSON.parse(e.metadata) }))
    const nearby = visible.filter(e => distance(e.pos, cell) <= 8)
    const freshIds = new Set(nearby.filter(e => e.raw?.[16] !== true && professionOf(e.raw) === 'unemployed').map(e => e.id))
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
    const refusal = rollRefusal({ villagers: nearby.length || ((a.id !== undefined || a.uuid) && a.pen !== false ? 1 : 0), allowCrowd: a.pen !== false, profession: professionOf(target?.raw), adult: target?.raw?.[16] !== true, nitwit: professionOf(target?.raw) === 'nitwit', day: api.clock().day, carried: api.inv(), placed, block, spareBlocks, buy: a.buy === true && block === 'lectern', maxPrice })
    if (refusal) throw new Error(refusal)
    const reserve = tries > 1 ? (placed ? 1 : 2) : 0
    if ((api.inv()[block] ?? 0) < reserve) throw new Error(`rerolling needs ${reserve} carried ${block}${reserve === 1 ? '' : 's'} so a spare can restore the job block if its drop stays inside the pen`)
    const penBlock = a.penBlock ?? 'cobblestone'
    const penIssue = pen => {
      const passable = name => !name || ['air', 'short_grass', 'tall_grass', 'grass', 'fern', 'large_fern', 'snow'].includes(name)
      const allCells = [cell, ...pen.floor]
      const badFloor = allCells.find(p => !api.block(p.x, p.y - 1, p.z)?.solid)
      if (badFloor) return `pen needs flat solid ground under ${badFloor.x},${badFloor.y},${badFloor.z}`
      const structure = [...pen.walls, ...pen.guard, ...pen.service]
      const badWall = structure.find(p => { const name = api.block(p.x, p.y, p.z)?.name; return name !== penBlock && !passable(name) })
      if (badWall) return `pen wall at ${badWall.x},${badWall.y},${badWall.z} is occupied by ${api.block(badWall.x, badWall.y, badWall.z)?.name}`
      const standing = [...pen.interior, pen.gate, pen.outside, pen.serviceStand]
      const blocked = standing.flatMap(p => [p, { ...p, y: p.y + 1 }]).find(p => !passable(api.block(p.x, p.y, p.z)?.name) && !(p.x === pen.gate.x && p.z === pen.gate.z && api.block(p.x, p.y, p.z)?.name === penBlock))
      if (blocked) return `pen walking route is blocked at ${blocked.x},${blocked.y},${blocked.z} by ${api.block(blocked.x, blocked.y, blocked.z)?.name}`
      const gateCells = [pen.gate, { ...pen.gate, y: pen.gate.y + 1 }, { ...pen.gate, y: pen.gate.y + 2 }]
      // The top gate block is a permanent lintel. Only the two blocks in the
      // walking opening can trap a villager or prevent a fresh capture.
      if (gateCells.slice(0, 2).some(p => api.block(p.x, p.y, p.z)?.name === penBlock) && !nearby.some(e => pen.interior.some(p => Math.floor(e.pos.x) === p.x && Math.floor(e.pos.y) === p.y && Math.floor(e.pos.z) === p.z) && (a.id === undefined || e.id === a.id))) return 'pen entry is already blocked with no villager inside: open it before luring'
      const needed = [...structure, ...gateCells].filter(p => api.block(p.x, p.y, p.z)?.name !== penBlock).length
      const reservePen = tries > 1 ? 2 : 0
      if ((api.inv()[penBlock] ?? 0) < needed + reservePen) return `pen needs ${needed} ${penBlock} to build and ${reservePen} spare to reseal its service route: carrying ${api.inv()[penBlock] ?? 0}`
      if ([...structure, ...gateCells].some(p => api.zones().some(z => inAnyZone([z], p) && !new RegExp(`^(${api.me().toLowerCase()}|starter)-`).test(z.name.toLowerCase())))) return 'pen wall crosses somebody else\'s protected zone'
      return null
    }
    const directions = [{ x: 1, z: 0 }, { x: -1, z: 0 }, { x: 0, z: 1 }, { x: 0, z: -1 }]
    const orientations = directions.map(d => {
      const plan = villagerPenPlan(cell, { x: cell.x + d.x, z: cell.z + d.z })
      return { plan, score: plan.walls.filter(p => api.block(p.x, p.y, p.z)?.name === penBlock).length, issue: penIssue(plan) }
    })
    const existing = [...orientations].sort((a, b) => b.score - a.score)[0]
    const preferred = [...orientations].sort((a, b) => distance(a.plan.outside, target.pos) - distance(b.plan.outside, target.pos))
    const chosen = existing.score >= 10 ? existing : preferred.find(p => !p.issue) ?? preferred[0]
    const pen = a.pen !== false ? chosen.plan : null
    let id = target.id
    const trackedUuid = a.uuid ?? target.uuid
    if (pen && chosen.issue) throw new Error(chosen.issue)
    let rounds = 0
    let best = null
    let lastLevel = null
    let missingBlock = false
    let serviceOpen = false
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
    const readTrades = async () => { await refreshTarget(); return api.act('trades', { id, ...cell }) }
    const status = async () => (await readTrades()).profession
    const ensureBlock = async (cleanup = false) => {
      if (present()) { missingBlock = false; return }
      await (cleanup && api.cleanupAct ? api.cleanupAct('place', { item: block, ...cell }) : api.act('place', { item: block, ...cell }))
      if (!present()) throw new Error(`could not restore ${block} at ${a.x},${a.y},${a.z}`)
      missingBlock = false
    }
    const serviceSills = pen?.service.filter(p => p.y === cell.y) ?? []
    const sealService = async (cleanup = false) => {
      if (!serviceOpen) return
      const act = cleanup && api.cleanupAct ? api.cleanupAct : api.act
      await act('goto', { ...pen.serviceStand, range: 0 })
      for (const p of serviceSills) {
        if (api.block(p.x, p.y, p.z)?.name !== penBlock) await act('place', { ...p, item: penBlock })
      }
      const hole = serviceSills.find(p => api.block(p.x, p.y, p.z)?.name !== penBlock)
      if (hole) throw new Error(`service route still open at ${hole.x},${hole.y},${hole.z}`)
      serviceOpen = false
    }
    if (professionOf(target.raw) === job) {
      if (!placed) throw new Error(`villager ${trackedUuid ?? id} already has profession ${job} but ${block} is not at the requested cell; inspect its current workstation before changing anything`)
      const current = await readTrades()
      const wantedNow = wants.length ? rollVerdict(current.offers, wants, maxPrice).found : current.offers.some(o => matchesVillagerOutput(o, selector))
      if (!wantedNow && (Number(current.level ?? 1) > 1 || current.offers.some(o => (o.nbTradeUses ?? 0) > 0))) throw new Error(`villager ${trackedUuid ?? id} has traded as ${job}; its profession is locked and this command will not break ${block}`)
    }
    try {
      if (pen) {
        const walls = [...pen.walls, ...pen.guard].filter(p => api.block(p.x, p.y, p.z)?.name !== penBlock).map(p => ({ ...p, item: penBlock }))
        if (walls.length) await api.act('place', { blocks: walls })
        for (const p of [...pen.walls, ...pen.guard]) {
          if (api.block(p.x, p.y, p.z)?.name !== penBlock) await api.act('place', { ...p, item: penBlock })
        }
        let hole = [...pen.walls, ...pen.guard].find(p => api.block(p.x, p.y, p.z)?.name !== penBlock)
        if (hole) throw new Error(`pen wall still open at ${hole.x},${hole.y},${hole.z}`)
        await api.act('goto', { ...pen.serviceStand, range: 0 })
        // The high outside lintel is out of reach from the first wall-building stance.
        // Build it from the service side after the lectern roof provides an attachment.
        for (const p of pen.service) {
          if (api.block(p.x, p.y, p.z)?.name !== penBlock) await api.act('place', { ...p, item: penBlock })
        }
        hole = pen.service.find(p => api.block(p.x, p.y, p.z)?.name !== penBlock)
        if (hole) throw new Error(`pen service wall still open at ${hole.x},${hole.y},${hole.z}`)
        api.report({ pen: 'entrance open', penAt: `${pen.center.x},${pen.center.y},${pen.center.z}` })
      }
      if (!placed) await ensureBlock()
      if (pen) {
        await api.act('goto', { ...pen.outside, range: 1 })
        const inside = async () => {
          const found = (await api.act('entity', { name: 'villager', uuid: true, count: 100 })).found.find(e => {
            if (trackedUuid && e.uuid !== trackedUuid) return false
            const p = e.exact.split(',').map(Number)
            // Wait for the back cell. A villager at the front can still have its
            // hitbox inside the entrance even though its feet floor to "inside".
            if (Math.floor(p[0]) !== pen.center.x || Math.floor(p[1]) !== pen.center.y || Math.floor(p[2]) !== pen.center.z) return false
            if (a.id !== undefined) return e.id === a.id
            const raw = JSON.parse(e.metadata)
            return (freshIds.has(e.id) || (trackedUuid && e.uuid === trackedUuid)) && raw?.[16] !== true && ['unemployed', job].includes(professionOf(raw))
          })
          if (found) id = found.id
          return Boolean(found)
        }
        const top = { ...pen.gate, y: pen.gate.y + 1 }
        const cap = { ...pen.gate, y: pen.gate.y + 2 }
        if (api.block(cap.x, cap.y, cap.z)?.name !== penBlock) await api.act('place', { ...cap, item: penBlock })
        if (api.block(cap.x, cap.y, cap.z)?.name !== penBlock) throw new Error('pen entry roof did not close')
        for (let attempt = 0; attempt < 3; attempt++) {
          await api.until(inside, { timeout: 60, every: 0.25, what: 'the villager did not enter the pen through its open side' })
          try { await api.act('place', { ...top, item: penBlock }) }
          catch (error) {
            if (!/server refused|stand in it/.test(error.message) || attempt === 2) throw error
            await api.pause(0.5)
            continue
          }
          if (await inside()) break
          if (api.block(top.x, top.y, top.z)?.name === penBlock) await api.act('dig', top)
          if (attempt === 2) throw new Error('villager left the pen before the entry could be closed')
        }
        await api.act('place', { ...pen.gate, item: penBlock })
        if ([pen.gate, top, cap].some(p => api.block(p.x, p.y, p.z)?.name !== penBlock)) throw new Error('pen entry did not close')
        const held = (await api.act('entity', { name: 'villager', uuid: true, count: 100 })).found.some(e => {
          if (trackedUuid ? e.uuid !== trackedUuid : e.id !== id) return false
          const p = e.exact.split(',').map(Number)
          return pen.interior.some(c => Math.floor(p[0]) === c.x && Math.floor(p[1]) === c.y && Math.floor(p[2]) === c.z)
        })
        if (!held) throw new Error(`pen entry closed but villager ${id} is not inside`)
        api.report({ pen: 'closed', capturedId: id })
      }
      for (rounds = 1; rounds <= tries; rounds++) {
        if (!api.clock().day) return { rounds: rounds - 1, found: 'none', best: best ? `${best.enchant} ${best.level} at ${best.price}` : null, stopped: `night fell; ${block} left placed` }
        await refreshTarget()
        await api.until(async () => (await status()) === job, { timeout: 60, every: 2, what: `the villager did not claim the ${block}` })
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
            await api.act('trade', { id, ...cell, offer: boughtOffer, times: 1 })
            const after = await readTrades()
            const stillMatches = after.offers.some(o => matchesVillagerOutput(o, selector, { includeDisabled: true }))
            locked = after.profession === job && stillMatches
            if (!locked) throw new Error(`trade completed but the requested ${wants.length ? 'book' : 'output'} was not confirmed afterwards for UUID ${trackedUuid ?? 'unknown'}`)
          }
          return { rounds, uuid: trackedUuid, profession: job, found: wants.length ? `${found.enchant} ${found.level}` : found.offer.outputItem?.name ?? 'matching offer', ...(wants.length ? { price: found.price, offer: found.index } : { offer: found.index }), ...(boughtOffer ? { boughtOffer } : {}), locked }
        }
        if (rounds === tries) break
        if ((api.inv()[block] ?? 0) < 1) return { rounds, uuid: trackedUuid, profession: job, level: lastLevel, found: 'none', best: best ? `${best.enchant} ${best.level} at ${best.price}` : null, locked: false, stopped: `out of spare ${block}; current job block left placed` }
        if (pen) await api.act('goto', { ...pen.serviceStand, range: 0 })
        const lecternsBefore = api.inv()[block] ?? 0
        missingBlock = true
        await api.act('dig', cell)
        if (pen && (api.inv()[block] ?? 0) <= lecternsBefore) {
          serviceOpen = true
          for (const p of [...serviceSills].reverse()) await api.act('dig', p)
          const nook = serviceSills[0]
          await api.act('goto', { ...nook, range: 0 })
          await api.until(() => (api.inv()[block] ?? 0) > lecternsBefore, { timeout: 5, every: 0.25, what: 'the lectern drop stayed beyond pickup reach' }).catch(() => {})
          if ((api.inv()[block] ?? 0) <= lecternsBefore) api.note(`lectern drop at ${cell.x},${cell.y},${cell.z} was not recovered; using a spare`)
          await sealService()
        }
        await api.until(async () => (await status()) === 'unemployed', { timeout: 20, every: 2, what: `this villager is locked already: it kept ${job} with no ${block}; use a fresh one` })
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
      if (serviceOpen) {
        try { await sealService(true) }
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
