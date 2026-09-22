// Gather seed stock, renewable ways only: grass for wheat seed, the top of a wild stand for cane and bamboo, a farm's
// surplus chest for carrots, potatoes and beetroot, a wild patch for melon and pumpkin. Never the last plant of a stand.
import { planStructure, seedSource } from '../../src/lib.mjs'

const ROUNDS = 8
const RANGE = 64
const CLUMPS = 12

const held = (api, item) => api.inv()[item] ?? 0

// grass drops a seed about one clump in eight, so this breaks whatever it can find and sweeps the drops up.
// (one `dig` at a time, not `mine.get`: a wildcard block name and a count are the wrong shape for weeding a patch.)
async function breakGrass (api, source, range) {
  const { positions = [] } = await api.act('find_blocks', { block: source.block, maxDistance: range, count: CLUMPS })
  if (!positions.length) return false
  for (const p of positions) await api.act('dig', { x: p.x, y: p.y, z: p.z }).catch(() => ({}))
  await api.act('collect', {})
  return true
}

// cut the top of the nearest wild stand: `harvest` takes the second segment and leaves the base to grow back
async function cutStand (api, source, range) {
  const { positions = [] } = await api.act('find_blocks', { block: source.block, maxDistance: range, count: 8 })
  const stand = positions[0]
  if (!stand) return false
  await api.act('goto', { x: stand.x, y: stand.y, z: stand.z, range: 3 })
  await api.act('farm.harvest', { within: 6 })
  return true
}

export default {
  doc: 'farm.get_seeds crop= count= [place=] [range=64]: gather seed the renewable way (grass for wheat, top-cut wild stands, a farm chest for roots)',
  stops: 'count= reached, or two rounds that bring nothing back',
  args: { crop: 'string!', count: 'number', place: 'string', range: 'number' },

  async run (api, a) {
    const source = seedSource(a.crop)
    if (!source) throw new Error(`no renewable way to get ${a.crop} seed: the crops with one are wheat, carrot, potato, beetroot, sugar_cane, bamboo, melon and pumpkin`)
    const want = a.count ?? 32
    const range = a.range ?? RANGE
    const had = held(api, source.item)

    if (source.from === 'chest') {
      if (!a.place) throw new Error(`${a.crop} does not grow from nothing: take some from a farm's surplus chest with place=<a marked farm whose plan has a C cell>, or trade for it`)
      const cell = planStructure(api.plan(a.place).cells, 'C')
      if (!cell) throw new Error(`${a.place} has no C (chest) cell in its plan`)
      await api.act('withdraw', { items: { [source.item]: want }, x: cell.x, y: cell.y, z: cell.z })
      return { crop: a.crop, item: source.item, got: held(api, source.item) - had, from: source.from }
    }

    const round = source.from === 'stalk'
      ? () => cutStand(api, source, range)
      : () => breakGrass(api, source, range)

    // `mine` answers ok with got=0 when there is nothing it can reach, so a round counts as barren unless the seed
    // really came in: two of those in a row and there is no more to take from where I stand
    let barren = 0
    for (let n = 0; n < ROUNDS && held(api, source.item) - had < want && barren < 2; n++) {
      const before = held(api, source.item)
      const found = await round()
      barren = found && held(api, source.item) > before ? 0 : barren + 1
      api.report({ crop: a.crop, item: source.item, got: held(api, source.item) - had })
      await api.checkpoint({ done: held(api, source.item) - had })
    }
    const got = held(api, source.item) - had
    return {
      crop: a.crop,
      item: source.item,
      got,
      from: source.from,
      gaveUp: barren >= 2 ? `nothing more to take within ${range} blocks: walk somewhere else and run it again` : undefined,
      short: got < want && barren < 2 ? `${want - got} short after ${ROUNDS} rounds` : undefined
    }
  }
}
