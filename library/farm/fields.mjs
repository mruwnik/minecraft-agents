// A cheap census of every saved plan in range, read from the map in memory without walking anywhere: is it worth a sweep?
// counts=false in the rendering, so a census reads as ripe=0 dry=1 rather than a bare `dry` for every count of one.
import { fieldCensus, planAnchor, planCells, compact } from '../../src/lib.mjs'
import { clutterBlocks, clutterLine } from './shared/clutter.mjs'

const RANGE = 48

export default {
  doc: 'farm.fields [place=] [range=48]: what every saved plan near me looks like right now: crops, ripe, growing, empty, untilled, dry, clutter',
  stops: 'nothing: it only looks, and never moves',
  instant: true,
  args: { place: 'string', range: 'number' },

  async run (api, a) {
    const here = api.pos()
    const range = a.range ?? RANGE
    const plans = api.places().filter(p => p.plan && (a.place ? p.name === a.place : Math.hypot(p.x - here.x, p.z - here.z) <= range))
    if (!plans.length) {
      throw new Error(a.place
        ? `no plan called ${a.place}: ./mc places kind=farm lists the ones there are`
        : `no farm plan within ${range} blocks: save one with ./mc farm.plan`)
    }
    // a plan anchored a block off reads as a field of empty, untilled beds, so the census is taken where the crops
    // really stand and the plan's own y is reported as the thing that is wrong
    const line = p => {
      const cells = planCells(p)
      const { off, note } = planAnchor(cells, api.block)
      const real = off ? cells.map(c => ({ ...c, y: c.y + off })) : cells
      // clutter is what stands over the plan that the plan never asked for: the rubble a walk or a tree left behind
      // the lane is a sentence, not a count: it goes on a line of its own like the anchor, so the census stays one line
      const { noLane, ...counts } = fieldCensus(real, api.block)
      const census = compact({ ...counts, clutter: clutterLine(clutterBlocks(real, api.block)) }, false)
      const lines = [noLane ? `\n  lane: ${noLane}` : '', note ? `\n  anchor: ${note}` : ''].join('')
      return `${p.name} ${Math.round(Math.hypot(p.x - here.x, p.z - here.z))}m ${census}${lines}`
    }
    return { text: plans.map(line).join('\n') }
  }
}
