// Rails in the fake world: the shape and powered state a rail reads with, worked out from its neighbours whenever it
// is read (a stored state wins, so a test can set a wrong shape). Straight flat lines only: a rail joins the rails
// beside it at its own height, along x or z, and a lone rail reads north_south. A powered rail is lit when a source
// touches it (a redstone block, a redstone torch, a lever switched on; below counts) or touches one of the powered
// rails up to 8 further along its own unbroken run of powered rails (measured live). No corners, slopes, detector
// rails or the game's connection rule.

const RAIL = /(^|_)rail$/
const REACH = 8
const SIDES = [[1, 0, 0], [-1, 0, 0], [0, 0, 1], [0, 0, -1], [0, 1, 0], [0, -1, 0]]
const ALONG = { east_west: [[1, 0, 0], [-1, 0, 0]], north_south: [[0, 0, 1], [0, 0, -1]] }

const key = ({ x, y, z }) => `${x},${y},${z}`
const plus = (p, [dx, dy, dz]) => ({ x: p.x + dx, y: p.y + dy, z: p.z + dz })

export const isRail = name => RAIL.test(name)

function railShape (nameAt, pos) {
  const joined = ds => ds.filter(d => isRail(nameAt(plus(pos, d)))).length
  return joined(ALONG.east_west) > joined(ALONG.north_south) ? 'east_west' : 'north_south'
}

function railLit (nameAt, stateAt, pos) {
  const source = p => {
    const name = nameAt(p)
    return name === 'redstone_block' || /^redstone(_wall)?_torch$/.test(name) || (name === 'lever' && stateAt(p)?.powered === true)
  }
  const touched = p => SIDES.some(d => source(plus(p, d)))
  const along = d => {
    let p = pos
    for (let i = 0; i < REACH; i++) {
      p = plus(p, d)
      if (nameAt(p) !== 'powered_rail') return false
      if (touched(p)) return true
    }
    return false
  }
  return touched(pos) || ALONG[railShape(nameAt, pos)].some(along)
}

// The worked-out properties of the block at pos in fake state s ({} for anything but a rail).
export function railProperties (s, pos) {
  const nameAt = p => s.blocks.get(key(p)) ?? 'air'
  const name = nameAt(pos)
  if (!isRail(name)) return {}
  const shape = railShape(nameAt, pos)
  return name === 'powered_rail' ? { shape, powered: railLit(nameAt, p => s.states.get(key(p)), pos) } : { shape }
}
