// Rails in the fake world, after the game's connection rule (BaseRailBlock / RailState). A rail takes its shape when it
// is placed (railsPlaced): from the rails beside it, at its own height or one up or down, that still have a free end
// (a rail with two live connections is full), and the rails it joins take theirs again; a rail whose neighbour is
// broken keeps its shape. The shape is kept in the cell's state, so out-of-order placing shows as the wrong shapes the
// game gives. A rail that was never placed (written into a test's world) reads as if it had been placed in line order.
// The first shape of a lone rail follows where the body stands (measured: along x when it stands out along x).
// A powered rail is lit when a source touches it (a redstone block, a redstone torch, a lever switched on; below
// counts) or touches one of the powered rails up to 8 further along its own unbroken run of powered rails, slopes
// included (measured live). Not modelled: detector and activator rails, the redstone signal a corner rail may take.

const RAIL = /(^|_)rail$/
const REACH = 8
const SIDES = [[1, 0, 0], [-1, 0, 0], [0, 0, 1], [0, 0, -1], [0, 1, 0], [0, -1, 0]]
const [N, S, W, E, UP] = [[0, 0, -1], [0, 0, 1], [-1, 0, 0], [1, 0, 0], [0, 1, 0]]
const CONNECTIONS = {
  north_south: [N, S],
  east_west: [E, W],
  ascending_east: [W, [1, 1, 0]],
  ascending_west: [[-1, 1, 0], E],
  ascending_north: [[0, 1, -1], S],
  ascending_south: [N, [0, 1, 1]],
  south_east: [E, S],
  south_west: [W, S],
  north_west: [W, N],
  north_east: [N, E]
}

const key = ({ x, y, z }) => `${x},${y},${z}`
const plus = (p, [dx, dy, dz]) => ({ x: p.x + dx, y: p.y + dy, z: p.z + dz })
const sameColumn = (a, b) => a.x === b.x && a.z === b.z
const nameAt = (s, p) => s.blocks.get(key(p)) ?? 'air'

export const isRail = name => RAIL.test(name)

const isRailAt = (s, p) => isRail(nameAt(s, p))
const isStraight = (s, p) => nameAt(s, p) !== 'rail'

// getRail: the rail at p, else the one on top of it, else the one under it.
function railAt (s, p) {
  return [[0, 0, 0], UP, [0, -1, 0]].map(d => plus(p, d)).find(q => isRailAt(s, q)) ?? null
}

// The shape a rail placed now at pos takes among the rails beside it. canConnect(rail) says whether a neighbouring rail
// has a free end for it. Straight rails (powered ones) take no corner.
function chooseShape (s, pos, initial, straight, canConnect) {
  const joins = d => { const r = railAt(s, plus(pos, d)); return r !== null && canConnect(r) }
  const [n, so, w, e] = [N, S, W, E].map(joins)
  const ns = n || so
  const ew = w || e
  let shape = null
  if (ns && !ew) shape = 'north_south'
  if (ew && !ns) shape = 'east_west'
  const [se, sw, ne, nw] = [so && e, so && w, n && e, n && w]
  if (!straight) {
    if (se && !n && !w) shape = 'south_east'
    if (sw && !n && !e) shape = 'south_west'
    if (nw && !so && !e) shape = 'north_west'
    if (ne && !so && !w) shape = 'north_east'
  }
  if (shape === null) {
    if (ns && ew) shape = initial
    if (!straight) {
      if (nw) shape = 'north_west'
      if (ne) shape = 'north_east'
      if (sw) shape = 'south_west'
      if (se) shape = 'south_east'
    }
  }
  return climbing(s, pos, shape) ?? initial
}

// A flat straight shape turns to climb when the rail one up lies ahead.
function climbing (s, pos, shape) {
  const rail = d => isRailAt(s, plus(plus(pos, d), UP))
  if (shape === 'north_south') return rail(S) ? 'ascending_south' : rail(N) ? 'ascending_north' : shape
  if (shape === 'east_west') return rail(W) ? 'ascending_west' : rail(E) ? 'ascending_east' : shape
  return shape
}

function storedShape (s, pos) { return s.states.get(key(pos))?.shape }

function shapeOf (s, pos) {
  return storedShape(s, pos) ?? chooseShape(s, pos, 'north_south', isStraight(s, pos), () => true)
}

const connectionsOf = (s, pos) => CONNECTIONS[shapeOf(s, pos)].map(d => plus(pos, d))

// The live connections of the rail at pos: those whose rail connects back.
function liveConnections (s, pos) {
  return connectionsOf(s, pos).filter(c => {
    const other = railAt(s, c)
    return other !== null && connectionsOf(s, other).some(x => sameColumn(x, pos))
  })
}

// A rail can take one more connection from `from` when it already holds one to it or has a free end.
function canConnectTo (s, rail, from) {
  const live = liveConnections(s, rail)
  return live.some(c => sameColumn(c, from)) || live.length !== 2
}

// The rail at pos takes the connection to `from` besides its live ones.
function connectTo (s, rail, from) {
  const held = [...liveConnections(s, rail), from]
  const has = d => held.some(c => sameColumn(c, plus(rail, d)))
  const [n, so, w, e] = [N, S, W, E].map(has)
  let shape = null
  if (n || so) shape = 'north_south'
  if (w || e) shape = 'east_west'
  if (!isStraight(s, rail)) {
    if (so && e && !n && !w) shape = 'south_east'
    if (so && w && !n && !e) shape = 'south_west'
    if (n && w && !so && !e) shape = 'north_west'
    if (n && e && !so && !w) shape = 'north_east'
  }
  s.states.set(key(rail), { ...s.states.get(key(rail)), shape: climbing(s, rail, shape) ?? 'north_south' })
}

// Where the body stands decides the shape of a rail with no neighbour.
const firstShape = (s, pos) => Math.abs(pos.x - s.self.pos.x) > Math.abs(pos.z - s.self.pos.z) ? 'east_west' : 'north_south'

function railPlaced (s, pos) {
  const initial = firstShape(s, pos)
  s.states.set(key(pos), { ...s.states.get(key(pos)), shape: initial })
  const shape = chooseShape(s, pos, initial, isStraight(s, pos), rail => canConnectTo(s, rail, pos))
  s.states.set(key(pos), { ...s.states.get(key(pos)), shape })
  for (const c of connectionsOf(s, pos)) {
    const rail = railAt(s, c)
    if (rail !== null && canConnectTo(s, rail, pos)) connectTo(s, rail, pos)
  }
}

// Called with the blocks a place just set: every rail among them settles and joins its neighbours.
export function railsPlaced (s, blocks) {
  for (const b of blocks) if (isRail(b.name)) railPlaced(s, b.pos)
}

function railLit (s, pos) {
  const source = p => {
    const name = nameAt(s, p)
    return name === 'redstone_block' || /^redstone(_wall)?_torch$/.test(name) || (name === 'lever' && s.states.get(key(p))?.powered === true)
  }
  const touched = p => SIDES.some(d => source(plus(p, d)))
  // walks the run from pos through its connection c, up to REACH rails
  const along = c => {
    let prev = pos
    let rail = railAt(s, c)
    for (let i = 0; i < REACH && rail !== null && nameAt(s, rail) === 'powered_rail'; i++) {
      if (touched(rail)) return true
      const next = connectionsOf(s, rail).find(x => !sameColumn(x, prev))
      prev = rail
      rail = next ? railAt(s, next) : null
    }
    return false
  }
  return touched(pos) || connectionsOf(s, pos).some(along)
}

// The worked-out properties of the block at pos in fake state s ({} for anything but a rail).
export function railProperties (s, pos) {
  const name = nameAt(s, pos)
  if (!isRail(name)) return {}
  const shape = shapeOf(s, pos)
  return name === 'powered_rail' ? { shape, powered: railLit(s, pos) } : { shape }
}
