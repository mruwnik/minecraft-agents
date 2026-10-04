// The game's rule from a click to the state of the block it places (vanilla getStateForPlacement), for the fake world:
// the face of the clicked neighbour, the cursor height on it, the look (yaw for a horizontal facing, yaw and pitch for
// torches and ladders) and nothing else. The ClojureScript side (engine.placement) runs the rule backwards to choose a
// click; this is the forward rule written on its own, so a job test through the fake checks the two against each other.
// Directions are mineflayer's: yaw 0 looks north (-z), pi/2 west; pitch -pi/2 looks down.

const DIRS = { north: [0, 0, -1], south: [0, 0, 1], east: [1, 0, 0], west: [-1, 0, 0], up: [0, 1, 0], down: [0, -1, 0] }
const OPPOSITE = { north: 'south', south: 'north', east: 'west', west: 'east', up: 'down', down: 'up' }
const BY_YAW = ['north', 'west', 'south', 'east']
const FREE = /^(air|cave_air|void_air|water|lava|fire|soul_fire|short_grass|tall_grass|grass|snow)$/
// no full face to hang a torch or ladder on, or to stand a door on
const THIN = /^(farmland|dirt_path|soul_sand|ladder|torch|wall_torch|lantern|chain|cobweb|chest|trapped_chest|ender_chest)$|_(slab|stairs|fence|fence_gate|wall|pane|door|trapdoor|carpet|sign|button|pressure_plate|bed|torch|rail|leaves|sapling)$/
const FRONT = /^(chest|trapped_chest|ender_chest|furnace|smoker|blast_furnace|carved_pumpkin|jack_o_lantern|lectern|beehive|bee_nest)$/
const PILLAR = /(_log|_wood|_stem|_hyphae)$|^(basalt|polished_basalt|quartz_pillar|purpur_pillar|hay_block|bone_block|deepslate|muddy_mangrove_roots|bamboo_block|stripped_bamboo_block)$/
const TORCH = /^(torch|soul_torch|redstone_torch|copper_torch)$/

const dirOf = ([x, y, z]) => Object.keys(DIRS).find(k => DIRS[k][0] === x && DIRS[k][1] === y && DIRS[k][2] === z)
const plus = (p, d) => ({ x: p.x + d[0], y: p.y + d[1], z: p.z + d[2] })
const horizontal = yaw => BY_YAW[((Math.round(yaw / (Math.PI / 2)) % 4) + 4) % 4]

// The six directions ordered by how much the look points along each (the game's getNearestLookingDirections).
const lookingDirections = (yaw, pitch) => {
  const v = { x: -Math.sin(yaw) * Math.cos(pitch), y: Math.sin(pitch), z: -Math.cos(yaw) * Math.cos(pitch) }
  return Object.keys(DIRS).map(d => [d, DIRS[d][0] * v.x + DIRS[d][1] * v.y + DIRS[d][2] * v.z]).sort((a, b) => b[1] - a[1]).map(([d]) => d)
}

const isFree = name => FREE.test(name)
const isSturdy = name => !isFree(name) && !THIN.test(name)

// placedBlocks({ item, pos, face, cursor, yaw, pitch, blockAt }): face is the clicked face's direction [dx, dy, dz]
// (from the clicked block into pos), cursor {x, y, z} on the clicked block, blockAt(pos) -> name ('air' when empty).
// Returns { blocks: [{ pos, name, properties }] } (a door or bed is two blocks) or { refused } when the game places nothing.
export function placedBlocks ({ item, pos, face, cursor, yaw = 0, pitch = 0, blockAt }) {
  const clicked = dirOf(face)
  const look = horizontal(yaw)
  const wet = blockAt(pos) === 'water'
  const half = clicked === 'down' ? 'top' : clicked === 'up' ? 'bottom' : cursor.y > 0.5 ? 'top' : 'bottom'
  const one = (name, properties) => ({ blocks: [{ pos, name, properties }] })
  if (item.endsWith('_stairs')) return one(item, { facing: look, half, shape: 'straight', waterlogged: wet })
  if (item.endsWith('_slab')) return one(item, { type: half, waterlogged: wet })
  if (item.endsWith('_trapdoor')) {
    const side = clicked !== 'up' && clicked !== 'down'
    return one(item, { facing: side ? clicked : OPPOSITE[look], half: side ? half : clicked === 'up' ? 'bottom' : 'top', open: false, powered: false, waterlogged: wet })
  }
  if (item.endsWith('_door')) {
    const above = plus(pos, DIRS.up)
    if (!isFree(blockAt(above))) return { refused: 'no room for the upper half' }
    if (!isSturdy(blockAt(plus(pos, DIRS.down)))) return { refused: 'no floor' }
    const door = part => ({ facing: look, half: part, hinge: 'left', open: false, powered: false })
    return { blocks: [{ pos, name: item, properties: door('lower') }, { pos: above, name: item, properties: door('upper') }] }
  }
  if (item.endsWith('_fence_gate')) return one(item, { facing: look, open: false, powered: false, in_wall: false })
  if (item.endsWith('_bed')) {
    const head = plus(pos, DIRS[look])
    if (!isFree(blockAt(head))) return { refused: 'no room for the head' }
    const bed = part => ({ facing: look, part, occupied: false })
    return { blocks: [{ pos, name: item, properties: bed('foot') }, { pos: head, name: item, properties: bed('head') }] }
  }
  if (FRONT.test(item)) return one(item, { facing: OPPOSITE[look], ...(item.endsWith('chest') && { type: 'single', waterlogged: wet }) })
  if (PILLAR.test(item)) return one(item, { axis: { east: 'x', west: 'x', up: 'y', down: 'y', north: 'z', south: 'z' }[clicked] })
  if (TORCH.test(item)) {
    for (const d of lookingDirections(yaw, pitch)) {
      if (d === 'up') continue
      if (d === 'down' && !isFree(blockAt(plus(pos, DIRS.down)))) return one(item, {})
      if (d !== 'down' && isSturdy(blockAt(plus(pos, DIRS[d])))) return one(item.replace(/torch$/, 'wall_torch'), { facing: OPPOSITE[d] })
    }
    return { refused: 'nothing holds the torch' }
  }
  if (item === 'ladder') {
    const d = lookingDirections(yaw, pitch).filter(d => d !== 'up' && d !== 'down').find(d => isSturdy(blockAt(plus(pos, DIRS[d]))))
    return d ? one(item, { facing: OPPOSITE[d], waterlogged: wet }) : { refused: 'nothing holds the ladder' }
  }
  return one(item, {})
}
