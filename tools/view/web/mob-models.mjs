// Why JavaScript: browser-safe graphics tables shared by the WebGL view and the Node software renderers (renderer.mjs, raycaster.mjs); imports nothing.
// Simple textured models for the commonest mobs: a few boxes each, in a static pose, painted from the mob's own entity sheet in the client jar
// (assets/minecraft/textures/entity/...). Recognisable, not exact: ALL DIMENSIONS AND SHEET REGIONS ARE FROM MEMORY of the vanilla models
// (checked only against the sheets' pictures), limbs are plain boxes, and heads, hats and noses are the only extras.
//
// A model is in the mob's own frame in model pixels (16 to a block): x across (+x is the mob's right), y up from the feet, z forward (+z the
// front), centred on the mob's position. It is scaled so the model's nominal height fits the entity's height (a baby is just smaller), and
// turned to the entity's yaw. A face's texture is `entity/<sheet>#x,y,w,h`: a region of the sheet, in block-entity-models.mjs's naming, so the
// texture array of the WebGL view holds each region once (materials.mjs registers them). Regions that are never seen well are shared:
// the bottom of a box takes its top, a limb takes one region on every face, left limbs take the right limb's.
// Mobs that are not in MOBS keep the renderers' flat colour shapes.

export const ENTITY_PREFIX = 'entity/'
// the order of a part's six layers, the shader's and the software renderers' (FACE_OF_DIR below)
export const FACES = ['top', 'bottom', 'south', 'north', 'east', 'west']

// the regions of a box (w x h x d at texture offset u, v) by direction, the front toward -z as the sheets have it (north = front)
const net = ({ u, v, w, h, d }) => ({
  up: [u + d, v, w, d],
  down: [u + d + w, v, w, d],
  east: [u, v + d, d, h],
  north: [u + d, v + d, w, h],
  west: [u + d + w, v + d, d, h],
  south: [u + 2 * d + w, v + d, w, h]
})

// which region goes on which face (our front is +z = 'south')
const upright = n => ({ top: n.up, bottom: n.up, south: n.north, north: n.south, east: n.east, west: n.east })
const trunk = n => ({ ...upright(n), top: n.north, bottom: n.north }) // the top of a body or robe is not worth a layer
const limb = n => ({ top: n.north, bottom: n.north, south: n.north, north: n.north, east: n.north, west: n.north })
// small parts: the front, the top and one side for the rest
const compact = n => ({ top: n.up, bottom: n.up, south: n.north, north: n.east, east: n.east, west: n.east })
// a body lying along z (quadruped bodies are drawn turned a quarter: the net's front is the back, its top the front end): the back, a side and the ends
const lying = n => ({ top: n.north, bottom: n.north, south: n.up, north: n.up, east: n.east, west: n.east })

// paint: 0 body, 1 head, 2 limbs (the entries of the flat palette, web/mobs.mjs, used when a sheet is missing)
const cube = (from, to, nt, pick, paint, sheet) => ({ from, to, faces: pick(net(nt)), paint, ...(sheet ? { sheet } : {}) })

// ---------------------------------------------------------------- shapes
const humanoid = ({ headW = 8, armsForward = false, nose = null } = {}) => [
  cube([-headW / 2, 24, -4], [headW / 2, 32, 4], { u: 0, v: 0, w: headW, h: 8, d: 8 }, upright, 1),
  ...(nose ? [cube([-2, 25, 4], [2, 29, 5], nose, limb, 1)] : []),
  cube([-4, 12, -2], [4, 24, 2], { u: 16, v: 16, w: 8, h: 12, d: 4 }, trunk, 0),
  ...[[4, 8], [-8, -4]].map(([x0, x1]) => cube(armsForward ? [x0, 20, -2] : [x0, 12, -2], armsForward ? [x1, 24, 10] : [x1, 24, 2], { u: 40, v: 16, w: 4, h: 12, d: 4 }, limb, 2)),
  ...[[0, 4], [-4, 0]].map(([x0, x1]) => cube([x0, 0, -2], [x1, 12, 2], { u: 0, v: 16, w: 4, h: 12, d: 4 }, limb, 2))
]

const thin = () => [
  cube([-4, 24, -4], [4, 32, 4], { u: 0, v: 0, w: 8, h: 8, d: 8 }, upright, 1),
  cube([-4, 12, -2], [4, 24, 2], { u: 16, v: 16, w: 8, h: 12, d: 4 }, trunk, 0),
  ...[[4, 6], [-6, -4]].map(([x0, x1]) => cube([x0, 12, -1], [x1, 24, 1], { u: 40, v: 16, w: 2, h: 12, d: 2 }, limb, 2)),
  ...[[0, 2], [-2, 0]].map(([x0, x1]) => cube([x0, 0, -1], [x1, 12, 1], { u: 0, v: 16, w: 2, h: 12, d: 2 }, limb, 2))
]

const creeper = () => [
  cube([-4, 18, -4], [4, 26, 4], { u: 0, v: 0, w: 8, h: 8, d: 8 }, upright, 1),
  cube([-4, 6, -2], [4, 18, 2], { u: 16, v: 16, w: 8, h: 12, d: 4 }, trunk, 0),
  ...[[-4, 0], [0, 4]].flatMap(([x0, x1]) => [[2, 6], [-6, -2]].map(([z0, z1]) => cube([x0, 0, z0], [x1, 6, z1], { u: 0, v: 16, w: 4, h: 6, d: 4 }, limb, 2)))
]

// body: [x half-width, y0, y1, z0, z1], the box it is drawn from (its sheet net is that of the box before it was turned)
const quadruped = ({ body, bodyNet, bodySheet, head, headNet, legTop, legNet, legZ }) => [
  cube([-body[0], body[1], body[3]], [body[0], body[2], body[4]], bodyNet, lying, 0, bodySheet),
  cube(head[0], head[1], headNet, compact, 1),
  ...[[1, 5], [-5, -1]].flatMap(([x0, x1]) => legZ.map(([z0, z1]) => cube([x0, 0, z0], [x1, legTop, z1], legNet, limb, 2)))
]

const SHAPES = {
  humanoid: { px: 32, hit: 1.8, parts: humanoid() },
  zombie: { px: 32, hit: 1.95, parts: humanoid({ armsForward: true }) },
  piglin: { px: 32, hit: 1.95, parts: humanoid({ headW: 10, nose: { u: 31, v: 1, w: 4, h: 4, d: 1 } }) },
  zombified_piglin: { px: 32, hit: 1.95, parts: humanoid({ headW: 10, armsForward: true, nose: { u: 31, v: 1, w: 4, h: 4, d: 1 } }) },
  thin: { px: 32, hit: 1.99, parts: thin() },
  creeper: { px: 26, hit: 1.7, parts: creeper() },
  cow: { px: 24, hit: 1.4, parts: quadruped({ body: [6, 13, 23, -10, 8], bodyNet: { u: 18, v: 4, w: 12, h: 18, d: 10 }, head: [[-4, 16, 8], [4, 24, 14]], headNet: { u: 0, v: 0, w: 8, h: 8, d: 6 }, legTop: 12, legNet: { u: 0, v: 16, w: 4, h: 12, d: 4 }, legZ: [[4, 8], [-9, -5]] }) },
  pig: { px: 16, hit: 0.9, parts: [...quadruped({ body: [5, 6, 14, -8, 8], bodyNet: { u: 28, v: 8, w: 10, h: 16, d: 8 }, head: [[-4, 8, 7], [4, 16, 15]], headNet: { u: 0, v: 0, w: 8, h: 8, d: 8 }, legTop: 6, legNet: { u: 0, v: 16, w: 4, h: 6, d: 4 }, legZ: [[3, 7], [-9, -5]] }), cube([-2, 9, 15], [2, 12, 16], { u: 16, v: 16, w: 4, h: 3, d: 1 }, limb, 1)] },
  sheep: { px: 21, hit: 1.3, parts: quadruped({ body: [5.5, 11, 21, -9, 9], bodyNet: { u: 28, v: 8, w: 8, h: 16, d: 6 }, bodySheet: 'sheep/sheep_wool', head: [[-3, 15, 7], [3, 21, 15]], headNet: { u: 0, v: 0, w: 6, h: 6, d: 8 }, legTop: 12, legNet: { u: 0, v: 16, w: 4, h: 12, d: 4 }, legZ: [[3, 7], [-9, -5]] }) },
  spider: { px: 13, hit: 0.9, parts: [
    cube([-4, 5, 3], [4, 13, 11], { u: 32, v: 4, w: 8, h: 8, d: 8 }, compact, 1),
    cube([-3, 6, -3], [3, 12, 3], { u: 0, v: 0, w: 6, h: 6, d: 6 }, compact, 0),
    cube([-5, 5, -15], [5, 13, -3], { u: 0, v: 12, w: 10, h: 8, d: 12 }, compact, 0),
    ...[[4, 11], [-11, -4]].flatMap(([x0, x1]) => [3.5, 1, -1.5, -4].map(z => cube([x0, 5, z], [x1, 7, z + 2], { u: 18, v: 0, w: 16, h: 2, d: 2 }, limb, 2)))
  ] },
  chicken: { px: 14, hit: 0.7, parts: [
    cube([-3, 5, -4], [3, 11, 4], { u: 0, v: 9, w: 6, h: 8, d: 6 }, lying, 0),
    cube([-2, 9, 3], [2, 15, 6], { u: 0, v: 0, w: 4, h: 6, d: 3 }, compact, 1),
    cube([-2, 11, 6], [2, 13, 8], { u: 14, v: 0, w: 4, h: 2, d: 2 }, limb, 1),
    cube([-1, 9, 6], [1, 11, 7], { u: 14, v: 4, w: 2, h: 2, d: 2 }, limb, 1),
    ...[[-4, -3], [3, 4]].map(([x0, x1]) => cube([x0, 7, -3], [x1, 11, 3], { u: 24, v: 13, w: 1, h: 4, d: 6 }, limb, 2)),
    ...[[0.5, 3.5], [-3.5, -0.5]].map(([x0, x1]) => cube([x0, 0, -2.5], [x1, 5, 0.5], { u: 26, v: 0, w: 3, h: 5, d: 3 }, limb, 2))
  ] },
  villager: { px: 34, hit: 1.95, parts: [
    cube([-4, 24, -4], [4, 34, 4], { u: 0, v: 0, w: 8, h: 10, d: 8 }, upright, 1),
    cube([-1, 23, 4], [1, 27, 6], { u: 24, v: 0, w: 2, h: 4, d: 2 }, limb, 1),
    cube([-4, 12, -3], [4, 24, 3], { u: 16, v: 20, w: 8, h: 12, d: 6 }, limb, 0),
    cube([-4.5, 3, -3.5], [4.5, 23, 3.5], { u: 0, v: 38, w: 8, h: 20, d: 6 }, limb, 0),
    cube([-4, 18, 3.5], [4, 22, 7.5], { u: 44, v: 22, w: 8, h: 4, d: 4 }, limb, 2),
    ...[[4, 8], [-8, -4]].map(([x0, x1]) => cube([x0, 15, 3], [x1, 23, 7], { u: 40, v: 38, w: 4, h: 8, d: 4 }, limb, 2)),
    ...[[0, 4], [-4, 0]].map(([x0, x1]) => cube([x0, 0, -2], [x1, 3, 2], { u: 0, v: 22, w: 4, h: 12, d: 4 }, limb, 2))
  ] },
  enderman: { px: 50, hit: 2.9, parts: [
    cube([-4, 42, -4], [4, 50, 4], { u: 0, v: 0, w: 8, h: 8, d: 8 }, upright, 1),
    cube([-4, 30, -2], [4, 42, 2], { u: 32, v: 16, w: 8, h: 12, d: 4 }, trunk, 0),
    ...[[4, 6], [-6, -4]].map(([x0, x1]) => cube([x0, 10, -1], [x1, 40, 1], { u: 56, v: 0, w: 2, h: 30, d: 2 }, limb, 2)),
    ...[[1, 3], [-3, -1]].map(([x0, x1]) => cube([x0, 0, -1], [x1, 30, 1], { u: 56, v: 0, w: 2, h: 30, d: 2 }, limb, 2))
  ] }
}
SHAPES.witch = { ...SHAPES.villager, parts: [...SHAPES.villager.parts,
  cube([-5, 34, -5], [5, 36, 5], { u: 0, v: 64, w: 10, h: 2, d: 10 }, compact, 1),
  cube([-3.5, 36, -3.5], [3.5, 40, 3.5], { u: 0, v: 76, w: 7, h: 4, d: 7 }, compact, 1),
  cube([-2, 40, -2], [2, 44, 2], { u: 0, v: 87, w: 4, h: 4, d: 4 }, compact, 1)] }

// mob name -> [shape, sheet under assets/minecraft/textures/entity (no .png)]
export const MOBS = {
  zombie: ['zombie', 'zombie/zombie'],
  husk: ['zombie', 'zombie/husk'],
  drowned: ['zombie', 'zombie/drowned'],
  zombified_piglin: ['zombified_piglin', 'piglin/zombified_piglin'],
  player: ['humanoid', 'player/wide/steve'],
  piglin: ['piglin', 'piglin/piglin'],
  piglin_brute: ['piglin', 'piglin/piglin_brute'],
  skeleton: ['thin', 'skeleton/skeleton'],
  stray: ['thin', 'skeleton/stray'],
  wither_skeleton: ['thin', 'skeleton/wither_skeleton'],
  bogged: ['thin', 'skeleton/bogged'],
  creeper: ['creeper', 'creeper/creeper'],
  cow: ['cow', 'cow/cow_temperate'],
  mooshroom: ['cow', 'cow/mooshroom_red'],
  pig: ['pig', 'pig/pig_temperate'],
  sheep: ['sheep', 'sheep/sheep'],
  spider: ['spider', 'spider/spider'],
  cave_spider: ['spider', 'spider/cave_spider'],
  chicken: ['chicken', 'chicken/chicken_temperate'],
  villager: ['villager', 'villager/villager'],
  wandering_trader: ['villager', 'wandering_trader/wandering_trader'],
  witch: ['witch', 'witch/witch'],
  enderman: ['enderman', 'enderman/enderman']
}

export const layerName = (sheet, [x, y, w, h]) => `${ENTITY_PREFIX}${sheet}#${x},${y},${w},${h}`

// the shape's parts for a sheet: {from, to, paint, layers: [top, bottom, south, north, east, west] layer names} in model pixels
const partsFor = (shape, sheet) => SHAPES[shape].parts.map(({ from, to, faces, paint, sheet: own }) => ({ from, to, paint, layers: FACES.map(f => layerName(own ?? sheet, faces[f])) }))

const built = new Map()
const modelOf = name => {
  if (!built.has(name)) built.set(name, MOBS[name] ? { ...SHAPES[MOBS[name][0]], parts: partsFor(...MOBS[name]) } : null)
  return built.get(name)
}

// every layer name the models use, for the texture array to hold
export const modelLayers = () => [...new Set(Object.keys(MOBS).flatMap(name => modelOf(name).parts.flatMap(p => p.layers)))]
// the sheets (under textures/entity) a mob's model reads
export const sheetsOf = name => modelOf(name) ? [...new Set(modelOf(name).parts.map(p => p.layers.map(l => l.slice(ENTITY_PREFIX.length, l.indexOf('#')))).flat(2))] : []

// the mob's right and forward in the world for a yaw (mineflayer's: 0 faces -z, turning left to -x): a model point (x, z) lands at
// (e.x + x * right.x + z * forward.x, e.z + x * right.z + z * forward.z)
export const yawBasis = yaw => ({ right: { x: Math.cos(yaw), z: -Math.sin(yaw) }, forward: { x: -Math.sin(yaw), z: -Math.cos(yaw) } })

// The model of an entity {name, height?, yaw?}: {parts: [{box: [x1, y1, z1, x2, y2, z2] in blocks in the mob's frame, paint, layers}], hull: the box
// round them, right, forward}, or null for a mob with no model.
export const modelFor = e => {
  const model = modelOf(e.name)
  if (!model) return null
  const unit = (e.height ?? model.hit) / model.px
  const parts = model.parts.map(p => ({ box: [...p.from, ...p.to].map(v => v * unit), paint: p.paint, layers: p.layers }))
  const hull = [0, 1, 2].map(i => Math.min(...parts.map(p => p.box[i]))).concat([3, 4, 5].map(i => Math.max(...parts.map(p => p.box[i]))))
  return { parts, hull, ...yawBasis(e.yaw ?? 0) }
}

// the world box (min, max) round a model turned about an entity at (x, y, z)
export const worldBox = (model, { x, y, z }) => {
  const h = model.hull
  const corners = [[h[0], h[2]], [h[3], h[2]], [h[0], h[5]], [h[3], h[5]]]
    .map(([mx, mz]) => [x + mx * model.right.x + mz * model.forward.x, z + mx * model.right.z + mz * model.forward.z])
  return { min: [Math.min(...corners.map(c => c[0])), y + h[1], Math.min(...corners.map(c => c[1]))], max: [Math.max(...corners.map(c => c[0])), y + h[4], Math.max(...corners.map(c => c[1]))] }
}

// Where on a face a texture is read, from the hit's position in the box as fractions (fx, fy, fz) of its x, y, z extent; the sheets' faces
// seen from outside: the front (+z) and back have u across, the sides u along z, the top has the front at its top.
// Returns [u, v] in 0..1 of the face's region.
export const faceUV = (face, fx, fy, fz) => {
  switch (face) {
    case 'south': return [1 - fx, 1 - fy]
    case 'north': return [fx, 1 - fy]
    case 'east': return [fz, 1 - fy]
    case 'west': return [1 - fz, 1 - fy]
    case 'top': return [fx, 1 - fz]
    default: return [fx, fz]
  }
}
