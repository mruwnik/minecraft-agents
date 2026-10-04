// Why JavaScript: binary/graphics; block entity models drawn by the WebGL view.
// Block entities (chests, beds, signs, banners, heads, shulker boxes, decorated pots, the bell) are drawn by the game's Java entity-model
// code, not by blockstate JSON. This draws them in a static pose as baked elements in block-bake's format.
//
// ALL DIMENSIONS AND SHEET REGIONS BELOW ARE FROM MEMORY of the vanilla entity models (checked only against the sheets' pictures); where a
// part would not fit the voxel (banners are 1.75 blocks tall) it is shrunk to fit, and parts the view cannot show well (text, patterns,
// the lid animation, the V of unattached hanging-sign chains) are left out.
//
// A face's texture is `entity/<sheet>#x,y,w,h[@rrggbb]`: the region of a sheet under assets/minecraft/textures (x y w h in sheet pixels)
// and an optional constant tint multiplied in. The face's uv is the full 0..16 of that region's own atlas layer (tools/view/textures.mjs).
// Faces that could never be seen, and left/right regions that differ only slightly, share a region, to keep the layer count small.
//
// The models are written facing north (the front toward -z) and turned by the facing / rotation.
import { bakeApply } from './block-bake.mjs'
import { DYE_COLORS } from './tints.mjs'

export { DYE_COLORS }
export const ENTITY_PREFIX = 'entity/'
export const ADDS_TO_MODEL = new Set(['bell', 'lectern', 'enchanting_table']) // the jar model already draws these blocks' frame: the entity part is added to it

const WOODS = new Set(['acacia', 'bamboo', 'birch', 'cherry', 'crimson', 'dark_oak', 'jungle', 'mangrove', 'oak', 'pale_oak', 'spruce', 'warped'])
const FACING_Y = { north: 0, east: 90, south: 180, west: 270 }
const CHEST_SHEETS = { chest: 'normal', trapped_chest: 'trapped', ender_chest: 'ender', copper_chest: 'copper', exposed_copper_chest: 'copper_exposed', weathered_copper_chest: 'copper_weathered', oxidized_copper_chest: 'copper_oxidized' }
const SKULLS = {
  skeleton: { sheet: 'skeleton/skeleton' },
  wither_skeleton: { sheet: 'skeleton/wither_skeleton' },
  zombie: { sheet: 'zombie/zombie' },
  player: { sheet: 'player/wide/steve' },
  creeper: { sheet: 'creeper/creeper' },
  piglin: { sheet: 'piglin/piglin', width: 10 },
  dragon: { sheet: 'enderdragon/dragon', dragon: true }
}
const SHULKER_UP = { up: {}, down: { x: 180 }, north: { x: 90 }, east: { x: 90, y: 90 }, south: { x: 90, y: 180 }, west: { x: 90, y: 270 } }

// ---------------------------------------------------------------- building blocks

const texture = (sheet, [x, y, w, h], hex) => `${ENTITY_PREFIX}${sheet}#${x},${y},${w},${h}${hex ? `@${hex}` : ''}`

// the regions of a box (w x h x d at texture offset u, v) on its sheet, by the direction they face with the front toward the north
// (the usual net: top above the front, left, front, right, back in a row below)
// `yDown`: the sheet was made for a model drawn upside down (the chest's), so its first top/bottom region is the underside
const net = ({ u, v, w, h, d }, { yDown = false } = {}) => ({
  up: [yDown ? u + d + w : u + d, v, w, d],
  down: [yDown ? u + d : u + d + w, v, w, d],
  east: [u, v + d, d, h],
  north: [u + d, v + d, w, h],
  west: [u + d + w, v + d, d, h],
  south: [u + 2 * d + w, v + d, w, h]
})

const face = (tex, rotation = 0, uv = [0, 0, 16, 16]) => ({ texture: tex, uv, rotation })
// faces: direction -> texture | [texture, rotation]
const part = (from, to, faces, extra = {}) => ({
  from,
  to,
  faces: Object.fromEntries(Object.entries(faces).map(([dir, f]) => [dir, Array.isArray(f) ? face(f[0], f[1]) : face(f)])),
  ...extra
})

// the same texture on the listed directions
const on = (dirs, tex, rotation = 0) => Object.fromEntries(dirs.map(dir => [dir, [tex, rotation]]))
const SIDES = ['north', 'south', 'east', 'west']

// ---------------------------------------------------------------- posing

// the model turned `degrees` clockwise (seen from above): whole quarter turns by the blockstate machinery, the rest as an element rotation about y
const yawed = (elements, degrees) => {
  const quarters = Math.round(degrees / 90)
  const rest = degrees - 90 * quarters
  const turned = rest === 0 ? elements : elements.map(e => ({ ...e, rotation: { origin: [8, 8, 8], axis: 'y', angle: -rest, rescale: false } }))
  return bakeApply({ model: 'm', y: ((quarters % 4) + 4) % 4 * 90 }, new Map([['m', { elements: turned }]]))
}
const posed = (elements, apply) => bakeApply({ model: 'm', ...apply }, new Map([['m', { elements }]]))
const standingDegrees = rotation => 180 + 22.5 * Number(rotation ?? 0) // rotation 0 faces south, increasing clockwise

// ---------------------------------------------------------------- chests

const chestParts = (sheet, type) => {
  const double = type === 'left' || type === 'right'
  const w = double ? 15 : 14
  const lid = net({ u: 0, v: 0, w, h: 5, d: 14 }, { yDown: true })
  const body = net({ u: 0, v: 19, w, h: 10, d: 14 }, { yDown: true })
  // the partner of a left half is clockwise of the facing (east for north), so a left half reaches x 16 and a right half x 0
  const [x0, x1] = type === 'left' ? [1, 16] : type === 'right' ? [0, 15] : [1, 15]
  const latch = net({ u: 0, v: 0, w: double ? 1 : 2, h: 4, d: 1 })
  const latchX = type === 'left' ? [15, 16] : type === 'right' ? [0, 1] : [7, 9]
  const sheetName = `chest/${sheet}${double ? `_${type}` : ''}`
  const lidSide = texture(sheetName, lid.north)
  const bodySide = texture(sheetName, body.north)
  return [
    part([x0, 9, 1], [x1, 14, 15], { up: texture(sheetName, lid.up), ...on(SIDES, lidSide) }),
    part([x0, 0, 1], [x1, 9, 15], on(SIDES, bodySide)),
    part([latchX[0], 8, 0], [latchX[1], 12, 1], { north: texture(sheetName, latch.north) })
  ]
}

const chestElements = (name, props) => {
  const sheet = CHEST_SHEETS[name.replace(/^waxed_/, '')]
  if (!sheet) return null
  const type = sheet === 'ender' ? 'single' : props.type ?? 'single'
  return yawed(chestParts(sheet, type), FACING_Y[props.facing ?? 'north'])
}

// ---------------------------------------------------------------- beds

const bedElements = (colour, props) => {
  const sheet = `bed/${colour}`
  const head = props.part === 'head'
  const slab = net({ u: 0, v: head ? 0 : 22, w: 16, h: 16, d: 6 })
  const shared = net({ u: 0, v: 0, w: 16, h: 16, d: 6 })
  const leg = net({ u: 50, v: 6, w: 3, h: 3, d: 3 })
  const legTexture = texture(sheet, leg.north)
  const [z0, z1] = head ? [0, 3] : [13, 16]
  const endDir = head ? 'north' : 'south'
  const legs = [[0, 3], [13, 16]].map(([x0, x1]) => part([x0, 0, z0], [x1, 3, z1], on(SIDES, legTexture)))
  const body = part([0, 3, 0], [16, 9, 16], {
    up: texture(sheet, slab.north),
    [endDir]: [texture(sheet, shared.up), 180],
    west: [texture(sheet, shared.east), 90],
    east: [texture(sheet, shared.east), 270]
  })
  return yawed([body, ...legs], FACING_Y[props.facing ?? 'north'])
}

// ---------------------------------------------------------------- signs

const signElements = (wood, kind, props) => {
  const wall = kind.startsWith('wall')
  const hanging = kind.endsWith('hanging_sign')
  const turn = wall ? FACING_Y[props.facing ?? 'north'] : standingDegrees(props.rotation)
  if (hanging) return yawed(hangingParts(wood, wall, props), turn)
  const sheet = `signs/${wood}`
  const board = net({ u: 0, v: 0, w: 24, h: 12, d: 2 })
  const post = net({ u: 0, v: 14, w: 2, h: 14, d: 2 })
  const edge = texture(sheet, board.up)
  const boardFaces = { north: texture(sheet, board.north), south: texture(sheet, board.south), ...on(['up', 'east', 'west'], edge) }
  if (wall) return yawed([part([0, 4.5, 14.67], [16, 12.5, 16], boardFaces)], turn)
  return yawed([
    part([7.33, 0, 7.33], [8.67, 8, 8.67], on(SIDES, texture(sheet, post.north))),
    part([0, 8, 6.67], [16, 16, 9.33], boardFaces)
  ], turn)
}

const chainPair = x => [
  part([x - 1.5, 10, 8], [x + 1.5, 16, 8], { north: ['iron_chain', 0], south: ['iron_chain', 0] }),
  part([x, 10, 6.5], [x, 16, 9.5], { east: ['iron_chain', 0], west: ['iron_chain', 0] })
]
const withChainUv = elements => elements.map(e => ({ ...e, faces: Object.fromEntries(Object.entries(e.faces).map(([dir, f]) => [dir, f.texture === 'iron_chain' ? { ...f, uv: [dir === 'north' || dir === 'south' ? 0 : 3, 0, dir === 'north' || dir === 'south' ? 3 : 6, 16] } : f])) }))

const hangingParts = (wood, wall, props) => {
  const sheet = `signs/hanging/${wood}`
  const board = net({ u: 0, v: 12, w: 14, h: 10, d: 2 })
  const plank = net({ u: 0, v: 0, w: 16, h: 2, d: 4 })
  const boardPart = part([1, 0, 7], [15, 10, 9], { north: texture(sheet, board.north), south: texture(sheet, board.south), ...on(['up', 'east', 'west', 'down'], texture(sheet, board.up)) })
  const straight = wall || props.attached === true
  const chainsAt = straight ? [3, 13] : [5, 11] // the V of unattached chains is drawn as chains closer together
  const chains = withChainUv(chainsAt.flatMap(chainPair))
  if (!wall) return [boardPart, ...chains]
  const plankPart = part([0, 14, 6], [16, 16, 10], on(['north', 'south', 'east', 'west', 'up'], texture(sheet, plank.north)))
  return [boardPart, ...chains.map(c => ({ ...c, from: [c.from[0], c.from[1], c.from[2]], to: [c.to[0], 14, c.to[2]] })), plankPart]
}

// ---------------------------------------------------------------- banners

const bannerElements = (colour, wall, props) => {
  const hex = DYE_COLORS[colour]
  const cloth = net({ u: 0, v: 0, w: 20, h: 40, d: 1 })
  const pole = net({ u: 44, v: 0, w: 2, h: 42, d: 2 })
  const bar = net({ u: 0, v: 42, w: 20, h: 2, d: 2 })
  const sheet = 'banner/banner_base'
  const barTexture = texture(sheet, bar.north)
  if (wall) {
    return yawed([
      part([1.5, 13.5, 13.5], [14.5, 15.5, 16], on(SIDES, barTexture)),
      part([1.5, 1, 13], [14.5, 13.5, 14], { north: texture('banner/base', cloth.north, hex), south: texture('banner/base', cloth.south, hex) })
    ], FACING_Y[props.facing ?? 'north'])
  }
  return yawed([
    part([7, 0, 7], [9, 16, 9], on(SIDES, texture(sheet, pole.north))),
    part([1.5, 13.5, 7], [14.5, 15.5, 9], on(SIDES, barTexture)),
    part([1.5, 1, 6], [14.5, 13.5, 7], { north: texture('banner/base', cloth.north, hex), south: texture('banner/base', cloth.south, hex) })
  ], standingDegrees(props.rotation))
}

// ---------------------------------------------------------------- heads

const dragonParts = (sheet, base) => {
  const upper = net({ u: 112, v: 30, w: 12, h: 5, d: 16 })
  const jaw = net({ u: 176, v: 65, w: 12, h: 4, d: 16 })
  const [dx, dy, dz] = base
  const faces = (r, up) => ({ ...(up ? { up: texture(sheet, r.up) } : {}), north: texture(sheet, r.north), south: texture(sheet, r.south), east: texture(sheet, r.east), west: texture(sheet, r.east) })
  return [
    part([dx + 3.5, dy + 3, dz + 2], [dx + 12.5, dy + 6.75, dz + 14], faces(upper, true)),
    part([dx + 3.5, dy, dz + 2], [dx + 12.5, dy + 3, dz + 14], faces(jaw, false))
  ]
}

const headParts = (kind, wall) => {
  const { sheet, width = 8, dragon } = SKULLS[kind]
  const base = wall ? [0, 4, 4] : [0, 0, 0] // wall heads sit 4 up and 4 toward the wall (south)
  if (dragon) return dragonParts(sheet, base)
  const head = net({ u: 0, v: 0, w: width, h: 8, d: 8 })
  const x0 = (16 - width) / 2
  return [part([x0, base[1], 4 + base[2]], [x0 + width, base[1] + 8, 12 + base[2]], {
    up: texture(sheet, head.up),
    north: texture(sheet, head.north),
    south: texture(sheet, head.south),
    east: texture(sheet, head.east),
    west: texture(sheet, head.east)
  })]
}

const skullElements = (kind, wall, props) => yawed(headParts(kind, wall), wall ? FACING_Y[props.facing ?? 'north'] : standingDegrees(props.rotation))

// ---------------------------------------------------------------- shulker boxes, pots, the bell

const shulkerElements = (colour, props) => {
  const sheet = colour ? `shulker/shulker_${colour}` : 'shulker/shulker'
  const lid = net({ u: 0, v: 0, w: 16, h: 12, d: 16 })
  const base = net({ u: 0, v: 28, w: 16, h: 8, d: 16 })
  const elements = [
    part([0, 0, 0], [16, 8, 16], on(SIDES, texture(sheet, base.north))),
    part([0, 4, 0], [16, 16, 16], { up: texture(sheet, lid.up), ...on(SIDES, texture(sheet, lid.north)) })
  ]
  return posed(elements, SHULKER_UP[props.facing ?? 'up'])
}

const potElements = () => {
  const side = texture('decorated_pot/decorated_pot_side', [0, 0, 16, 16])
  const neck = texture('decorated_pot/decorated_pot_base', [0, 8, 8, 5])
  return posed([
    part([1, 0, 1], [15, 14, 15], on(['up', ...SIDES], side)),
    part([4, 14, 4], [12, 16, 12], on(['up', ...SIDES], neck))
  ], {})
}

const bellElements = () => {
  const body = net({ u: 0, v: 0, w: 6, h: 7, d: 6 })
  const lip = net({ u: 0, v: 13, w: 8, h: 2, d: 8 })
  const sheet = 'bell/bell_body'
  return posed([
    part([4, 4, 4], [12, 6, 12], { up: texture(sheet, lip.up), ...on(SIDES, texture(sheet, lip.north)) }),
    part([5, 6, 5], [11, 13, 11], { up: texture(sheet, body.up), ...on(SIDES, texture(sheet, body.north)) })
  ], {})
}

// ---------------------------------------------------------------- books (the lectern's and the enchanting table's)

// The vanilla book model on enchanting_table_book.png (64x32), open, spine along z: two 6x10 covers either side of a 2 wide seam, and
// 5x8x1 pages on them. In the game the table's book floats and flips and the lectern's lies open on its slope; both are static here.
// Covers and pages come from the sheet's regions: left cover (0,0) outside / (6,0) inside, right cover (16,0) / (22,0), seam (12,0),
// left pages (0,10), right pages (12,10).
const BOOK_SHEET = 'enchantment/enchanting_table_book'
const bookTexture = region => texture(BOOK_SHEET, region)

const bookParts = (y0, zc) => {
  const cover = ([x0, x1], outside, inside) => part([x0, y0, zc - 5], [x1, y0 + 0.25, zc + 5], { up: bookTexture(inside), down: bookTexture(outside), ...on(SIDES, bookTexture(outside)) })
  const pages = ([x0, x1], u) => {
    const box = net({ u, v: 10, w: 5, h: 8, d: 1 })
    return part([x0, y0 + 0.25, zc - 4], [x1, y0 + 1.25, zc + 4], { up: texture(BOOK_SHEET, box.north), down: texture(BOOK_SHEET, box.south), north: texture(BOOK_SHEET, box.up), south: texture(BOOK_SHEET, box.up), east: texture(BOOK_SHEET, box.east), west: texture(BOOK_SHEET, box.west) })
  }
  const seam = bookTexture([12, 0, 2, 10])
  return [
    cover([1, 7], [0, 0, 6, 10], [6, 0, 6, 10]),
    cover([9, 15], [16, 0, 6, 10], [22, 0, 6, 10]),
    part([7, y0, zc - 5], [9, y0 + 0.5, zc + 5], { up: seam, down: seam, ...on(SIDES, seam) }),
    pages([2, 7], 0),
    pages([9, 14], 12)
  ]
}

const enchantingTableBook = () => posed(bookParts(13, 8), {})

// The lectern's top is the jar element 12..16 high, tilted -22.5 degrees about x around (8, 8, 8). The book lies on its top surface (y 16
// before the tilt) but a baked element must stay inside the voxel, so it is stored 1.5 lower and tilted about the origin that puts it at
// the same place: R(p - O') + O' = R(p + d - O) + O  =>  O' = O + (I - R)^-1 R d  (in the y-z plane).
const LECTERN_TILT = -22.5
const LECTERN_LIFT = 1.5
const liftedTiltOrigin = (angle, lift) => {
  const [c, s] = [Math.cos(angle * Math.PI / 180), Math.sin(angle * Math.PI / 180)]
  const a = 1 - c // I - R = [[a, s], [-s, a]], inverse [[a, -s], [s, a]] / (a^2 + s^2)
  const det = a * a + s * s
  const [ry, rz] = [c * lift, s * lift] // R d, with d = (lift, 0)
  return [8 + (a * ry - s * rz) / det, 8 + (s * ry + a * rz) / det]
}

const lecternBook = props => {
  const [oy, oz] = liftedTiltOrigin(LECTERN_TILT, LECTERN_LIFT)
  const tilt = { rotation: { origin: [8, oy, oz], axis: 'x', angle: LECTERN_TILT, rescale: false } }
  return yawed(bookParts(16 - LECTERN_LIFT, 9.5).map(e => ({ ...e, ...tilt })), FACING_Y[props.facing ?? 'north'])
}

// ---------------------------------------------------------------- the dispatch

const SIGN = /^(.+?)_(wall_hanging_sign|wall_sign|hanging_sign|sign)$/
const BANNER = /^(.+?)_(wall_)?banner$/
const BED = /^(.+)_bed$/
const SHULKER = /^(?:(.+)_)?shulker_box$/
const SKULL = /^(.+?)_(wall_)?(?:skull|head)$/

const build = (name, props) => {
  if (name in CHEST_SHEETS || name.replace(/^waxed_/, '') in CHEST_SHEETS) return chestElements(name, props)
  if (name === 'decorated_pot') return potElements()
  if (name === 'bell') return bellElements()
  if (name === 'enchanting_table') return enchantingTableBook()
  if (name === 'lectern') return props.has_book === true ? lecternBook(props) : null
  const sign = SIGN.exec(name)
  if (sign && WOODS.has(sign[1])) return signElements(sign[1], sign[2], props)
  const banner = BANNER.exec(name)
  if (banner && banner[1] in DYE_COLORS) return bannerElements(banner[1], Boolean(banner[2]), props)
  const bed = BED.exec(name)
  if (bed && bed[1] in DYE_COLORS) return bedElements(bed[1], props)
  const shulker = SHULKER.exec(name)
  if (shulker && (!shulker[1] || shulker[1] in DYE_COLORS)) return shulkerElements(shulker[1], props)
  const skull = SKULL.exec(name)
  if (skull && skull[1] in SKULLS && name !== 'piston_head') return skullElements(skull[1], Boolean(skull[2]), props)
  return null
}

const POSE_PROPS = ['facing', 'type', 'part', 'rotation', 'attached', 'has_book']
const cache = new Map()

// The baked elements of this block state, or null when it is not one of the block entities drawn here. Equal for states that differ
// only in properties the pose ignores (waterlogged, powered, occupied...).
export const blockEntityElements = (name, props) => {
  const key = `${name}|${POSE_PROPS.map(p => props[p]).join(',')}`
  if (!cache.has(key)) cache.set(key, build(name, props))
  return cache.get(key)
}
