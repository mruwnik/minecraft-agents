// Why JavaScript: test data for the WebGL pixel checks (JS view stack).
// A world for the pixel checks of the block-entity models (tools/view-web-check.mjs --entities): a row of block entities in front of a
// lime wool wall and floor, seen from the south and a little above. Each region is a rectangle on a plane, in sixteenths of its block
// ("z16" is how far into the block's cell the plane lies), chosen so that a part drawn one or two sixteenths off lands in the
// wrong colour. Lime is the background: every region either must be all lime or must contain none.
import { writeWorld, MC_VERSION } from './fixture.mjs'

export const AGENT = 'Entities'
export const WIDTH = 1280
export const HEIGHT = 360
export const FOV = 80
const FLOOR_Y = 64
const PIECE_Y = 65
const ROW_Z = 8
const WALL_Z = 5
const FRONT_EYE_Y = 66.0
const FRONT_PITCH = -0.03

// Three stages along the row, each seen from the south with its middle piece straight ahead (so the far sides of the outer pieces are
// clean background), and a view from above for the bed's top. A view is { eye, pitch }.
export const VIEWS = {
  A: { eye: { x: -9, y: FRONT_EYE_Y, z: 16.5 }, pitch: FRONT_PITCH },
  B: { eye: { x: 0.5, y: FRONT_EYE_Y, z: 16.5 }, pitch: FRONT_PITCH },
  C: { eye: { x: 10.5, y: FRONT_EYE_Y, z: 16.5 }, pitch: FRONT_PITCH },
  D: { eye: { x: -2.5, y: 68, z: 11 }, pitch: -0.68 },
  E: { eye: { x: -6.5, y: 68, z: 11 }, pitch: -0.68 },
  F: { eye: { x: -1.5, y: 68, z: 11 }, pitch: -0.68 }
}

// cell (x, z) of each piece, and its block state
export const PIECES = {
  chest: { x: -13, z: ROW_Z, name: 'chest', props: { facing: 'south', type: 'single', waterlogged: false } },
  doubleRight: { x: -10, z: ROW_Z, name: 'chest', props: { facing: 'south', type: 'right', waterlogged: false } },
  doubleLeft: { x: -9, z: ROW_Z, name: 'chest', props: { facing: 'south', type: 'left', waterlogged: false } },
  table: { x: -7, z: ROW_Z, name: 'enchanting_table', props: {} },
  lectern: { x: -2, z: ROW_Z, name: 'lectern', props: { facing: 'south', has_book: true, powered: false } },
  sign: { x: -5, z: ROW_Z, name: 'oak_sign', props: { rotation: 0, waterlogged: false } },
  bedHead: { x: -3, z: ROW_Z - 1, name: 'blue_bed', props: { facing: 'north', part: 'head', occupied: false } },
  bedFoot: { x: -3, z: ROW_Z, name: 'blue_bed', props: { facing: 'north', part: 'foot', occupied: false } },
  banner: { x: 0, z: ROW_Z, name: 'red_banner', props: { rotation: 0 } },
  head: { x: 3, z: ROW_Z, name: 'skeleton_skull', props: { rotation: 0, powered: false } },
  shulker: { x: 7, z: ROW_Z, name: 'orange_shulker_box', props: { facing: 'up' } },
  bell: { x: 10, z: ROW_Z, name: 'bell', props: { attachment: 'floor', facing: 'north', powered: false } },
  pot: { x: 13, z: ROW_Z, name: 'decorated_pot', props: { facing: 'south', cracked: false, waterlogged: false } }
}
export const ENTITY_STATES = Object.fromEntries(Object.entries(PIECES).map(([key, { name, props }]) => [key, { name, props }]))

const pieceAt = (x, y, z) => y === PIECE_Y ? Object.entries(PIECES).find(([, p]) => p.x === x && p.z === z)?.[0] : undefined
const entityBlockAt = (x, y, z) => {
  if (x < -16 || x > 15 || z < -16 || z > 15) return 'air'
  if (y === FLOOR_Y) return 'lime_wool'
  if (z === WALL_Z && y >= PIECE_Y && y <= PIECE_Y + 3) return 'lime_wool'
  return pieceAt(x, y, z) ?? 'air'
}
const entityLight = (x, y, z) => entityBlockAt(x, y, z) === 'lime_wool' ? { sky: 0, block: 0 } : { sky: 15, block: 0 }

// a rectangle on the plane z = cell z + z16/16, x and y given in sixteenths of the cell
const zPlane = (piece, z16, [xa, xb], [ya, yb]) => ({ axis: 'z', at: piece.z + z16 / 16, x: [piece.x + xa / 16, piece.x + xb / 16], y: [PIECE_Y + ya / 16, PIECE_Y + yb / 16] })
const yPlane = (piece, y16, [xa, xb], [za, zb]) => ({ axis: 'y', at: PIECE_Y + y16 / 16, x: [piece.x + xa / 16, piece.x + xb / 16], z: [piece.z + za / 16, piece.z + zb / 16] })
const P = PIECES

// type: the check it belongs to; expect: 'lime' (all background), 'solid' (almost none), 'red' (banner cloth), 'seam' / 'same' / 'lighter' (against another rectangle)
export const ENTITY_REGIONS = [
  { view: 'A', type: 'chest', name: 'chest: above the lid is background (lid top at 14)', expect: 'lime', face: zPlane(P.chest, 15, [3, 13], [14.6, 15.7]) },
  { view: 'A', type: 'chest', name: 'chest: the lid is solid up to its top', expect: 'solid', face: zPlane(P.chest, 15, [3, 6], [12.3, 13.4]) },
  { view: 'A', type: 'chest', name: 'chest: the body is solid', expect: 'solid', face: zPlane(P.chest, 15, [3, 13], [1, 7.7]) },
  { view: 'A', type: 'chest', name: 'chest: beside the 14 wide body is background', expect: 'lime', face: zPlane(P.chest, 15, [-4, 0.7], [1, 13]) },
  { view: 'A', type: 'chest', name: 'chest: the lid seam (lid bottom row at 9) is darker than the body', expect: 'seam', face: zPlane(P.chest, 15, [3, 6], [8.3, 9.4]), against: zPlane(P.chest, 15, [3, 6], [4, 7]) },
  { view: 'A', type: 'double chest', name: 'double chest: no seam at the joint (lid)', expect: 'solid', face: zPlane(P.doubleLeft, 15, [-0.5, 0.5], [10.3, 12.7]) },
  { view: 'A', type: 'double chest', name: 'double chest: no seam at the joint (body)', expect: 'solid', face: zPlane(P.doubleLeft, 15, [-0.5, 0.5], [1, 7.7]) },
  { view: 'A', type: 'double chest', name: 'double chest: the west half ends 1/16 in', expect: 'lime', face: zPlane(P.doubleRight, 15, [0.15, 0.85], [1, 13]) },
  { view: 'A', type: 'double chest', name: 'double chest: the lid bottom row (9) is darker than the body', expect: 'seam', face: zPlane(P.doubleLeft, 15, [3, 6], [8.3, 9.4]), against: zPlane(P.doubleLeft, 15, [3, 6], [4, 7]) },
  { view: 'A', type: 'sign', name: 'sign: the board is solid', expect: 'solid', face: zPlane(P.sign, 9.33, [1, 15], [8.5, 15.5]) },
  { view: 'A', type: 'sign', name: 'sign: below the board, beside the post, is background', expect: 'lime', face: zPlane(P.sign, 9.33, [1.5, 5.5], [6.3, 7.7]) },
  { view: 'A', type: 'sign', name: 'sign: the board reaches down to 8', expect: 'solid', face: zPlane(P.sign, 9.33, [1.5, 5.5], [8.3, 9.7]) },
  { view: 'A', type: 'sign', name: 'sign: the post is solid', expect: 'solid', face: zPlane(P.sign, 8.67, [7.5, 8.5], [1, 6.5]) },
  { view: 'A', type: 'sign', name: 'sign: beside the post is background', expect: 'lime', face: zPlane(P.sign, 8.67, [5.5, 6.8], [1, 6.5]) },
  { view: 'B', type: 'bed', name: 'bed: the slab is solid from 3 to 9', expect: 'solid', face: zPlane(P.bedFoot, 16, [1, 15], [3.4, 8.6]) },
  { view: 'B', type: 'bed', name: 'bed: above the slab and its top surface is background (top at 9)', expect: 'lime', face: zPlane(P.bedFoot, 16, [1, 15], [11.2, 12.6]) },
  { view: 'B', type: 'bed', name: 'bed: the gap between the legs, under the slab', expect: 'lime', face: zPlane(P.bedFoot, 16, [4.2, 11.8], [0.4, 2.6]) },
  { view: 'B', type: 'bed', name: 'bed: the slab bottom is at 3, not lower', expect: 'solid', face: zPlane(P.bedFoot, 16, [4.2, 11.8], [3.3, 3.9]) },
  { view: 'B', type: 'bed', name: 'bed: a leg is solid', expect: 'solid', face: zPlane(P.bedFoot, 13, [0.6, 2.4], [0.5, 2.4]) },
  { view: 'D', type: 'bed', name: 'bed: the pillow end is lighter than the blanket', expect: 'lighter', face: yPlane(P.bedHead, 9, [2, 14], [0.4, 3.2]), against: yPlane(P.bedFoot, 9, [2, 14], [3, 12]) },
  { view: 'B', type: 'banner', name: 'banner: the cloth is the dye colour', expect: 'red', face: zPlane(P.banner, 10, [2.5, 6.3], [2, 12.5]) },
  { view: 'B', type: 'banner', name: 'banner: below the cloth (it ends at 1) is background', expect: 'lime', face: zPlane(P.banner, 10, [2.5, 6.3], [0.15, 0.8]) },
  { view: 'B', type: 'banner', name: 'banner: the cloth reaches down to 1', expect: 'red', face: zPlane(P.banner, 10, [2.5, 6.3], [1.25, 1.9]) },
  { view: 'B', type: 'banner', name: 'banner: the bar is solid above the cloth', expect: 'solid', face: zPlane(P.banner, 10, [2, 6], [13.8, 15.2]) },
  { view: 'B', type: 'banner', name: 'banner: the cloth is narrower than the block', expect: 'lime', face: zPlane(P.banner, 10, [-3, 0.9], [2, 12.5]) },
  { view: 'B', type: 'head', name: 'head: the cube is solid', expect: 'solid', face: zPlane(P.head, 12, [5, 11], [1, 7]) },
  { view: 'B', type: 'head', name: 'head: the face has dark eye sockets', expect: 'dark', face: zPlane(P.head, 12, [5, 11], [1, 7]) },
  { view: 'B', type: 'head', name: 'head: 8 wide, background to the right', expect: 'lime', face: zPlane(P.head, 12, [12.7, 15], [1, 7]) },
  { view: 'B', type: 'head', name: 'head: 8 high, background above', expect: 'lime', face: zPlane(P.head, 12, [5, 11], [8.7, 10]) },
  { view: 'C', type: 'shulker', name: 'shulker: the lid (above 4.4) is a different shade from the base', expect: 'seam', face: zPlane(P.shulker, 16, [3, 13], [4.6, 6]), against: zPlane(P.shulker, 16, [3, 13], [0.4, 2.4]) },
  { view: 'C', type: 'shulker', name: 'shulker: the base shade reaches up to 2.8 (the lid line is above)', expect: 'same', face: zPlane(P.shulker, 16, [3, 13], [2, 2.8]), against: zPlane(P.shulker, 16, [3, 13], [0.4, 1.8]) },
  { view: 'C', type: 'pot', name: 'pot: the neck is solid', expect: 'solid', face: zPlane(P.pot, 12, [4.6, 11.4], [14.4, 15.6]) },
  { view: 'C', type: 'pot', name: 'pot: the neck is 8 wide (background beside it)', expect: 'lime', face: zPlane(P.pot, 12, [12.6, 14.8], [15, 15.8]) },
  { view: 'C', type: 'pot', name: 'pot: the body is solid up to its shoulder', expect: 'solid', face: zPlane(P.pot, 15, [2, 14], [2, 13.4]) },
  { view: 'C', type: 'pot', name: 'pot: the body is 14 wide', expect: 'lime', face: zPlane(P.pot, 15, [15.3, 19], [2, 12]) },
  { view: 'C', type: 'bell', name: 'bell: the rim is solid', expect: 'solid', face: zPlane(P.bell, 12, [5, 11], [4.4, 5.6]) },
  { view: 'C', type: 'bell', name: 'bell: background under the rim (rim bottom at 4)', expect: 'lime', face: zPlane(P.bell, 12, [5, 11], [2.2, 3.6]) },
  { view: 'C', type: 'bell', name: 'bell: the rim is 8 wide', expect: 'lime', face: zPlane(P.bell, 12, [2.5, 3.6], [4.4, 5.6]) },
  { view: 'C', type: 'bell', name: 'bell: the body hangs clear of the posts', expect: 'lime', face: zPlane(P.bell, 12, [2.5, 3.8], [7.2, 12]) },
  { view: 'C', type: 'bell', name: 'bell: the body is 6 wide', expect: 'lime', face: zPlane(P.bell, 12, [3.9, 4.7], [7.2, 12]) },
  { view: 'E', type: 'enchanting table', name: 'enchanting table: the book above the table shows cream pages, not the table top', expect: 'cream', face: yPlane(P.table, 14.25, [3, 6], [5, 11]) },
  { view: 'F', type: 'lectern', name: 'lectern: the book pages show cream over the slanted top (tuned on the render), not the lectern wood', expect: 'cream', face: yPlane(P.lectern, 14, [10, 13], [10.6, 11.8]) }
]

export const writeEntityWorld = (stateDir, view = 'B') => writeWorld({
  stateDir,
  agent: AGENT,
  camera: { eye: VIEWS[view].eye, yaw: 0, pitch: VIEWS[view].pitch },
  blockAt: entityBlockAt,
  light: entityLight,
  states: ENTITY_STATES,
  keys: [...Object.keys(ENTITY_STATES), 'lime_wool', 'stone']
})
export { MC_VERSION }
