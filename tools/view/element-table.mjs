// Why JavaScript: GPU/binary; builds the float32 element table texture the shader reads.
// The model element table the shader reads: a float32 RGBA texture, TABLE_WIDTH texels wide, row after row. Served as raw little-endian
// float32 (the page uploads it as RGBA32F, rows = data.length / 4 / TABLE_WIDTH).
//
//   texels [0, listTexels):   element id lists, four ids per texel (x, y, z, w), one list per distinct state element list;
//                             a material's elemOffset is the index of its first id, elemCount how many follow
//   texels [listTexels, ...): the distinct elements, ELEMENT_TEXELS texels each; element id e starts at texel listTexels + e * ELEMENT_TEXELS
//
// One element (texel: x y z w):
//   0   from.x from.y from.z  rotation angle in degrees (0 when the element has no rotation)
//   1   to.x to.y to.z        flags: rotation axis (0 none, 1 x, 2 y, 3 z) + 4 * rescale + 8 * (no directional shading)
//   2   rotation origin x y z, 0
//   3 + 2f  layer + 1 (0 = no face), uv rotation in quarter turns (0..3), tint = group + 8 * constant index (groups: tints.mjs TINT_GROUPS),
//           cullface (0 none, 1 + direction index)
//   4 + 2f  u0 v0 u1 v1 (uv rect in 0..16 texture space)
//   for face f = 0..5 in FACE_DIRS order. Coordinates are 0..16 block space; from/to may leave 0..16 (the shader only draws what is inside the voxel).
import { TINT_GROUPS } from './tints.mjs'

export const TABLE_WIDTH = 1024
export const ELEMENT_TEXELS = 15
export const FACE_DIRS = ['up', 'down', 'north', 'south', 'east', 'west']
const AXES = ['', 'x', 'y', 'z']

const texelsOf = elementCount => elementCount * ELEMENT_TEXELS

const encodeElement = (element, out, at) => {
  const { from, to, rotation, shade = true } = element
  out.set([...from, rotation?.angle ?? 0], at)
  out.set([...to, AXES.indexOf(rotation?.axis ?? '') + (rotation?.rescale ? 4 : 0) + (shade ? 0 : 8)], at + 4)
  out.set([...(rotation?.origin ?? [8, 8, 8]), 0], at + 8)
  FACE_DIRS.forEach((dir, f) => {
    const face = element.faces[dir]
    if (!face) return
    out.set([face.layer + 1, face.rotation / 90, TINT_GROUPS.indexOf(face.tint ?? 'none') + 8 * (face.tintIndex ?? 0), face.cullface ? 1 + FACE_DIRS.indexOf(face.cullface) : 0], at + (3 + 2 * f) * 4)
    out.set(face.uv, at + (4 + 2 * f) * 4)
  })
}

const decodeElement = (data, at) => {
  const flags = data[at + 7]
  const axis = AXES[flags & 3]
  const faces = {}
  FACE_DIRS.forEach((dir, f) => {
    const [layer, turns, tint, cull] = data.subarray(at + (3 + 2 * f) * 4, at + (3 + 2 * f) * 4 + 4)
    if (!layer) return
    faces[dir] = { layer: layer - 1, uv: [...data.subarray(at + (4 + 2 * f) * 4, at + (4 + 2 * f) * 4 + 4)], rotation: turns * 90, tint: TINT_GROUPS[tint & 7], ...(tint >> 3 ? { tintIndex: tint >> 3 } : {}), cullface: cull ? FACE_DIRS[cull - 1] : null }
  })
  return {
    from: [...data.subarray(at, at + 3)],
    to: [...data.subarray(at + 4, at + 7)],
    rotation: axis ? { origin: [...data.subarray(at + 8, at + 11)], axis, angle: data[at + 3], rescale: (flags & 4) !== 0 } : null,
    shade: (flags & 8) === 0,
    faces
  }
}

// lists: arrays of elements (see the layout; faces[dir] = { layer, uv, rotation, tint, cullface }). Returns the float data and, per list, its [offset, count].
export function packElementTable (lists) {
  const elementIds = new Map()
  const elements = []
  const ids = []
  const ranges = lists.map(list => {
    const offset = ids.length
    for (const element of list) {
      const key = JSON.stringify(element)
      if (!elementIds.has(key)) elementIds.set(key, elements.push(element) - 1)
      ids.push(elementIds.get(key))
    }
    return [offset, list.length]
  })
  const listTexels = Math.ceil(ids.length / 4)
  const total = listTexels + texelsOf(elements.length)
  const rows = Math.max(1, Math.ceil(total / TABLE_WIDTH))
  const data = new Float32Array(rows * TABLE_WIDTH * 4)
  data.set(ids)
  elements.forEach((element, e) => encodeElement(element, data, (listTexels + e * ELEMENT_TEXELS) * 4))
  return { data, ranges, listTexels, elementCount: elements.length, idCount: ids.length, rows }
}

// the elements of the list at [offset, count) of a packed table
export const unpackList = ({ data, listTexels }, [offset, count]) =>
  Array.from({ length: count }, (_, k) => decodeElement(data, (listTexels + data[offset + k] * ELEMENT_TEXELS) * 4))
