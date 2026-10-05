// Why JavaScript (for now): maths of the Node headless pixel check (tools/view-web-check.mjs) that inverts web/camera.mjs; it moves to
// cljs together with the camera maths, not alone.
// Pixel projection, the exact inverse of rayDir in web/camera.mjs. Browser-safe, no imports.
const dot = (a, b) => a.x * b.x + a.y * b.y + a.z * b.z
const MIN_DEPTH = 1e-6

// `offset` is the point minus the eye; null when it is not in front of the camera
export const projectPoint = ({ forward, right, up, half }, offset, width, height) => {
  const depth = dot(offset, forward)
  if (depth <= MIN_DEPTH) return null
  const sx = dot(offset, right) / depth / half
  const sy = dot(offset, up) / depth / (half * height / width)
  return { px: (sx + 1) / 2 * width - 0.5, py: (1 - sy) / 2 * height - 0.5 }
}

const AXES = ['x', 'y', 'z']

// the four corners of an axis-aligned face {axis, at, <other axis>: [lo, hi], ...}, each range shrunk by `inset` of its extent per side
const corners = (face, inset) => {
  const [a, b] = AXES.filter(k => k !== face.axis)
  const shrink = ([lo, hi]) => [lo + (hi - lo) * inset, hi - (hi - lo) * inset]
  const [aLo, aHi] = shrink(face[a])
  const [bLo, bHi] = shrink(face[b])
  return [[aLo, bLo], [aLo, bHi], [aHi, bLo], [aHi, bHi]].map(([u, v]) => ({ [face.axis]: face.at, [a]: u, [b]: v }))
}

// the pixel rectangle {x0, y0, x1, y1} (inclusive) inside the projected face, or null if any corner is behind the camera
export const faceRegion = (basis, eye, face, width, height, inset = 0.15) => {
  const points = corners(face, inset).map(p => projectPoint(basis, { x: p.x - eye.x, y: p.y - eye.y, z: p.z - eye.z }, width, height))
  if (points.some(p => p === null)) return null
  const xs = points.map(p => p.px)
  const ys = points.map(p => p.py)
  return { x0: Math.ceil(Math.min(...xs)), y0: Math.ceil(Math.min(...ys)), x1: Math.floor(Math.max(...xs)), y1: Math.floor(Math.max(...ys)) }
}
