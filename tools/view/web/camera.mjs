// Why JavaScript: WebGL/browser; camera data a shader takes, browser-safe with no imports.
// The camera of src/vision/renderer.mjs (cameraFor, non-panorama) as data a shader can take. Browser-safe, no imports.
// mineflayer's convention: yaw 0 faces north (-z) and grows turning left; pitch > 0 looks up.
export const directionFor = (yaw, pitch) => ({
  x: -Math.sin(yaw) * Math.cos(pitch),
  y: Math.sin(pitch),
  z: -Math.cos(yaw) * Math.cos(pitch)
})

const cross = (a, b) => ({ x: a.y * b.z - a.z * b.y, y: a.z * b.x - a.x * b.z, z: a.x * b.y - a.y * b.x })

// fov is the horizontal field of view in degrees; `half` is the tangent of half of it
export const cameraBasis = ({ yaw = 0, pitch = 0, fov = 90 }) => {
  const forward = directionFor(yaw, pitch)
  const right = directionFor(yaw - Math.PI / 2, 0)
  return { forward, right, up: cross(right, forward), half: Math.tan(fov * Math.PI / 360) }
}

// the unit direction of pixel (px, py) in a width x height image
export const rayDir = ({ forward, right, up, half }, px, py, width, height) => {
  const sx = ((px + 0.5) / width * 2 - 1) * half
  const sy = (1 - (py + 0.5) / height * 2) * half * height / width
  const x = forward.x + right.x * sx + up.x * sy
  const y = forward.y + right.y * sx + up.y * sy
  const z = forward.z + right.z * sx + up.z * sy
  const len = Math.hypot(x, y, z)
  return { x: x / len, y: y / len, z: z / len }
}
