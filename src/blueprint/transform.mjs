// Rotate local coordinates in a rectangular footprint. Anchors remain world-space.
export function rotateBlueprintPosition (at, width, depth, turns) {
  let [x, y, z] = at
  for (let n = 0; n < ((turns % 4) + 4) % 4; n++) {
    [x, z, width, depth] = [depth - 1 - z, x, depth, width]
  }
  return [x, y, z]
}
