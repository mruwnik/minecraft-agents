// Why JavaScript: WebGL/binary; reorders biome ids for the GPU in the browser.
// Biome ids of a dumped column come from decodeSections (decode.mjs); this only reorders them for the GPU.

// section order to a 3D texture of 4 x (numSections * 4) x 4 cells: index (z4 * (numSections * 4) + y4global) * 4 + x4
export function biomeTextureOrder (ids, numSections, out = new Uint8Array(ids.length)) {
  const height = numSections * 4
  for (let i = 0; i < ids.length; i++) {
    const y = (i >> 6) * 4 + ((i >> 4) & 3)
    out[(((i >> 2) & 3) * height + y) * 4 + (i & 3)] = ids[i]
  }
  return out
}
