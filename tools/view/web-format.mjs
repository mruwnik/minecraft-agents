// The numbers the browser decoder needs about a column's wire format, as prismarine-chunk's 1.18 ChunkColumn works them out
// for a version (see decodeSections in web/decode.mjs). numSections comes from the file header's worldHeight (>> 4).
import { makeChunkClass } from './columns.mjs'

export const columnFormat = version => {
  const Chunk = makeChunkClass(version)
  const probe = new Chunk()
  return {
    noSizePrefix: Chunk.registry.version['>=']('1.21.5'),
    hasFluidCount: Chunk.registry.version['>=']('26.1'),
    maxBitsPerBlock: probe.maxBitsPerBlock,
    maxBitsPerBiome: probe.maxBitsPerBiome
  }
}
