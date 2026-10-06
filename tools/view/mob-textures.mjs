// Why JavaScript: binary/graphics; reads the mobs' entity sheets from the client jar for the software renderers.
// The picture of a mob-model face (web/mob-models.mjs layer name `entity/<sheet>#x,y,w,h`) for renderer.mjs and raycaster.mjs: the region
// of the jar's sheet resampled to 16x16 (textures.mjs cropRegion), as the WebGL view's texture array has it. null when the jar or the
// sheet is missing, and the renderers draw the mob's flat colours then.
import { findClientJar, openJar } from './jar-read.mjs'
import { cropRegion, decodeTexture, parseEntityLayer, SIZE } from './textures.mjs'

// `jar`: an opened jar ({has, read}), or a function returning one (read on first use); default the newest client jar found
export const createMobImages = (jar = () => {
  const file = findClientJar()
  return file ? openJar(file) : null
}) => {
  let opened
  const sheets = new Map()
  const images = new Map()
  const sheetOf = name => {
    if (!sheets.has(name)) {
      opened ??= { jar: typeof jar === 'function' ? jar() : jar }
      const path = `assets/minecraft/textures/${name}.png`
      sheets.set(name, opened.jar?.has(path) ? decodeTexture(opened.jar.read(path)) : null)
    }
    return sheets.get(name)
  }
  return layer => {
    if (!images.has(layer)) {
      const { sheet, region } = parseEntityLayer(layer)
      const decoded = sheetOf(sheet)
      images.set(layer, decoded ? { width: SIZE, height: SIZE, rgba: cropRegion(decoded, region) } : null)
    }
    return images.get(layer)
  }
}

export const mobImage = createMobImages()
