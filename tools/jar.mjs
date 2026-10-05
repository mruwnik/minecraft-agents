// Why JavaScript: binary data; locates the client jar and reads its block texture atlas.
// Finds the client jar (launcher folders, release order) and picks its block and item textures.
import path from 'node:path'

// The launcher's versions folders, best first: ~/.minecraft everywhere, but the Mac launcher puts it under Library.
export const versionsDirs = (home, platform) => [
  ...(platform === 'darwin' ? [path.join(home, 'Library/Application Support/minecraft/versions')] : []),
  path.join(home, '.minecraft/versions')
]

// Only plain release names (digits and dots) count: OptiFine builds, pre-releases, snapshots and loader folders sit
// beside them and are not the jar tools/textures.mjs wants. Newest first.
const RELEASE = /^\d+(\.\d+)*$/
const versionOrder = (a, b) => {
  const [x, y] = [a.split('.').map(Number), b.split('.').map(Number)]
  return [...Array(Math.max(x.length, y.length)).keys()].map(i => (y[i] ?? 0) - (x[i] ?? 0)).find(d => d !== 0) ?? 0
}
export const clientVersions = dirs => dirs.filter(d => RELEASE.test(d)).sort(versionOrder)

// The block (or item) textures inside a client jar, in jar order. Only the folder's own png files: textures.mjs writes
// each under its bare name, so a subfolder's would overwrite a sibling.
const TEXTURE_FILE = kind => new RegExp(`^assets/minecraft/textures/${kind}/[^/]+\\.png$`)
export const jarTextures = (names, kind) => names.filter(n => TEXTURE_FILE(kind).test(n))
