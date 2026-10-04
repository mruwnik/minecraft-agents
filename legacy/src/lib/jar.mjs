// The client jar: where the launcher keeps it, known release order and reading its block texture atlas.
import path from 'node:path'

// ---------------------------------------------------------------- the client jar (textures/ is not checked in)
// The version folders under ~/.minecraft/versions that hold a plain client jar, newest first. A name that is not
// only digits and dots belongs to something else: OptiFine builds, pre-releases, release candidates, snapshots and
// loader folders all sit beside the releases, and none of them is the jar tools/textures.mjs is looking for.
// The launcher's versions folders, best first: ~/.minecraft everywhere, but the Mac launcher puts it under Library.
export const versionsDirs = (home, platform) => [
  ...(platform === 'darwin' ? [path.join(home, 'Library/Application Support/minecraft/versions')] : []),
  path.join(home, '.minecraft/versions')
]

const RELEASE = /^\d+(\.\d+)*$/
const versionOrder = (a, b) => {
  const [x, y] = [a.split('.').map(Number), b.split('.').map(Number)]
  return [...Array(Math.max(x.length, y.length)).keys()].map(i => (y[i] ?? 0) - (x[i] ?? 0)).find(d => d !== 0) ?? 0
}
export const clientVersions = dirs => dirs.filter(d => RELEASE.test(d)).sort(versionOrder)

// The block (or item) textures inside a client jar, in the order the jar lists them. Everything else in there is
// somebody else's business: entities, the gui, the animation metadata beside a png, and other namespaces. Only the
// folder's own files: tools/textures.mjs writes each under its bare name, so a subfolder's would overwrite a sibling.
const TEXTURE_FILE = kind => new RegExp(`^assets/minecraft/textures/${kind}/[^/]+\\.png$`)
export const jarTextures = (names, kind) => names.filter(n => TEXTURE_FILE(kind).test(n))
