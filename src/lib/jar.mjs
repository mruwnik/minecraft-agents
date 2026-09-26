// The client jar: known release order and reading its block texture atlas.

// ---------------------------------------------------------------- the client jar (textures/ is not checked in)
// The version folders under ~/.minecraft/versions that hold a plain client jar, newest first. A name that is not
// only digits and dots belongs to something else: OptiFine builds, pre-releases, release candidates, snapshots and
// loader folders all sit beside the releases, and none of them is the jar tools/textures.mjs is looking for.
const RELEASE = /^\d+(\.\d+)*$/
const versionOrder = (a, b) => {
  const [x, y] = [a.split('.').map(Number), b.split('.').map(Number)]
  return [...Array(Math.max(x.length, y.length)).keys()].map(i => (y[i] ?? 0) - (x[i] ?? 0)).find(d => d !== 0) ?? 0
}
export const clientVersions = dirs => dirs.filter(d => RELEASE.test(d)).sort(versionOrder)

// The block textures inside a client jar, in the order the jar lists them. Everything else in there is somebody
// else's business: items, entities, the gui, the animation metadata beside a png, and other namespaces.
export const BLOCK_TEXTURES = 'assets/minecraft/textures/block/'
export const blockTextures = names => names.filter(n => n.startsWith(BLOCK_TEXTURES) && n.endsWith('.png'))
