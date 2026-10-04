import test from 'node:test'
import assert from 'node:assert/strict'
import { clientVersions, versionsDirs, jarTextures } from '../tools/jar.mjs'

// ---------------------------------------------------------------- the client jar and its block textures
// textures/ is not checked in: tools/textures.mjs extracts it from a client jar when a body starts without it
for (const [name, dirs, expected] of [
  ['newest first, and numerically: 1.10 is newer than 1.9', ['1.9', '1.10', '1.21.8'], ['1.21.8', '1.10', '1.9']],
  ['a bare version is older than the same version with a patch', ['1.19', '1.19.2'], ['1.19.2', '1.19']],
  ['OptiFine builds are somebody else\'s jar', ['1.16.5-OptiFine_HD_U_G8', '1.16.5'], ['1.16.5']],
  ['pre-releases and release candidates are not releases', ['1.21.4-pre1', '1.21.4-rc3', '1.21.4'], ['1.21.4']],
  ['loader folders hold no client jar of their own', ['fabric-loader-0.16.7-1.21.1', 'iris-fabric-loader-0.16.7-1.21.1', '1.21.1'], ['1.21.1']],
  ['snapshots are skipped', ['23w31a', '1.21'], ['1.21']],
  ['nothing usable', ['fabric-loader-0.16.7-1.21.1'], []],
  ['no versions installed at all', [], []]
]) {
  test(`clientVersions: ${name}`, () => assert.deepEqual(clientVersions(dirs), expected))
}

for (const [name, platform, expected] of [
  ['the Mac launcher keeps its jars under Library, with the plain dot folder as a fallback', 'darwin', ['/h/Library/Application Support/minecraft/versions', '/h/.minecraft/versions']],
  ['elsewhere only the dot folder', 'linux', ['/h/.minecraft/versions']]
]) {
  test(`versionsDirs: ${name}`, () => assert.deepEqual(versionsDirs('/h', platform), expected))
}

for (const [name, kind, entries, expected] of [
  ['a block texture is taken', 'block', ['assets/minecraft/textures/block/dirt.png'], ['assets/minecraft/textures/block/dirt.png']],
  ['an item texture is taken', 'item', ['assets/minecraft/textures/item/apple.png'], ['assets/minecraft/textures/item/apple.png']],
  ['items, entities and the gui are not blocks', 'block', ['assets/minecraft/textures/item/apple.png', 'assets/minecraft/textures/entity/creeper.png', 'assets/minecraft/textures/gui/bars.png'], []],
  ['blocks, entities and the gui are not items', 'item', ['assets/minecraft/textures/block/dirt.png', 'assets/minecraft/textures/entity/creeper.png', 'assets/minecraft/textures/gui/bars.png'], []],
  ['animation metadata is not a texture', 'block', ['assets/minecraft/textures/block/water_still.png.mcmeta'], []],
  ['the folder entry itself is not a texture', 'item', ['assets/minecraft/textures/item/'], []],
  ['a subfolder would be flattened onto a sibling\'s name', 'item', ['assets/minecraft/textures/item/trims/apple.png'], []],
  ['another namespace is not ours', 'block', ['assets/create/textures/block/andesite.png'], []],
  ['the order of the jar is kept', 'item', ['assets/minecraft/textures/item/stick.png', 'pack.mcmeta', 'assets/minecraft/textures/item/apple.png'],
    ['assets/minecraft/textures/item/stick.png', 'assets/minecraft/textures/item/apple.png']]
]) {
  test(`jarTextures: ${name}`, () => assert.deepEqual(jarTextures(entries, kind), expected))
}
