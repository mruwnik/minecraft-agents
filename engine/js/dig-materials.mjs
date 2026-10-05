// Why JavaScript: a patch of Mineflayer's minecraft-data registry (the Mineflayer boundary), applied by connect.mjs.
//
// minecraft-data (1.21.x and later) gives obsidian, every ore, the metal blocks, copper and a few others the material
// `incorrect_for_<tier>_tool`, whose speed table names only the tier's own four tools. prismarine-block's digTime then
// finds no speed for a diamond pickaxe on obsidian and counts hand speed: 75 s instead of 9.4 s, and mineflayer waits
// that long before it sends the finish of the dig. All of these blocks are mined with a pickaxe, so they get the
// pickaxe material. What a tool may harvest does not come from the material (see the harvestTools primitive).
const PICKAXE_MATERIAL = 'mineable/pickaxe'

export function fixDigMaterials (registry) {
  for (const block of registry.blocksArray) {
    if (block.material?.startsWith('incorrect_for_')) block.material = PICKAXE_MATERIAL
  }
}
