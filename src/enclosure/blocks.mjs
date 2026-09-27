export const cellKey = p => `${p.x},${p.y},${p.z}`
// The API's generic solid flag also covers some partial collision boxes.
// Those cannot serve as a flat floor or a sealed nursery wall.
export const safeFullBlock = b => b?.solid && !/(?:_slab|_stairs|_fence|_fence_gate|_wall|_pane|_bed|_door|_trapdoor|_leaves|_chest|shulker_box)$|^(?:magma_block|cactus|campfire|soul_campfire|farmland|dirt_path|soul_sand|snow|powder_snow|honey_block|scaffolding|lectern|anvil|chipped_anvil|damaged_anvil|enchanting_table|end_portal_frame|stonecutter|lily_pad|cobweb)$/.test(b.name)
export const buildingMaterial = name => /^(cobblestone|cobbled_deepslate|stone|(?:oak|spruce|birch|jungle|acacia|dark_oak|mangrove|cherry|pale_oak|bamboo|crimson|warped)_planks)$/.test(name)
export const woodenGate = name => /^(oak|spruce|birch|jungle|acacia|dark_oak|mangrove|cherry|pale_oak|bamboo|crimson|warped)_fence_gate$/.test(name)
