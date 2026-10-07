// Why JavaScript: reads prismarine-block collision shapes (Mineflayer boundary), shared by raw-world and prim-sense.
// Collision-shape facts about a prismarine block.

// a collision shape that fills the whole cell: what a head can be stuck in (slabs, farmland, crops, carpets are not)
export const fullCube = block => block.boundingBox === 'block' && (block.shapes ?? []).some(([x0, y0, z0, x1, y1, z1]) => x0 <= 0 && y0 <= 0 && z0 <= 0 && x1 >= 1 && y1 >= 1 && z1 >= 1)

// the highest point of a solid collision shape above the cell's floor (1 for a block, 0.5 for a bottom slab, 1.5 for a fence); 0 when none
export const shapeTop = block => block.boundingBox === 'block' ? Math.max(0, ...(block.shapes ?? []).map(s => s[4])) : 0
