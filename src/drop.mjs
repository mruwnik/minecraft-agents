// The cell a walk should aim at to reach a dropped item, not the block it rests on. An item's y already floors to
// the air cell above whatever holds it up when that surface is a full block (its top sits at blockY+1, so
// floor(entityY) lands on the cell above it), but a shorter surface floors to the surface block itself: farmland is
// 15/16 high, a slab half that, so an item resting on either has floor(entityY) equal to the FARMLAND or SLAB cell,
// not the air above it. The standing guard added in 31416fe correctly refuses that cell ("nowhere to stand within 0
// of x,y,z: it is farmland, a block; aim at the cell above it, or pass range=1"), so a walk aimed there with range=0
// finds nothing to land on and collect stops early (card 21657b89: stander, twice in a row, every wheat drop on
// farmland). The fix is range=1, not a smarter y: it lets the walk land on any of the 6 face-adjacent cells too, so
// the cell above a farmland or slab surface is in reach without this helper needing to know what block is there -
// and a drop that already floors correctly (an ordinary block, a water cell it floats in) keeps its own cell first,
// since the pathfinder tries the nearest cell before its neighbours.
export function dropGoal (pos) {
  return { x: Math.floor(pos.x), y: Math.floor(pos.y), z: Math.floor(pos.z), range: 1 }
}
