// The cell a walk should aim at to reach a dropped item, not the block it rests on. An item's y already floors to
// the air cell above whatever holds it up when that surface is a full block (its top sits at blockY+1, so
// floor(entityY) lands on the cell above it), but a shorter surface floors to the surface block itself: farmland is
// 15/16 high, a slab half that, so an item resting on either has floor(entityY) equal to the FARMLAND or SLAB cell,
// not the air above it. The standing guard added in 31416fe correctly refuses that cell ("nowhere to stand within 0
// of x,y,z: it is farmland, a block; aim at the cell above it, or pass range=1"), so a walk aimed there with range=0
// finds nothing to land on and collect stops early (card 21657b89: stander, twice in a row, every wheat drop on
// farmland). Range=1 reaches the cell above a short surface, but when a crop occupies it the adjacent lane is
// diagonally above the old goal and OUTSIDE that range. Read the surface when available and aim one cell above a
// solid block: neighboring feet cells are then valid pickup positions without standing in or breaking the crop.
// Unloaded positions keep their original goal until the walk brings the world into sight.
export function dropGoal (pos, cellAt = () => null) {
  const goal = { x: Math.floor(pos.x), y: Math.floor(pos.y), z: Math.floor(pos.z), range: 1 }
  if (cellAt(goal.x, goal.y, goal.z)?.solid) goal.y++
  return goal
}
