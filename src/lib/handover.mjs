// Handing an item to a player (card 8c7b6652). `give count=101` tossed one stack of 64 and said taken=yes with the
// other 37 still in the pocket, because the toss was capped at the first stack found; and a toss from three blocks
// off lay where the player never came, read as "has not picked it up (full inventory...)". A give now tosses every
// stack until the count is met, names what it could not give, stands within arm's reach first, and says how far a
// drop lies from the player it was meant for.

// a thrown item flies about three blocks: from further than two it lands beyond the player, who never walks to it
export const GIVE_REACH = 2

// how many to toss, over every stack carried, and how many of those asked for are not there to give
export const givePlan = ({ count, carried }) => {
  const give = Math.min(count ?? carried, carried)
  return { give, short: Math.max(0, (count ?? carried) - carried) }
}

// the line that says what stayed behind and why, or null when the pocket held all that was asked for
export const shortNote = ({ item, asked, carried }) => asked > carried
  ? `asked for ${item}:${asked} but carried ${carried}: gave the ${carried}`
  : null

// a give from beyond arm's reach litters: refuse it with the distance, before anything is thrown
// (half a block of slack: the player stands somewhere inside their cell)
export const tooFarToGive = (player, distance) => distance > GIVE_REACH + 0.5
  ? `${player} is ${distance.toFixed(1)} blocks away, beyond arm's reach (${GIVE_REACH}): nothing given. Ask them to stand still, or use a chest`
  : null

// how many of a container's own slots are empty (slots: the window's slot array, containerSlots: how many of them are
// the container's, the rest being the pockets): a deposit plan can know a chest's room before it walks there
export const chestFree = (slots, containerSlots) => slots.slice(0, containerSlots).filter(s => !s).length

// where a drop still lies, and how far that is from the player it was thrown to
export const lyingFrom = (drop, player, name) => {
  const cell = `${Math.floor(drop.x)},${Math.floor(drop.y)},${Math.floor(drop.z)}`
  if (!player) return `${cell} (${name} out of sight)`
  const distance = Math.hypot(drop.x - player.x, drop.y - player.y, drop.z - player.z)
  const blocks = distance < 0.05 ? '0' : distance.toFixed(1)
  return `${cell} (${blocks} blocks from ${name})`
}
