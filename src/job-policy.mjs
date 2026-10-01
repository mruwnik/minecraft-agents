// Actions in this exact allowlist neither move the body nor change its inventory, controls,
// reflex policy, work authorization, or world state. Everything else is serialized as a job.
export const CONCURRENT_READ_ACTIONS = new Set([
  'help', 'state', 'look', 'block_at', 'find_blocks', 'scan', 'entity', 'inventory', 'screen', 'look_around', 'animals', 'places', 'zones',
  'boat_state', 'cart_state', 'horse_state', 'path_to', 'events', 'chat', 'whisper'
])

export const mayRunBesideOwner = (name, { quick, long }) => CONCURRENT_READ_ACTIONS.has(name) && typeof quick?.[name] === 'function' && typeof long?.[name] !== 'function'
