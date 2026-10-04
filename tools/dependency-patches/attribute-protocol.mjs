// Why JavaScript: patches protocol handling in JS dependency sources in node_modules (Mineflayer boundary).
// ViaVersion 5.12 identifiers-26.1.nbt uses identity IDs 0..34 from its
// identifier-table.nbt. These match the negotiated 26.1 attribute registry;
// Paper 26.2 has a different table and must not be used for bridged packets.
export const ATTRIBUTE_IDS_26_1 = Object.freeze([
  'armor', 'armor_toughness', 'attack_damage', 'attack_knockback', 'attack_speed',
  'block_break_speed', 'block_interaction_range', 'burning_time', 'camera_distance',
  'explosion_knockback_resistance', 'entity_interaction_range', 'fall_damage_multiplier',
  'flying_speed', 'follow_range', 'gravity', 'jump_strength', 'knockback_resistance',
  'luck', 'max_absorption', 'max_health', 'mining_efficiency', 'movement_efficiency',
  'movement_speed', 'oxygen_bonus', 'safe_fall_distance', 'scale', 'sneaking_speed',
  'spawn_reinforcements', 'step_height', 'submerged_mining_speed', 'sweeping_damage_ratio',
  'tempt_range', 'water_movement_efficiency', 'waypoint_transmit_range', 'waypoint_receive_range'
].map(name => `minecraft:${name}`))

export function patchAttributeProtocol (source) {
  const schema = JSON.parse(source)
  const packet = schema.play?.toClient?.types?.packet_entity_update_attributes
  const properties = packet?.[1]?.find(field => field.name === 'properties')
  const key = properties?.type?.[1]?.type?.[1]?.find(field => field.name === 'key')
  if (key?.type?.[0] !== 'mapper' || key.type[1].type !== 'varint') return { status: 'anchor missing', source }
  const wanted = Object.fromEntries(ATTRIBUTE_IDS_26_1.map((name, id) => [id, name]))
  if (JSON.stringify(key.type[1].mappings) === JSON.stringify(wanted)) return { status: 'already patched', source }
  // Refuse unknown dependency tables rather than silently remapping a future
  // protocol. The shipped stale table identifies speed20 and scale22.
  const old = key.type[1].mappings
  if (Object.keys(old).length !== 31 || old[20] !== 'generic.movement_speed' || old[22] !== 'generic.scale') return { status: 'anchor missing', source }
  key.type[1].mappings = wanted
  return { status: 'patched', source: JSON.stringify(schema, null, 2) + '\n' }
}
