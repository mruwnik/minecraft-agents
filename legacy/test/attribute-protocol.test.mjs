import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { createRequire } from 'node:module'
import { ATTRIBUTE_IDS_26_1, patchAttributeProtocol } from '../src/navigation/attribute-protocol.mjs'
const require = createRequire(import.meta.url)
const data = require('minecraft-data')('26.1')
const protocol = require('minecraft-protocol')

test('negotiated attribute table matches complete registry, including inserted IDs', () => {
  assert.deepEqual(ATTRIBUTE_IDS_26_1, data.attributesArray.map(attribute => attribute.resource))
  assert.equal(ATTRIBUTE_IDS_26_1[22], 'minecraft:movement_speed')
  assert.equal(ATTRIBUTE_IDS_26_1[25], 'minecraft:scale')
})

test('attribute patch is idempotent and refuses unknown mappings', () => {
  const source = fs.readFileSync(new URL('../../node_modules/minecraft-data/minecraft-data/data/pc/26.1/protocol.json', import.meta.url), 'utf8')
  const schema = JSON.parse(source)
  const mapper = schema.play.toClient.types.packet_entity_update_attributes[1].find(f => f.name === 'properties').type[1].type[1].find(f => f.name === 'key').type[1]
  mapper.mappings = {"0": "generic.armor", "1": "generic.armor_toughness", "2": "generic.attack_damage", "3": "generic.attack_knockback", "4": "generic.attack_speed", "5": "player.block_break_speed", "6": "player.block_interaction_range", "7": "burning_time", "8": "camera_distance", "9": "explosion_knockback_resistance", "10": "player.entity_interaction_range", "11": "generic.fall_damage_multiplier", "12": "generic.flying_speed", "13": "generic.follow_range", "14": "generic.gravity", "15": "generic.jump_strength", "16": "generic.knockback_resistance", "17": "generic.luck", "18": "generic.max_absorption", "19": "generic.max_health", "20": "generic.movement_speed", "21": "generic.safe_fall_distance", "22": "generic.scale", "23": "zombie.spawn_reinforcements", "24": "generic.step_height", "25": "submerged_mining_speed", "26": "sweeping_damage_ratio", "27": "tempt_range", "28": "water_movement_efficiency", "29": "waypoint_transmit_range", "30": "waypoint_receive_range"}
  const result = patchAttributeProtocol(JSON.stringify(schema))
  assert.equal(result.status, 'patched')
  assert.equal(patchAttributeProtocol(result.source).status, 'already patched')
  mapper.mappings[22] = 'unknown'
  assert.equal(patchAttributeProtocol(JSON.stringify(schema)).status, 'anchor missing')
})

test('real encoded packet keeps movement, scale and all 35 attributes distinct', () => {
  const serializer = protocol.createSerializer({ state: 'play', isServer: true, version: '26.1' })
  const deserializer = protocol.createDeserializer({ state: 'play', version: '26.1' })
  const properties = ATTRIBUTE_IDS_26_1.map((key, id) => ({ key, value: id === 22 ? 0.18432864220812917 : id === 25 ? 1 : id, modifiers: [] }))
  const buffer = serializer.createPacketBuffer({ name: 'entity_update_attributes', params: { entityId: 102386, properties } })
  const decoded = deserializer.parsePacketBuffer(buffer).data.params
  assert.deepEqual(decoded.properties, properties)
})
