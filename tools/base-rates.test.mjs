// Why JavaScript: node --test file for tools/base-rates.mjs (a Node tool over the server jar).
import test from 'node:test'
import assert from 'node:assert/strict'
import { oreRates } from './base-rates.mjs'

const ore = (block, size, air) => ({
  type: 'minecraft:ore',
  config: {
    size,
    discard_chance_on_air_exposure: air,
    targets: [{ target: { tag: 'minecraft:stone_ore_replaceables' }, state: { Name: `minecraft:${block}` } },
      { target: { tag: 'minecraft:deepslate_ore_replaceables' }, state: { Name: `minecraft:deepslate_${block}` } }]
  }
})
const placed = (feature, count, height) => ({
  feature: `minecraft:${feature}`,
  placement: [...count, { type: 'minecraft:in_square' }, { type: 'minecraft:height_range', height }, { type: 'minecraft:biome' }]
})
const count = n => [{ type: 'minecraft:count', count: n }]
const features = (...names) => ({ features: [[], names.map(n => `minecraft:${n}`)] })

const worldgen = {
  version: '26.2',
  dimension: { min_y: -64, height: 384 },
  configured: { ore_iron: ore('iron_ore', 9), ore_emerald: ore('emerald_ore', 3), ore_gold_buried: ore('gold_ore', 9, 0.5), ore_dirt: ore('dirt', 33) },
  placed: {
    ore_iron_middle: placed('ore_iron', count(10), { type: 'minecraft:trapezoid', min_inclusive: { absolute: -24 }, max_inclusive: { absolute: 56 } }),
    ore_iron_small: placed('ore_iron', count(10), { type: 'minecraft:uniform', min_inclusive: { above_bottom: 0 }, max_inclusive: { below_top: 8 } }),
    ore_emerald: placed('ore_emerald', count(100), { type: 'minecraft:trapezoid', min_inclusive: { absolute: -16 }, max_inclusive: { absolute: 480 } }),
    ore_gold_lower: placed('ore_gold_buried', count({ type: 'minecraft:uniform', min_inclusive: 0, max_inclusive: 1 }),
      { type: 'minecraft:uniform', min_inclusive: { absolute: -64 }, max_inclusive: { absolute: -48 } }),
    ore_gold_rare: placed('ore_gold_buried', [{ type: 'minecraft:rarity_filter', chance: 4 }],
      { type: 'minecraft:uniform', min_inclusive: { absolute: 0 }, max_inclusive: { absolute: 8 } }),
    ore_dirt: placed('ore_dirt', count(7), { type: 'minecraft:uniform', min_inclusive: { absolute: 0 }, max_inclusive: { absolute: 160 } })
  },
  biomes: {
    plains: features('ore_dirt', 'ore_iron_middle', 'ore_iron_small', 'ore_gold_lower', 'ore_gold_rare'),
    meadow: features('ore_iron_middle', 'ore_iron_small', 'ore_emerald', 'ore_gold_lower', 'ore_gold_rare'),
    the_end: features()
  },
  tags: {
    stone_ore_replaceables: { values: ['minecraft:stone', '#minecraft:other_stone'] },
    other_stone: { values: ['minecraft:granite'] },
    deepslate_ore_replaceables: { values: ['minecraft:deepslate'] }
  }
}

test('oreRates: each wanted ore gets its placements, with heights made absolute, counts as means and its air discard, and the blocks it replaces', () => {
  assert.deepEqual(oreRates(worldgen, ['iron', 'gold', 'emerald']), {
    version: '26.2',
    biomes: ['meadow', 'plains'],
    ores: {
      iron: [
        { feature: 'ore_iron_middle', count: 10, size: 9, shape: 'trapezoid', min: -24, max: 56, air: 0 },
        { feature: 'ore_iron_small', count: 10, size: 9, shape: 'uniform', min: -64, max: 311, air: 0 }
      ],
      gold: [
        { feature: 'ore_gold_lower', count: 0.5, size: 9, shape: 'uniform', min: -64, max: -48, air: 0.5 },
        { feature: 'ore_gold_rare', count: 0.25, size: 9, shape: 'uniform', min: 0, max: 8, air: 0.5 }
      ],
      emerald: [
        { feature: 'ore_emerald', count: 100, size: 3, shape: 'trapezoid', min: -16, max: 480, air: 0, biomes: ['meadow'] }
      ]
    },
    hosts: { iron: ['deepslate', 'granite', 'stone'], gold: ['deepslate', 'granite', 'stone'], emerald: ['deepslate', 'granite', 'stone'] }
  })
})

test('oreRates refuses a placement step it cannot price', () => {
  const odd = structuredClone(worldgen)
  odd.placed.ore_iron_small.placement.push({ type: 'minecraft:surface_water_depth_filter', max_water_depth: 0 })
  assert.throws(() => oreRates(odd, ['iron']), /ore_iron_small.*surface_water_depth_filter/)
})
