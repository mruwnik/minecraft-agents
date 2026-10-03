import test from 'node:test'
import assert from 'node:assert/strict'
import { buildCommand, targetsFromEnv } from '../tools/rcon-test.mjs'

const T = 'ClaudeProbe'

const allowed = [
  [['list'], ['list']],
  [['time', 'day'], ['time set day']],
  [['time', 'night'], ['time set night']],
  [['time', 'midnight'], ['time set midnight']],
  [['time', 'noon'], ['time set noon']],
  [['time', '6000'], ['time set 6000']],
  [['weather', 'clear'], ['weather clear']],
  [['weather', 'rain'], ['weather rain']],
  [['weather', 'thunder'], ['weather thunder']],
  [['tp', T, '10', '64', '-5.5'], [`tp ${T} 10 64 -5.5`]],
  [['give', T, 'bread'], [`give ${T} minecraft:bread 1`]],
  [['give', T, 'iron_sword', '64'], [`give ${T} minecraft:iron_sword 64`]],
  [['clear', T], [`clear ${T}`]],
  [['effect', T, 'poison'], [`effect give ${T} minecraft:poison 30 0`]],
  [['effect', T, 'wither', '120', '4'], [`effect give ${T} minecraft:wither 120 4`]],
  [['effect-clear', T], [`effect clear ${T}`]],
  [['damage', T, '4'], [`damage ${T} 4`]],
  [['damage', T, '20'], [`damage ${T} 20`]],
  [['heal', T], [`effect give ${T} minecraft:instant_health 1 10`]],
  [['feed', T], [`effect give ${T} minecraft:saturation 1 10`]],
  [['fire', T], [`damage ${T} 1 minecraft:on_fire`]],
  [['summon', 'zombie', '1', '64', '2'], ['summon minecraft:zombie 1 64 2']],
  [['summon', 'cow', '1', '64', '2', '3'], Array(3).fill('summon minecraft:cow 1 64 2')],
  [['kill-mobs', '0', '64', '0', '16'], ['kill @e[type=!player,type=!item,x=0,y=64,z=0,distance=..16]']],
  [['setblock', '1', '2', '3', 'stone'], ['setblock 1 2 3 minecraft:stone']],
  [['fill', '0', '0', '0', '7', '7', '6', 'glass'], ['fill 0 0 0 7 7 6 minecraft:glass']],
  [['fill', '5', '5', '5', '0', '0', '0', 'air'], ['fill 5 5 5 0 0 0 minecraft:air']]
]

for (const [argv, expected] of allowed) {
  test(`builds ${argv.join(' ')}`, () => {
    assert.deepEqual(buildCommand(argv, { targets: [T] }), expected)
  })
}

test('extra target is accepted via the targets option', () => {
  assert.deepEqual(buildCommand(['clear', 'Bob_1'], { targets: [T, 'Bob_1'] }), ['clear Bob_1'])
})

const refused = [
  [['gamemode', 'creative', T], /unknown or forbidden subcommand/],
  [['op', T], /unknown or forbidden subcommand/],
  [['deop', T], /unknown or forbidden subcommand/],
  [['stop'], /unknown or forbidden subcommand/],
  [['execute', 'as', T], /unknown or forbidden subcommand/],
  [['gamerule', 'x'], /unknown or forbidden subcommand/],
  [['nonsense'], /unknown or forbidden subcommand/],
  [[], /unknown or forbidden subcommand/],
  [['clear', 'Mallory'], /ClaudeProbe/],
  [['tp', 'Mallory', '1', '2', '3'], /not an allowed target.*ClaudeProbe/],
  [['tp', T, 'abc', '1', '2'], /number/],
  [['tp', T, '1,5', '1', '2'], /number/],
  [['tp', T, '~', '1', '2'], /number/],
  [['summon', 'zombie', '^', '1', '2'], /number/],
  [['setblock', '~1', '2', '3', 'stone'], /number/],
  [['tp', T, '1', '2'], /usage/],
  [['give', T, 'command_block'], /not allowed/],
  [['give', T, 'bread', '65'], /count/],
  [['give', T, 'bread', '0'], /count/],
  [['summon', 'zombie', '1', '2', '3', '6'], /count/],
  [['summon', 'item', '1', '2', '3'], /not allowed/],
  [['summon', 'armor_stand', '1', '2', '3'], /not allowed/],
  [['fill', '0', '0', '0', '7', '7', '7', 'stone'], /volume/],
  [['fill', '0', '0', '0', '1.5', '1', '1', 'stone'], /integer/],
  [['setblock', '1', '2', '3', 'bedrock'], /not allowed/],
  [['kill-mobs', '0', '0', '0', '33'], /radius/],
  [['kill-mobs', '0', '0', '0', '5', '@e'], /usage/],
  [['kill-mobs', '@e', '0', '0', '5'], /number/],
  [['effect', T, 'poison', '121'], /seconds/],
  [['effect', T, 'poison', '10', '5'], /amplifier/],
  [['effect', T, 'speed'], /not allowed/],
  [['damage', T, '21'], /amount/],
  [['fire', T, '5'], /usage/],
  [['time', 'dusk'], /time/],
  [['weather', 'snow'], /weather/]
]

for (const [argv, pattern] of refused) {
  test(`refuses ${argv.join(' ') || '(nothing)'}`, () => {
    assert.throws(() => buildCommand(argv, { targets: [T] }), pattern)
  })
}

const envCases = [
  [undefined, ['ClaudeProbe']],
  ['', ['ClaudeProbe']],
  ['Bob_1', ['ClaudeProbe', 'Bob_1']],
  ['Bob_1, Carol', ['ClaudeProbe', 'Bob_1', 'Carol']]
]

for (const [value, expected] of envCases) {
  test(`targetsFromEnv ${JSON.stringify(value)}`, () => {
    assert.deepEqual(targetsFromEnv({ RCON_TEST_TARGETS: value }), expected)
  })
}

for (const bad of ['ab', 'has space', 'Bob;op', 'Bob,@a']) {
  test(`targetsFromEnv rejects ${JSON.stringify(bad)}`, () => {
    assert.throws(() => targetsFromEnv({ RCON_TEST_TARGETS: bad }), /not a valid player name/)
  })
}
