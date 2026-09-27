// The air a body reads while it stands on dry-looking ground (card 962beec2: oxygen 18 -> 5 standing still at
// 119,72,-66 with a full bar of health, walks from there frozen with a valid path and air ahead). The air number is
// server-sent entity metadata (air_supply, 300 ticks = 20 bubbles), so a drain on dry land means the SERVER sees the
// head in water, either because the bridge hands the client another block there or because the server holds the body
// somewhere else than the client does. Every change of the number is one `oxygen` event with the evidence that tells
// those apart: the client's position against where the server last put it and how long ago, the block at the head,
// and whether the client thinks it is wet. Pure: bot.mjs reads the body and hands the numbers in.
//
// What bot.oxygenLevel reads before the first metadata: UNDEFINED. mineflayer never gives it a default; it is set only
// in lib/plugins/entities.js (`bot.oxygenLevel = Math.round(metas.air_supply / 15)`, or breath.js on old versions)
// when an entity_metadata packet for the body carries air_supply. The server sends only the metadata that differs
// from the default, and a full bar (300) is the default, so a body that has never been wet holds undefined for its
// whole life, and `./mc state` prints no oxygen at all. The first number a body ever sees is therefore the first
// change: `settling=true` marks the readings before the first 20 or the first damage, when the number and the health
// bar cannot yet be said to belong to the same, settled body.

// the memory a body starts with: no number, no health, not settled
export const freshAir = Object.freeze({ oxygen: undefined, health: undefined, settled: false })

const round2 = n => Math.round(n * 100) / 100
const cell = p => [p.x, p.y, p.z].map(round2).join(',')

// one reading against the last: the new memory, and the event to write when the number changed (else null)
export function airSample ({ memory, oxygen, health, client, server, now, head, inWater }) {
  const hurt = memory.health !== undefined && health < memory.health
  const settled = memory.settled || oxygen === 20 || hurt
  const next = { oxygen, health, settled }
  if (oxygen === undefined || oxygen === memory.oxygen) return { memory: next, event: null }
  return {
    memory: next,
    event: {
      oxygen,
      prev: memory.oxygen ?? null,
      health,
      client: cell(client),
      server: server ? cell(server) : null,
      serverAgeMs: server ? now - server.at : null,
      head,
      inWater,
      settling: !settled
    }
  }
}

// the state line: where the server last put the body, only when that is more than a block from the client AND the packet is
// under five seconds old. A `position` packet moves the client to the server's cell and the body walks on from there, so an old
// packet's distance is only how far it has walked since (Perrin: serverPos=30,66,108 age=404s standing at 111,70,-136), and
// the spawn packet, the only one most bodies ever get, would show for ever; under a minute it still showed as a desync after
// two walks (Jizo, 09-26 23:18Z: age=49s). A wedged or rubber-banded body is reset every tick: always fresh
export function serverPosNote ({ client, server, now, farBlocks = 1, freshMs = 5000 }) {
  if (!server || now - server.at > freshMs) return null
  const off = Math.hypot(server.x - client.x, server.y - client.y, server.z - client.z)
  if (off <= farBlocks) return null
  return { serverPos: cell(server), age: `${Math.round((now - server.at) / 1000)}s` }
}

// mineflayer 4.39.0 lib/plugins/entities.js: the air_supply in EVERY entity's metadata packet lands in bot.oxygenLevel
// (the old breath.js checked the entity id first; the move into entities.js dropped that). Beside a pond a body reads a
// glow squid's air (8) and a swimmer's (20) by turns while its own head is dry: the "drain on dry land" of card
// 962beec2 (Sancho at -145,60,-195: 41 air packets in a minute, none its own). The patch keeps only the body's own.
const BREATH_ANY = "      // Breathing (formerly in breath.js)\n      if (metas.air_supply != null) {\n        bot.oxygenLevel = Math.round(metas.air_supply / 15)\n"
const BREATH_OWN = "      // Breathing (formerly in breath.js)\n      if (metas.air_supply != null && entity === bot.entity) { // patched by bot/patch-deps.mjs: my own air, not every swimmer's\n        bot.oxygenLevel = Math.round(metas.air_supply / 15)\n"
export const patchOwnBreath = source => source.includes(BREATH_OWN)
  ? { status: 'already', source }
  : source.includes(BREATH_ANY) ? { status: 'patched', source: source.replace(BREATH_ANY, BREATH_OWN) } : { status: 'anchor missing', source }
