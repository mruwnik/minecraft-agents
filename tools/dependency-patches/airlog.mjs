// mineflayer 4.39.0 lib/plugins/entities.js: the air_supply in EVERY entity's metadata packet lands in bot.oxygenLevel
// (the old breath.js checked the entity id first; the move into entities.js dropped that). Beside a pond a body reads a
// glow squid's air (8) and a swimmer's (20) by turns while its own head is dry: the "drain on dry land" of card
// 962beec2 (Sancho at -145,60,-195: 41 air packets in a minute, none its own). The patch keeps only the body's own.
const BREATH_ANY = "      // Breathing (formerly in breath.js)\n      if (metas.air_supply != null) {\n        bot.oxygenLevel = Math.round(metas.air_supply / 15)\n"
const BREATH_OWN = "      // Breathing (formerly in breath.js)\n      if (metas.air_supply != null && entity === bot.entity) { // patched by bot/patch-deps.mjs: my own air, not every swimmer's\n        bot.oxygenLevel = Math.round(metas.air_supply / 15)\n"
export const patchOwnBreath = source => source.includes(BREATH_OWN)
  ? { status: 'already', source }
  : source.includes(BREATH_ANY) ? { status: 'patched', source: source.replace(BREATH_ANY, BREATH_OWN) } : { status: 'anchor missing', source }
