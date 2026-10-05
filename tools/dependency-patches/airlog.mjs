// Why JavaScript: patches Mineflayer's own JS source in node_modules (Mineflayer boundary).
// mineflayer 4.39.0 lib/plugins/entities.js: the air_supply in EVERY entity's metadata packet lands in bot.oxygenLevel
// (the old breath.js checked the entity id first). Beside a pond the body reads a squid's or swimmer's air by turns
// while its own head is dry, which looks like drowning on dry land. The patch keeps only the body's own.
const BREATH_ANY = "      // Breathing (formerly in breath.js)\n      if (metas.air_supply != null) {\n        bot.oxygenLevel = Math.round(metas.air_supply / 15)\n"
const BREATH_OWN = "      // Breathing (formerly in breath.js)\n      if (metas.air_supply != null && entity === bot.entity) { // patched by bot/patch-deps.mjs: my own air, not every swimmer's\n        bot.oxygenLevel = Math.round(metas.air_supply / 15)\n"
export const patchOwnBreath = source => source.includes(BREATH_OWN)
  ? { status: 'already', source }
  : source.includes(BREATH_ANY) ? { status: 'patched', source: source.replace(BREATH_ANY, BREATH_OWN) } : { status: 'anchor missing', source }
