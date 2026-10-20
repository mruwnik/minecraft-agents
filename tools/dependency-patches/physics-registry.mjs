// Why JavaScript: patches prismarine-physics source in node_modules (Mineflayer boundary).
// PlayerState's constructor runs require('minecraft-data')(bot.version) on every physics tick, which builds a new Version
// each time (garbage on an idle body); the bot already holds that registry as bot.registry.
const LOOKUP = "    const mcData = require('minecraft-data')(bot.version)\n"
const SHARED = "    const mcData = bot.registry ?? require('minecraft-data')(bot.version) // patched by bot/patch-deps.mjs: no Version per tick\n"
export const patchPhysicsRegistry = source => source.includes(SHARED)
  ? { status: 'already', source }
  : source.includes(LOOKUP) ? { status: 'patched', source: source.replace(LOOKUP, SHARED) } : { status: 'anchor missing', source }
