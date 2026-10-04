// prismarine-physics 1.11.1 index.js: vanilla (1.9 and on, 26.1 too) climbs an open trapdoor that sits directly over a
// ladder of its own facing (LivingEntity.trapdoorUsableAsLadder), and isOnLadder has that rule, but only behind the
// climbableTrapdoor feature, whose version list stops at 1.20. On 26.1 the client never climbed such a hatch: the body
// stopped with its feet at the trapdoor's floor while the server would have let it up (card 5c05d0d7).
export const TRAPDOOR_FEATURE = "  const climbableTrapdoorFeature = supportFeature('climbableTrapdoor')\n"
export const TRAPDOOR_ALWAYS = '  const climbableTrapdoorFeature = true // patched by bot/patch-deps.mjs: open trapdoor over a ladder of its facing, as vanilla since 1.9\n'
export const patchClimbableTrapdoor = source => source.includes(TRAPDOOR_ALWAYS)
  ? { status: 'already', source }
  : source.includes(TRAPDOOR_FEATURE) ? { status: 'patched', source: source.replace(TRAPDOOR_FEATURE, TRAPDOOR_ALWAYS) } : { status: 'anchor missing', source }
