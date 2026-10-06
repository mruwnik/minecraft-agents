// Why JavaScript: thin helper for the tools/world-test.mjs launcher (which res-slot kinds a run holds).
// A run holds the body slot; with --allow-time it takes the time slot first (only one world-time-changing run at a time).
export const slotKinds = (args) => (args.includes('--allow-time') ? ['time', 'body'] : ['body'])

export const slotArgv = (args, resSlot, node, script) =>
  [...slotKinds(args).flatMap((k) => [resSlot, k, '--']), node, script, ...args]
