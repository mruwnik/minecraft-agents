// The body's actions, by name. src/bot.mjs fills both tables: the runtimes' own (src/body/runtimes.mjs), the
// primitives of src/body/actions/, then a composite for each library/ file.
// "long" actions take over the body; starting a new one cancels the previous.
export const long = {}
// "quick" actions answer immediately and don't interrupt whatever the body is doing.
export const quick = {}
