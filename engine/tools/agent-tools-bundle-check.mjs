// Why JavaScript: it checks the AOT cljs bundle from outside before a launcher calls into it; a stale bundle cannot
// report exports it does not yet have, so this check cannot live inside the bundle.

export const missingExports = (bundle, required) =>
  required.filter(name => bundle?.[name] === undefined)

export const buildRequiredMessage = missing =>
  `The compiled agent-tools bundle is stale; missing exports: ${missing.join(', ')}. Rebuild it once with: cd dashboard && npm run build-agent-tools`

export const buildRequiredEdn = missing =>
  `{:ok false :reason :build-required :missing-exports [${missing.map(name => JSON.stringify(name)).join(' ')}] :message ${JSON.stringify(buildRequiredMessage(missing))}}`
