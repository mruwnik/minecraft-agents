// Why JavaScript: it checks the AOT cljs bundle from outside before a launcher calls into it; a stale bundle cannot
// report exports it does not yet have, so this check cannot live inside the bundle.

// Every bundle export a launcher in engine/tools calls (engine/test/tools/agent-tools-loader.test.mjs keeps it complete).
export const launcherExports = Object.freeze([
  'blueprintForm',
  'changesExecute',
  'changesMain',
  'changesOptions',
  'changesUsage',
  'driveMain',
  'driveUsage',
  'ednRead',
  'ednWrite',
  'entitiesMain',
  'entitiesOptions',
  'entitiesUsage',
  'jobsMain',
  'jobsRequestFor',
  'jobsUsage',
  'mapExecute',
  'mapFilters',
  'mapMain',
  'mapOptions',
  'mapSummary',
  'mapTtl',
  'mapUsage',
  'mapValidateZone',
  'observeMain',
  'observeRequestFor',
  'observeUsage',
  'planExecute',
  'planOneForm',
  'planRequestFor',
  'planUsage',
  'sayMain',
  'sayRequestFor',
  'sayUsage',
  'snapshotMain',
  'snapshotUsage',
  'storage',
  'timeClock',
  'timeExecute',
  'timeMain',
  'timeOptions',
  'timeUsage',
  'triggersMain',
  'triggersRequestFor',
  'triggersUsage',
  'workspaceGenerate',
  'workspaceRoute',
  'workspaceUsage',
  'worldMain',
  'worldRequestFor',
  'worldUsage',
])

export const missingExports = (bundle, required = launcherExports) =>
  required.filter(name => bundle?.[name] === undefined)

export const buildRequiredMessage = missing =>
  `The compiled agent-tools bundle is stale; missing exports: ${missing.join(', ')}. Rebuild it once with: cd dashboard && npm run build-agent-tools`

export const buildRequiredEdn = missing =>
  `{:ok false :reason :build-required :missing-exports [${missing.map(name => JSON.stringify(name)).join(' ')}] :message ${JSON.stringify(buildRequiredMessage(missing))}}`
