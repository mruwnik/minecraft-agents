// Why JavaScript: Mineflayer boundary; decodes the villager_data entity metadata.
const DATA_FALLBACK = 18
const PROFESSIONS = 'none armorer butcher cartographer cleric farmer fisherman fletcher leatherworker librarian mason nitwit shepherd toolsmith weaponsmith'.split(' ')

// villager_data holds the profession as a registry index (number) or as a namespaced name
export const professionOf = raw => {
  if (raw == null) return 'unemployed'
  const name = typeof raw === 'number' ? PROFESSIONS[raw] : typeof raw === 'string' ? raw.replace(/^minecraft:/, '') : undefined
  if (name === undefined) return 'unknown'
  return name === 'none' ? 'unemployed' : name
}

// The server leaves villager_data out for a villager whose data is the default (plains, no profession, level 1)
const DEFAULT_DATA = { villagerProfession: undefined, level: 1 }
export const villagerData = (bot, e) => {
  const at = bot.registry?.entitiesByName?.[e.name]?.metadataKeys?.indexOf('villager_data')
  return e.metadata?.[at >= 0 ? at : DATA_FALLBACK] ?? DEFAULT_DATA
}
