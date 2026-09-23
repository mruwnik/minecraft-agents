// Honey is only taken through verified smoke, and never over an open fire. Raw `use` can anger a colony; this wrapper refuses that state, checks the
// honey level fell after every click, and collects the comb that shears drop.
import { apiarySnapshot } from './shared/common.mjs'

export default {
  doc: 'apiary.harvest place=|x= y= z= [mode=comb] [range=16]: safely harvest ripe, smoked hives with shears or bottles',
  stops: 'every safe ripe hive is harvested, equipment is missing, or a hive fails to empty',
  args: { place: 'string', x: 'number', y: 'number', z: 'number', mode: 'string', range: 'number' },

  async run (api, a) {
    const mode = a.mode ?? 'comb'
    if (!['comb', 'bottle'].includes(mode)) throw new Error('mode= is comb or bottle')
    const item = mode === 'comb' ? 'shears' : 'glass_bottle'
    if (!(api.inv()[item] > 0)) throw new Error(`${mode} harvest needs ${item}`)
    const seen = await apiarySnapshot(api, a, 'apiary.harvest')
    const ripe = seen.hives.filter(h => h.ripe)
    const safe = ripe.filter(h => h.smoked && !h.open && h.entranceClear)
    const unsafe = ripe.filter(h => !h.smoked)
    const open = ripe.filter(h => h.open)
    const blocked = ripe.filter(h => !h.entranceClear)
    if (!safe.length && (unsafe.length || open.length || blocked.length)) {
      const first = unsafe[0] ?? open[0] ?? blocked[0]
      const why = !first.smoked ? 'has no lit campfire with a clear smoke path'
        : first.open ? `stands over an open fire at ${first.campfire.x},${first.campfire.y},${first.campfire.z}: put a carpet on it (apiary.guard), bees burn in open fire`
        : 'has a blocked entrance'
      throw new Error(`no ripe hive is safe to harvest: ${first.x},${first.y},${first.z} ${why}`)
    }
    let harvested = 0
    for (const hive of safe) {
      await api.act('use', { x: hive.x, y: hive.y, z: hive.z, item })
      await api.pause(0.5)
      const now = Number(api.block(hive.x, hive.y, hive.z)?.properties?.honey_level ?? 0)
      if (now >= 5) throw new Error(`the ${hive.name} at ${hive.x},${hive.y},${hive.z} stayed at honey level ${now} after using ${item}`)
      harvested++
      api.report({ harvested })
      await api.checkpoint()
    }
    if (mode === 'comb' && harvested) await api.act('collect', {})
    return { harvested, mode, ripe: ripe.length, unsafe: unsafe.length, openFires: open.length, blocked: blocked.length }
  }
}
