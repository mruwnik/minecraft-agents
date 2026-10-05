// Why JavaScript: Mineflayer boundary; turns the bot's chunk and block events into the region map's changes.
// The world a region map (engine.path.regions/attach!) is kept over: one live snapshot kept for the body's whole session
// (not one per plan), so every change must reach it: a block update is written into the copy (setState), a column loaded
// or unloaded is forgotten (read again when next asked). onChange(fn) calls fn after the snapshot has the change, with
// {type: 'block', x, y, z, old} (old: the state id before) or {type: 'load' | 'unload', cx, cz}; it returns the unsubscribe.
import { liveSnapshot } from './live-snapshot.mjs'
import { defaultStateTable } from './blocks.mjs'
import * as space from './space.mjs'

export function regionSource (bot) {
  const snapshot = liveSnapshot(bot.world, { minY: bot.game.minY, height: bot.game.height })
  const listeners = new Set()
  const emit = e => { for (const fn of listeners) fn(e) }
  const onBlock = (oldBlock, newBlock) => {
    const p = (newBlock ?? oldBlock)?.position
    if (!p) return
    const id = newBlock?.stateId ?? 0
    const old = oldBlock?.stateId
    if (old === id) return
    snapshot.setState(p.x, p.y, p.z, id)
    emit({ type: 'block', x: p.x, y: p.y, z: p.z, old })
  }
  const onColumn = type => corner => {
    const cx = corner.x >> 4
    const cz = corner.z >> 4
    snapshot.forgetColumn(cx, cz)
    emit({ type, cx, cz })
  }
  const onLoad = onColumn('load')
  const onUnload = onColumn('unload')
  let hooked = false
  const hook = () => {
    if (hooked) return
    hooked = true
    bot.on('blockUpdate', onBlock)
    bot.on('chunkColumnLoad', onLoad)
    bot.on('chunkColumnUnload', onUnload)
  }
  const unhook = () => {
    if (!hooked) return
    hooked = false
    bot.removeListener('blockUpdate', onBlock)
    bot.removeListener('chunkColumnLoad', onLoad)
    bot.removeListener('chunkColumnUnload', onUnload)
  }
  return {
    snapshot,
    table: defaultStateTable(),
    space,
    // [[cx, cz], ...] of the columns the bot has loaded now
    columns: () => (bot.world.getColumns?.() ?? []).map(c => [Number(c.chunkX), Number(c.chunkZ)]),
    onChange: fn => {
      listeners.add(fn)
      hook()
      return () => {
        listeners.delete(fn)
        if (listeners.size === 0) unhook()
      }
    }
  }
}
