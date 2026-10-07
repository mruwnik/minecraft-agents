// Why JavaScript: listens to the server's set_ticking_state packet on the Mineflayer client, a thin adapter; the rate's meaning lives in engine.settings.
// The game rate: ticks per second the server runs at (/tick rate), and whether the tick is frozen. One clock lives for
// the whole body and is attached to every client it connects with (a reconnect makes a new one), before spawn: the
// join packet carries the rate and arrives before the spawn event. Independent of the physics shim, which keeps its
// own copy of the rate for the physics interval. Without a packet the rate is 20 and the source is 'assumed'.
export const VANILLA_RATE = 20

export function createGameClock () {
  const listeners = []
  const clock = {
    rate: VANILLA_RATE,
    frozen: false,
    source: 'assumed',
    // fn(rate, frozen, source) after every packet
    onChange: fn => { listeners.push(fn) },
    attach (client) {
      client.on('set_ticking_state', packet => {
        if (packet.tick_rate > 0) clock.rate = packet.tick_rate
        clock.frozen = Boolean(packet.is_frozen)
        clock.source = 'packet'
        for (const fn of listeners) fn(clock.rate, clock.frozen, clock.source)
      })
    }
  }
  return clock
}
