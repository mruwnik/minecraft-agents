// Since 1.21.3 the server only clears its "had a position this tick" flag on a serverbound tick_end, and a 26.3 server
// kicks the second position packet that arrives without one (invalid_player_movement, about a second after spawn).
// The real client sends it every tick; mineflayer 4.39 does not (PrismarineJS/mineflayer#4128 is still open).
export function installTickEnd (bot) {
  if (!bot.registry.protocol.play.toServer.types.packet_tick_end) return
  bot.on('physicsTick', () => bot._client.write('tick_end', {}))
}
