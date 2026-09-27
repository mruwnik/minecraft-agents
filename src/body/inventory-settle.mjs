// Passenger physics is disabled by Mineflayer, so physicsTick waits time out
// aboard a vehicle. Inventory packets still arrive on ordinary wall time.
export async function settleInventory (bot, counts, pause = ms => new Promise(resolve => setTimeout(resolve, ms))) {
  let last = null
  for (let i = 0; i < 4; i++) {
    if (bot.vehicle) await pause(250)
    else await bot.waitForTicks(5).catch(() => {})
    const now = JSON.stringify(counts())
    if (now === last) return
    last = now
  }
}
