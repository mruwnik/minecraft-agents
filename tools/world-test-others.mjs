#!/usr/bin/env node
// Why JavaScript: Mineflayer boundary; joins plain offline players so the world-test runner can show the body a tab list with awake others.
//   node tools/world-test-others.mjs <world.json> NAME...   prints "ready" once all joined; leaves on stdin close or SIGTERM
import fs from 'node:fs'
import { createRequire } from 'node:module'

const [worldFile, ...names] = process.argv.slice(2)
const mineflayer = createRequire(import.meta.url)('mineflayer')
const world = JSON.parse(fs.readFileSync(worldFile, 'utf8'))
const bots = names.map(username => {
  const bot = mineflayer.createBot({ host: world.host, port: world.port, username, auth: 'offline', viewDistance: 'tiny' })
  bot.on('error', e => console.error(`${username}: ${e.message}`))
  return bot
})
await Promise.all(bots.map(bot => new Promise((resolve, reject) => {
  bot.once('spawn', resolve)
  bot.once('kicked', r => reject(new Error(`kicked ${JSON.stringify(r)}`)))
})))
console.log('ready')
const leave = () => { for (const bot of bots) bot.quit(); setTimeout(() => process.exit(0), 500) }
process.on('SIGTERM', leave)
process.stdin.on('end', leave).resume()
