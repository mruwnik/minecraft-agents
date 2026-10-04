// The one-time Microsoft sign-in for a body on an online-mode server:
//   node tools/login.mjs AmethystFan7865 --world claude
// Prints a URL and a code; sign in there with the account that owns that player. The world names the body folder whose
// config.json is read (state/worlds/<world>/agents/<Name>); the tokens land per account in state/accounts/<Name>/, shared
// by every world, and refresh themselves from then on; this is run again only when the body reports
// login_needed (a refresh token expires after months of disuse) or to switch the account.
import fs from 'node:fs'
import path from 'node:path'
import prismarineAuth from 'prismarine-auth'
import { readConfig } from '../src/config.mjs'
import { authDir, profileFile, profileMismatch, AUTHFLOW_OPTIONS } from '../src/auth.mjs'
import { bodyDir, missingWorldError } from '../engine/js/bodies.mjs'

// cjs-module-lexer only detects `Authflow` as a named export of this CJS package, not `Titles` (see src/auth.mjs)
const { Authflow } = prismarineAuth

const [name, flag, world] = process.argv.slice(2)
if (!name) { console.error('usage: node tools/login.mjs <Name> --world <world>'); process.exit(2) }
if (flag !== '--world' || !world) { console.error(`${missingWorldError('--world')}\nusage: node tools/login.mjs <Name> --world <world>`); process.exit(2) }
const home = bodyDir(path.join(import.meta.dirname, '..', 'state'), world, name)
const cfg = readConfig(home)
if (cfg.auth !== 'microsoft') { console.error(`${name}'s config.json says auth "${cfg.auth}": nothing to sign in to`); process.exit(2) }

const dir = authDir(home)
fs.mkdirSync(dir, { recursive: true, mode: 0o700 })
const showCode = data => console.log(`\nOpen ${data.verification_uri} and enter the code ${data.user_code}\n(signing in as the account that owns ${cfg.username})\n`)
const flow = new Authflow(cfg.username, dir, AUTHFLOW_OPTIONS, showCode)
const { profile } = await flow.getMinecraftJavaToken({ fetchProfile: true })

const mismatch = profileMismatch(profile, cfg.username)
if (mismatch) {
  fs.rmSync(dir, { recursive: true, force: true })
  console.error(mismatch)
  process.exit(1)
}
fs.writeFileSync(profileFile(home), JSON.stringify({ name: profile.name, id: profile.id, at: new Date().toISOString() }, null, 1) + '\n')
console.log(`signed in: ${profile.name} (${profile.id}); tokens cached in ${dir}`)
