// The one-time Microsoft sign-in for a body on an online-mode server:
//   node tools/login.mjs AmethystFan7865
// Prints a URL and a code; sign in there with the account that owns that player. Tokens land in
// state/agents/<Name>/auth/ and refresh themselves from then on; this is run again only when the body reports
// login_needed (a refresh token expires after months of disuse) or to switch the account.
import fs from 'node:fs'
import path from 'node:path'
import prismarineAuth from 'prismarine-auth'
import { readConfig } from '../src/config.mjs'
import { authDir, profileFile, profileMismatch, AUTHFLOW_OPTIONS } from '../src/auth.mjs'

// cjs-module-lexer only detects `Authflow` as a named export of this CJS package, not `Titles` (see src/auth.mjs)
const { Authflow } = prismarineAuth

const name = process.argv[2]
if (!name) { console.error('usage: node tools/login.mjs <Name>'); process.exit(2) }
const home = path.join(import.meta.dirname, '..', 'state', 'agents', name)
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
