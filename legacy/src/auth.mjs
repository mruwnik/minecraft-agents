// A body on an online-mode server logs in with a real Microsoft account. The human signs in once (tools/login.mjs),
// the tokens are cached per account in state/accounts/<Name> (one identity, shared by every world the account plays in),
// and the body reads that cache. The decisions both sides share are here.
import path from 'node:path'
import prismarineAuth from 'prismarine-auth'
import { accountDir, worldOfBodyDir } from '../../engine/js/bodies.mjs'

// cjs-module-lexer only detects `Authflow` as a named export of this CJS package, not `Titles`, so a default
// import plus destructuring is required here (an `import { Titles } from 'prismarine-auth'` throws at parse time)
const { Titles } = prismarineAuth

// home is a body folder, state/worlds/<world>/agents/<Name>: the state dir is four levels up
export const authDir = home => accountDir(path.resolve(home, '..', '..', '..', '..'), path.basename(home))
export const profileFile = home => path.join(authDir(home), 'profile.json')

// minecraft-protocol's own Authflow arguments (src/client/microsoftAuth.js): the cache is keyed by them
export const AUTHFLOW_OPTIONS = { authTitle: Titles.MinecraftNintendoSwitch, deviceType: 'Nintendo', flow: 'live' }

// the folder is named after the player and Mojang decides the player name: a sign-in under the wrong folder would
// run a body whose config, API port and clock entries belong to someone else
export const profileMismatch = (profile, username) => {
  if (!profile?.name) return `Microsoft returned no profile for ${username}: does that account own Minecraft Java?`
  if (profile.name.toLowerCase() === username.toLowerCase()) return null
  return `signed in as ${profile.name}, but this folder is ${username}: sign in with the account that owns ${username}`
}

export const loginAdvice = home => `no Microsoft sign-in for ${path.basename(home)} yet: run \`node legacy/tools/login.mjs ${path.basename(home)} --world ${worldOfBodyDir(home)}\` once (from the repo root) and ./start again`
