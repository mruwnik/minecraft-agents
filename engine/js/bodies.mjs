// Where a body's folder is: state/worlds/<world>/agents/<name>. A name is unique only within a world, so the world is
// always given; there is no search over worlds and no default. A real account's login cache is per account, shared by
// every world: state/accounts/<name>. The ClojureScript twin is engine/src/engine/bodies.cljs; keep the two alike.
import fs from 'node:fs'
import path from 'node:path'

export const NAME = /^[A-Za-z0-9_-]{1,64}$/

const checked = (what, value) => {
  if (typeof value !== 'string' || !NAME.test(value)) throw new Error(`a body folder needs a ${what} of letters, digits, _ and -, got ${JSON.stringify(value)}`)
  return value
}

export const missingWorldError = flag => `missing ${flag} <world>: the world the body plays in (a folder under state/worlds/)`

export const worldsDir = stateDir => path.join(stateDir, 'worlds')

export const bodyDir = (stateDir, world, name) =>
  path.join(worldsDir(stateDir), checked('world', world), 'agents', checked('name', name))

export const accountDir = (stateDir, name) => path.join(stateDir, 'accounts', checked('name', name))

// the world of a body folder is the name of the folder two levels up
export const worldOfBodyDir = dir => path.basename(path.dirname(path.dirname(path.resolve(dir))))

const dirNames = dir => {
  if (!fs.existsSync(dir)) return []
  return fs.readdirSync(dir, { withFileTypes: true }).filter(e => e.isDirectory() && NAME.test(e.name)).map(e => e.name).sort()
}

// every body folder, of one world or of all: [{ world, name, dir }] sorted by world then name
export const listBodies = (stateDir, world) =>
  (world === undefined ? dirNames(worldsDir(stateDir)) : [world])
    .flatMap(w => dirNames(path.join(worldsDir(stateDir), w, 'agents')).map(name => ({ world: w, name, dir: bodyDir(stateDir, w, name) })))
