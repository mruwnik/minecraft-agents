// Why JavaScript: patches CommonJS dependencies through require.cache at their load boundary; must be imported before mineflayer or protodef is.
// Drops CPU a body start spends on dependency code it never uses. Both patches only defer work; behaviour is unchanged.
import { createRequire } from 'node:module'

const require = createRequire(import.meta.url)

// protodef's index.js builds a schema-validating ProtoDef at import (ajv, ~100 ms CPU) only to export its `types`.
function skipDefaultValidation () {
  const file = require.resolve('protodef/src/protodef.js')
  const ProtoDef = require(file)
  class Unvalidated extends ProtoDef { constructor (validation = false) { super(validation) } }
  require.cache[file].exports = Unvalidated
}

// minecraft-protocol loads prismarine-auth and realms (~50 ms) for Microsoft logins; bodies log in offline.
function deferMicrosoftAuth () {
  const file = require.resolve('minecraft-protocol/src/client/microsoftAuth.js')
  const stub = { id: file, filename: file, loaded: true, exports: null }
  let loaded
  const real = () => {
    if (loaded) return loaded
    delete require.cache[file]
    return (loaded = require(file))
  }
  const lazy = name => (...args) => real()[name](...args)
  stub.exports = { authenticate: lazy('authenticate'), realmAuthenticate: lazy('realmAuthenticate') }
  require.cache[file] = stub
}

skipDefaultValidation()
deferMicrosoftAuth()
