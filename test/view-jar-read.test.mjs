// Which client jar the view tools read: $MC_CLIENT_JAR, else the newest release in the launcher's versions dir or the agents' cache.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { findClientJar, openJar } from '../tools/view/jar-read.mjs'

const makeHome = ({ launcher = [], cache = [], others = [] }) => {
  const home = fs.mkdtempSync(path.join(os.tmpdir(), 'jar-read-'))
  for (const v of launcher) {
    fs.mkdirSync(path.join(home, '.minecraft/versions', v), { recursive: true })
    fs.writeFileSync(path.join(home, '.minecraft/versions', v, `${v}.jar`), '')
  }
  fs.mkdirSync(path.join(home, '.cache/minecraft-agents/client'), { recursive: true })
  for (const v of cache) fs.writeFileSync(path.join(home, '.cache/minecraft-agents/client', `${v}.jar`), '')
  for (const v of others) fs.writeFileSync(path.join(home, '.cache/minecraft-agents/client', v), '')
  return home
}

const cases = [
  ['the cache jar wins when it is newer (26.1.2 > 1.21.8, numerically)', { launcher: ['1.21.8', '1.9'], cache: ['26.1.2'] }, 'cache', '26.1.2'],
  ['the launcher jar wins when it is newer', { launcher: ['1.21.8'], cache: ['1.9.4'] }, 'launcher', '1.21.8'],
  ['10 is newer than 9, not older', { cache: ['1.9', '1.10'] }, 'cache', '1.10'],
  ['non-release names are ignored', { cache: ['1.21.8'], others: ['27.0-snapshot-1.jar', 'notes.txt'] }, 'cache', '1.21.8']
]
for (const [name, layout, where, version] of cases) {
  test(name, () => {
    const home = makeHome(layout)
    const expected = where === 'cache' ? path.join(home, '.cache/minecraft-agents/client', `${version}.jar`) : path.join(home, '.minecraft/versions', version, `${version}.jar`)
    assert.equal(findClientJar({ env: {}, home, platform: 'linux' }), expected)
  })
}

test('$MC_CLIENT_JAR wins when it exists, and means no jar when it does not', () => {
  const home = makeHome({ cache: ['26.1.2'] })
  const given = path.join(home, 'mine.jar')
  fs.writeFileSync(given, '')
  assert.equal(findClientJar({ env: { MC_CLIENT_JAR: given }, home, platform: 'linux' }), given)
  assert.equal(findClientJar({ env: { MC_CLIENT_JAR: path.join(home, 'none.jar') }, home, platform: 'linux' }), null)
})

test('no jar anywhere is null', () => {
  assert.equal(findClientJar({ env: {}, home: makeHome({}), platform: 'linux' }), null)
})

// a one-entry stored zip, written by hand: local header, data, central directory, end record
const zipOf = (name, data) => {
  const nameBuf = Buffer.from(name)
  const local = Buffer.alloc(30)
  local.writeUInt32LE(0x04034b50, 0)
  local.writeUInt16LE(nameBuf.length, 26)
  const central = Buffer.alloc(46)
  central.writeUInt32LE(0x02014b50, 0)
  central.writeUInt32LE(data.length, 20)
  central.writeUInt16LE(nameBuf.length, 28)
  const end = Buffer.alloc(22)
  const centralAt = local.length + nameBuf.length + data.length
  end.writeUInt32LE(0x06054b50, 0)
  end.writeUInt16LE(1, 10)
  end.writeUInt32LE(central.length + nameBuf.length, 12)
  end.writeUInt32LE(centralAt, 16)
  return Buffer.concat([local, nameBuf, data, central, nameBuf, end])
}

test('openJar reads an entry by name from a path, and null for a missing one', () => {
  const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'jar-open-')), 'x.jar')
  fs.writeFileSync(file, zipOf('assets/a.txt', Buffer.from('hello')))
  const jar = openJar(file)
  assert.deepEqual(jar.names, ['assets/a.txt'])
  assert.equal(jar.read('assets/a.txt').toString(), 'hello')
  assert.equal(jar.read('nope'), null)
})
