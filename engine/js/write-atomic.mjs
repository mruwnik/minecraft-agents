// Why JavaScript: file writes for the JS-side view and seen-block files.
import fs from 'node:fs'
import path from 'node:path'

// write to <file>.tmp.<pid> then rename, so a reader never sees half a file
let counter = 0
export async function writeAtomic (file, data) {
  const tmp = `${file}.tmp.${process.pid}.${counter++}`
  const write = () => fs.promises.writeFile(tmp, data)
  await write().catch(async err => {
    if (err.code !== 'ENOENT') throw err
    await fs.promises.mkdir(path.dirname(file), { recursive: true })
    await write()
  })
  await fs.promises.rename(tmp, file).catch(async err => {
    await fs.promises.rm(tmp, { force: true })
    throw err
  })
}
