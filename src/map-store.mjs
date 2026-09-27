import fs from 'node:fs'
// Shared map writers and the schema migration take the same exclusive lock.
// A busy writer fails explicitly; it never replaces a snapshot while migration runs.
export function withMapLock (file, run) {
  const lock = `${file}.lock`
  let fd
  try { fd = fs.openSync(lock, 'wx', 0o600) } catch (error) {
    if (error.code === 'EEXIST') throw new Error('shared map is busy; retry after the current writer or migration finishes')
    throw error
  }
  try {
    fs.writeFileSync(fd, String(process.pid))
    return run()
  } finally {
    fs.closeSync(fd)
    fs.unlinkSync(lock)
  }
}
export function atomicMapWrite (file, value) {
  const tmp = `${file}.${process.pid}.tmp`
  try {
    fs.writeFileSync(tmp, JSON.stringify(value, null, 1))
    fs.renameSync(tmp, file)
  } finally { if (fs.existsSync(tmp)) fs.unlinkSync(tmp) }
}
