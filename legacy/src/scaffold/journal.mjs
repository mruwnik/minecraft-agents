import fs from 'node:fs'
import path from 'node:path'
// Body-local provenance. Intent is persisted BEFORE placement; a cancelled click
// that reaches the server is still discoverable on the next invocation.
export function scaffoldJournal (file) {
  return (id, value) => {
    if (!/^[a-z0-9_-]{1,100}$/.test(id)) throw new Error('invalid scaffold journal ID')
    const all = fs.existsSync(file) ? JSON.parse(fs.readFileSync(file, 'utf8')) : {}
    if (value === undefined) return all[id] ?? null
    if (value === null) delete all[id]
    else all[id] = value
    fs.mkdirSync(path.dirname(file), { recursive: true })
    const tmp = `${file}.tmp`
    fs.writeFileSync(tmp, JSON.stringify(all))
    fs.renameSync(tmp, file)
    return value
  }
}
