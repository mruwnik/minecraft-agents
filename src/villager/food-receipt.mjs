// Small owned map records: each note stays within the map's 80-character limit.
export function foodReceiptStore (api, plan, food) {
  const name = `${api.me().toLowerCase()}-villager-food-${plan.x}-${plan.y}-${plan.z}`
  const records = () => api.places().filter(p => p.name === name || p.name.startsWith(`${name}-`))
  const mark = (suffix, note) => api.act('mark', { name: name + suffix, kind: 'work', x: plan.x, y: plan.y, z: plan.z, note })
  const clear = async () => { for (const p of records()) await api.act('unmark', { name: p.name }) }
  const load = () => {
    const rows = records()
    if (!rows.length) return null
    if (rows.some(p => String(p.by ?? '').toLowerCase() !== api.me().toLowerCase())) throw new Error('food receipt belongs to another player')
    if (rows.find(p => p.name === name)?.note !== food) throw new Error('food receipt does not match the selected food; inspect before feeding')
    if (rows.some(p => p.name === `${name}-pending`)) throw new Error('a prior food offering has unresolved pickup evidence; inspect the owned receipt before any further feeding')
    const parents = rows.filter(p => p.name.startsWith(`${name}-parent-`))
    const before = rows.filter(p => p.name.startsWith(`${name}-before-`)).map(p => p.name.slice(`${name}-before-`.length))
    const pair = parents.map(p => p.name.slice(`${name}-parent-`.length))
    const credits = Object.fromEntries(parents.map((p, n) => [pair[n], Number(p.note)]))
    const uuid = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i
    if (pair.length !== 2 || !before.length || [...pair, ...before].some(p => !uuid.test(p)) || parents.some(p => !/^\d+$/.test(p.note)) || Object.values(credits).some(c => c < 0 || c > (food === 'bread' ? 3 : 12) * Math.max(1, plan.target - 2)) || pair.some(p => !before.includes(p))) throw new Error('food receipt does not match a bounded pair feeding round')
    const totalRow = rows.find(p => p.name === `${name}-total`)
    const total = totalRow ? Number(totalRow.note) : Object.values(credits).reduce((sum, n) => sum + n, 0)
    if (!Number.isInteger(total) || total < 0 || totalRow && !/^\d+$/.test(totalRow.note)) throw new Error('aggregate food receipt is invalid')
    const held = Object.fromEntries(rows.filter(p => p.name.startsWith(`${name}-held-`)).map(p => [p.name.slice(`${name}-held-`.length), Number(p.note)]))
    if (Object.entries(held).some(([id, n]) => !uuid.test(id) || !Number.isInteger(n) || n < 0) || Object.values(held).reduce((sum, n) => sum + n, 0) > total) throw new Error('food held by young residents exceeds the confirmed household receipt')
    return { food, pair, before, credits, total, held }
  }
  const save = async receipt => {
    await mark('', receipt.food)
    for (const uuid of receipt.before) await mark(`-before-${uuid}`, 'before')
    for (const uuid of receipt.pair) await mark(`-parent-${uuid}`, String(receipt.credits[uuid] ?? 0))
    if (receipt.total !== undefined) await mark('-total', String(receipt.total))
    for (const [uuid, count] of Object.entries(receipt.held ?? {})) await mark(`-held-${uuid}`, String(count))
    for (const p of records().filter(p => p.name.startsWith(`${name}-held-`))) if (!(p.name.slice(`${name}-held-`.length) in (receipt.held ?? {}))) await api.act('unmark', { name: p.name })
    if (receipt.pending) await mark('-pending', `${receipt.pending.uuid}:${receipt.pending.count}`)
    else if (records().some(p => p.name === `${name}-pending`)) await api.act('unmark', { name: `${name}-pending` })
  }
  return { name, load, save, clear }
}
