// A gate left open makes a pen read as open country: pen.check answers LEAKS and its census comes back empty, so
// anything that counts what is inside has to shut the gate it walked through before it looks.
import { gateLeak } from './lib.mjs'

export async function penHolds (api, at) {
  const check = () => api.act('pen.check', at)
  const first = await check()
  const gate = first.pen === 'LEAKS' && first.via
    ? gateLeak(first.via, (x, y, z) => { const b = api.block(x, y, z); return b && { name: b.name, open: b.properties?.open } })
    : null
  if (!gate) return { held: first }
  await api.act('toggle', { x: gate[0], y: gate[1], z: gate[2], open: false }).catch(() => {})
  return { held: await check(), shut: gate.join(',') }
}
