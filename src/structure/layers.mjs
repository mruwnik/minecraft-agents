// Browser-safe geometry shared by construction blueprints and maintenance plans.
// Layer y is an ACTUAL block coordinate relative to the anchor. _ is unconstrained;
// . is explicit air. Intent and placement recipes are interpreted by the caller.
export function readStructureLayers (layers, { width, depth, minY = -4, maxY = 51, maxCells = 16384, label = 'structure' } = {}) {
  if (!Array.isArray(layers)) throw new Error(`${label}.layers: must be an array`)
  if (layers.length > 56) throw new Error(`${label}.layers: at most 56 layers`)
  const cells = [], occupied = new Set()
  for (const [index, layer] of layers.entries()) {
    const at = `${label}.layers[${index}]`
    if (!layer || typeof layer !== 'object' || Array.isArray(layer) || Object.keys(layer).some(k => !['y', 'rows'].includes(k))) throw new Error(`${at}: unsupported layer field`)
    if (!Number.isInteger(layer.y) || layer.y < minY || layer.y > maxY || !Array.isArray(layer.rows) || !layer.rows.length || layer.rows.some(row => typeof row !== 'string')) throw new Error(`${at}: layer needs integer y in ${minY}..${maxY} and dimensions-sized rows`)
    width ??= [...layer.rows[0]].length
    depth ??= layer.rows.length
    if (!width || width > 64 || depth > 64 || layer.rows.length !== depth || layer.rows.some(row => [...row].length !== width)) throw new Error(`${at}: layer needs dimensions-sized rows (at most 64x64)`)
    for (const [z, row] of layer.rows.entries()) for (const [x, token] of [...row].entries()) {
      if (token === '_') continue
      const key = `${x},${layer.y},${z}`
      if (occupied.has(key)) throw new Error(`${at}: overlapping cell ${key}`)
      occupied.add(key)
      if (cells.length >= maxCells) throw new Error(`${label}: more than ${maxCells} constrained cells`)
      cells.push({ x, y: layer.y, z, token, layer: index })
    }
  }
  return { width: width ?? 0, depth: depth ?? 0, cells }
}
