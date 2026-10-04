// Why JavaScript: browser/binary; fetches the per-version block tables once for every scene.
// The per-version block tables (materials, textures, elements, tints) of one renderer, fetched once and shared by every scene
// that draws with it. The decoder pool is told the same table. One renderer holds one version (gl.mjs claimTable).
const ISSUE_LEVELS = ['missing', 'wrong', 'approximate']
const cache = new WeakMap() // renderer -> tables

const decodeBase64U16 = text => {
  const binary = atob(text)
  const bytes = Uint8Array.from(binary, c => c.charCodeAt(0))
  return new Uint16Array(bytes.buffer)
}

const fetchTable = async ({ renderer, decoder, baseUrl, debugLevel }, version) => {
  const [res, texRes, elemRes] = await Promise.all([fetch(`${baseUrl}/blocks/${version}.json${debugLevel > 0 ? `?debug=${debugLevel}` : ''}`), fetch(`${baseUrl}/textures/${version}.bin`), fetch(`${baseUrl}/elements/${version}.bin`)])
  if (!res.ok) throw new Error(`blocks table for ${version}: HTTP ${res.status}`)
  if (!texRes.ok) throw new Error(`textures for ${version}: HTTP ${texRes.status}`)
  const [json, bytes] = await Promise.all([res.json(), texRes.arrayBuffer()])
  if (json.tints) renderer.setTints(json.tints)
  if (json.elements) {
    if (!elemRes.ok) throw new Error(`elements for ${version}: HTTP ${elemRes.status}`)
    renderer.setElements({ data: new Float32Array(await elemRes.arrayBuffer()), width: json.elements.width, rows: json.elements.rows, listTexels: json.elements.listTexels })
  }
  const table = { materialOf: decodeBase64U16(json.materialOf), materials: json.materials, format: json.format }
  decoder.setTable({ format: json.format, materialOf: table.materialOf })
  renderer.setMaterials(json.materials.map(m => ({ ...m, issue: ISSUE_LEVELS.indexOf(m.issue) >= 0 && ISSUE_LEVELS.indexOf(m.issue) <= debugLevel })))
  renderer.setTextures({ bytes: new Uint8Array(bytes), layers: json.textures.names.length, size: json.textures.size, levels: json.textures.levels })
  return table
}

const createTables = options => {
  let loaded = null // {version, promise}
  // the table of a version, or null when the renderer already holds another version
  const ensure = version => {
    if (loaded?.version === version) return loaded.promise
    if (!options.renderer.claimTable(version)) return Promise.resolve(null)
    const promise = fetchTable(options, version)
    loaded = { version, promise }
    promise.catch(() => { if (loaded?.promise === promise) loaded = null }) // a failed load is retried by the next pose
    return promise
  }
  return { ensure }
}

export const tablesFor = (renderer, options) => {
  if (!cache.has(renderer)) cache.set(renderer, createTables({ renderer, baseUrl: '', debugLevel: 0, ...options }))
  return cache.get(renderer)
}
