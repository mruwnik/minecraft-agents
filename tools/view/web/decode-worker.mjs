// Worker thread of the column decoder (decoder.mjs). {type: 'table', format, materialOf} stores the block table;
// {type: 'job', id, key, bytes} decodes one column file and answers {id, result, ms} with the result buffers transferred,
// or {id, error}.
import { decodeColumn, transferables } from './column-work.mjs'

export const createHandler = post => {
  let table = null
  return async ({ data }) => {
    if (data.type === 'table') {
      table = { format: data.format, materialOf: data.materialOf }
      return
    }
    if (data.type !== 'job') return
    const started = performance.now()
    try {
      const result = await decodeColumn(data.bytes, table)
      post({ id: data.id, result, ms: performance.now() - started }, transferables(result))
    } catch (error) {
      post({ id: data.id, error: String(error?.message ?? error) })
    }
  }
}

if (typeof WorkerGlobalScope !== 'undefined' && self instanceof WorkerGlobalScope) self.onmessage = createHandler((message, transfer) => self.postMessage(message, transfer))
