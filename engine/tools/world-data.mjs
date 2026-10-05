// Why JavaScript: compatibility exports for Node callers, storage and coordination are CLJS; Thin Node launcher over the AOT cljs bundle dashboard/out/agent-tools.cjs; runs without starting a compiler or JVM (500 ms startup budget).
import { loadTools } from './agent-tools-loader.mjs'
const tools = loadTools(['storage'])

export const {
  MAX_FILE_BYTES, MAX_OBJECTS, MAX_EVENTS, ID, name, fail, revision,
  context, documentPath, readDocument, listDocuments, boxOf, synchronize,
  mutateDocument, query, readChanges, withFileLock, rawBound
} = tools.storage
