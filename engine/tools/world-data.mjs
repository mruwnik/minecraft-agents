// Why JavaScript: Node-facing exports of the cljs storage and coordination code in the AOT bundle (no compiler or JVM start, 500 ms budget).
import { loadTools } from './agent-tools-loader.mjs'
const tools = loadTools(['storage'])

export const {
  MAX_FILE_BYTES, MAX_OBJECTS, MAX_EVENTS, ID, name, fail, revision,
  context, documentPath, readDocument, listDocuments, boxOf, synchronize,
  mutateDocument, query, readChanges, withFileLock, rawBound
} = tools.storage
