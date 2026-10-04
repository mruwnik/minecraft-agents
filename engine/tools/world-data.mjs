// Compatibility exports for Node callers; storage and coordination are CLJS.
import tools from './agent-tools-loader.mjs'

export const {
  MAX_FILE_BYTES, MAX_OBJECTS, MAX_EVENTS, ID, name, fail, revision,
  context, documentPath, readDocument, listDocuments, boxOf, synchronize,
  mutateDocument, query, readChanges, withFileLock, rawBound
} = tools.storage
