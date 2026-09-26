// The blueprints this body can build (blueprints/*.md, see docs/superpowers/specs/2026-09-26-blueprint-format-design.md),
// one line each. It reads files and never takes the body over.
import { listText } from '../../src/blueprint-build.mjs'

export default {
  doc: 'blueprint.list [tag=] [q=]: the blueprints in blueprints/, one line each: name, size, tags, total items, title',
  stops: 'nothing: it reads files and moves nothing',
  instant: true,
  args: { tag: 'string', q: 'string' },

  run: async (api, a) => ({ text: listText(a) })
}
