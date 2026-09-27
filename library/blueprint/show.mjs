import { showBlueprintV2 } from '../../src/blueprint/v2.mjs'
// One blueprint in full: metadata, the bill per layer and in total, the stages a build would take with the inventory as
// it is, what lint says, and the layers rendered after facing= turns them. It reads a file and never takes the body over.

export default {
  doc: 'blueprint.show (name=<catalog> | plan=<v2 object> | file=<caller JSON path>) [facing=] [layer=]: validated intent, illustrative material palette and layers; check resolves actual carried/declared stock',
  stops: 'nothing: it reads a file and moves nothing',
  instant: true,
  args: { plan: 'any', origin: 'string', file: 'string', name: 'string', facing: 'string', layer: 'number' },

  run: showBlueprintV2
}
