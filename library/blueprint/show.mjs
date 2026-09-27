import { isV2BlueprintArguments, showBlueprintV2 } from '../../src/blueprint/v2.mjs'
// One blueprint in full: metadata, the bill per layer and in total, the stages a build would take with the inventory as
// it is, what lint says, and the layers rendered after facing= turns them. It reads a file and never takes the body over.
import { showText, paramArgs } from '../../src/blueprint/build.mjs'

export default {
  doc: 'blueprint.show (name=<catalog> | plan=<v2 object> | file=<caller JSON path>) [facing=] [layer=]: validated intent, illustrative material palette and layers; check resolves actual carried/declared stock',
  stops: 'nothing: it reads a file and moves nothing',
  instant: true,
  args: { plan: 'object', origin: 'string', file: 'string', name: 'string', facing: 'string', layer: 'number', ...paramArgs() },

  run: async (api, a) => isV2BlueprintArguments(a) ? showBlueprintV2(api, a) : ({ text: showText(api, a) })
}
