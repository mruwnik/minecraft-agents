// One blueprint in full: metadata, the bill per layer and in total, the stages a build would take with the inventory as
// it is, what lint says, and the layers rendered after facing= turns them. It reads a file and never takes the body over.
import { showText, paramArgs } from '../../src/blueprint-build.mjs'

export default {
  doc: 'blueprint.show name= [facing=] [layer=] [<param>=]: metadata, bill per layer and total, stages for the current inventory, lint warnings and the layers after rotation',
  stops: 'nothing: it reads a file and moves nothing',
  instant: true,
  args: { name: 'string!', facing: 'string', layer: 'number', ...paramArgs() },

  run: (api, a) => ({ text: showText(api, a) })
}
