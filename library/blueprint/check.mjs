import { checkBlueprintV2 } from '../../src/blueprint/v2.mjs'
// Everything blueprint.build would refuse over, with nothing built: the stage table first (so the driver fetches
// everything before starting), then obstacles by block and cell, foundation cells that are not solid, the clearance over
// the roof, zones and other agents' places, unloaded chunks, and what lint says. With supply= the chest's contents count
// toward what is at hand; checks require the chest to be observable without walking.

export default {
  doc: 'blueprint.check (name=<catalog> | plan=<v2 object> | file=<caller JSON path>) x= y= z= [facing=] [supply=] [clear=true], or place=<a marked build>: the stage table and everything build would refuse over, nothing built',
  stops: 'nothing: it reads the world and the map; declared supply must be within observation reach',
  args: { plan: 'object', origin: 'string', file: 'string', name: 'string', place: 'string', x: 'number', y: 'number', z: 'number', facing: 'string', supply: 'string', clear: 'boolean' },

  run: checkBlueprintV2
}
