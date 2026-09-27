// Everything blueprint.build would refuse over, with nothing built: the stage table first (so the driver fetches
// everything before starting), then obstacles by block and cell, foundation cells that are not solid, the clearance over
// the roof, zones and other agents' places, unloaded chunks, and what lint says. With supply= the chest's contents count
// toward what is at hand (that is the one walk it makes).
import { checkBlueprint, paramArgs } from '../../src/blueprint/build.mjs'

export default {
  doc: 'blueprint.check name= x= y= z= [facing=] [<param>=] [supply=] [clear=true], or place=<a marked build>: the stage table and everything build would refuse over, nothing built',
  stops: 'nothing: it reads the world and the map; with supply= it walks to that chest once',
  args: { name: 'string', place: 'string', x: 'number', y: 'number', z: 'number', facing: 'string', supply: 'string', clear: 'boolean', ...paramArgs() },

  run: (api, a) => checkBlueprint(api, a)
}
