// Build a blueprint at an anchor, or go on with one already marked. The world is the only progress record: every job is
// recomputed from what stands, so a second run after a stop, a death or a restart picks up where the blocks say. The
// place is marked kind=build before the first block moves and becomes the blueprint's first tag when the last one is in.
import { buildBlueprint, paramArgs } from '../../src/blueprint-build.mjs'

export default {
  doc: 'blueprint.build name= x= y= z= place=<new name> [facing=] [<param>=] [supply=x,y,z] [clear=true] [partial=true], or place=<a marked build> alone: builds it, layer by layer, stage by stage, and reports built= skipped= stage= missing=',
  stops: 'the last block is in, dusk (night=), a supply chest short of a stage, `stop`, hurt, hungry, or a step that failed twice',
  args: { name: 'string', place: 'string', x: 'number', y: 'number', z: 'number', facing: 'string', supply: 'string', clear: 'boolean', partial: 'boolean', until: 'number', ...paramArgs() },

  run: (api, a) => buildBlueprint(api, a)
}
