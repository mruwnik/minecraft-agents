import { buildBlueprintV2 } from '../../src/blueprint/v2.mjs'
// Freeze the validated source and allocation before building. Placement progress is observed from the world on resume.

export default {
  doc: 'blueprint.build (name=<catalog> | plan=<v2 object> | file=<caller JSON path>) x= y= z= place=<new name> [facing=] [supply=x,y,z] [clear=true] [partial=true], or place=<a marked build> alone: builds it, layer by layer, stage by stage, and reports built= skipped= stage= missing=',
  stops: 'the last block is in, dusk (night=), a supply chest short of a stage, `stop`, hurt, hungry, or a step that failed twice',
  args: { plan: 'any', origin: 'string', file: 'string', name: 'string', place: 'string', x: 'number', y: 'number', z: 'number', facing: 'string', supply: 'string', clear: 'boolean', partial: 'boolean', until: 'number' },

  run: buildBlueprintV2
}
