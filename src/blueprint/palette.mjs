// Representative intent is deliberately separate from stock allocation.
import { compileBlueprintStructure, concreteBlueprint } from './compiler.mjs'
import { materialCandidates } from './materials.mjs'
export const representativeAssignments = ir => Object.fromEntries(ir.objects.map(object => [object.id, object.block ?? materialCandidates(ir.document.materials[object.material])[0]]))
export function representativeBlueprint (document) {
  const ir = compileBlueprintStructure(document)
  return concreteBlueprint(ir, representativeAssignments(ir))
}
