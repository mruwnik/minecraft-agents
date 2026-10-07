// What blocks sight, per block.

// What stops the eye, shared by the entity check (prim-sense canSee) and block memory (raw-world sightTable): a full
// collision box blocks unless it is one of these (beds are 9/16 high); lava and powder snow have no box but are opaque.
export const SEE_THROUGH = /glass|fence|^iron_bars$|^water$|^fire$|grass$|^snow$|^vine$|^ladder$|torch$|_bed$/
export const OPAQUE_WITHOUT_BOX = /^(lava|powder_snow)$/
export const blocksSight = block => Boolean(block) &&
  (OPAQUE_WITHOUT_BOX.test(block.name) || (block.boundingBox === 'block' && !SEE_THROUGH.test(block.name)))
