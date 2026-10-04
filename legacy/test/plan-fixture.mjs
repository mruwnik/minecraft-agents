// Old fixture maps are explicit migration inputs; production readers receive only
// the canonical layered record, just as the migrated live world does.
import { migratePlan, planCells as canonicalCells, parsePlacePlan, planBill } from '../src/lib/plan.mjs'
export const canonicalFixture = place => {
  if (!place?.plan && !place?.structure) return place
  const next = migratePlan(place)
  const parsed = parsePlacePlan(next)
  return { ...next, parsed, cells: canonicalCells(next), bill: planBill(parsed) }
}
export const planCells = place => canonicalCells(migratePlan(place))
