# Bedtime reflex places the carried bed

Dan: "please have the place bed be part of the reflex". Tonight the body carried a bed all evening while the
automatic sleep reflex waited for a placed one.

## Global Constraints

- When the reflex would otherwise not go to bed (night, idle conditions met, no usable bed within range) and the
  inventory holds a bed (`*_bed` item), it places that bed on a safe spot next to the body and sleeps in it.
- Safe spot: standable solid floor (boundingBox `block`, not water/lava) with two air blocks above it, adjacent to
  the body; no hostile within 8 blocks (the existing `nearbyHostiles(8)` / `hostileNear` check); not inside any zone
  and never within 50 blocks of a human base; not in water/lava. A bed takes two cells: both cells must pass.
- Human base, for this feature: any zone in `zones` whose name does not start with `<me>-` (case-insensitive), and any
  place of `kind: 'base'` whose `by` is not the body. Distance to a zone is distance to its box (0 inside it).
- No safe spot: the reflex leaves things as they are (no placement, existing behaviour).
- Placement reuses the existing `place` action (`long.place({ item, x, y, z, facing })`), which already handles bed
  facing and checks both halves. No new placement code.
- The placed bed is marked `<me>-bed`, `kind: 'bed'`, `by: <me>` (the mark the driver uses), so the existing
  `automaticBeds` accepts it and later nights find it.
- On waking, the body picks the bed back up ONLY if the reflex placed it in this run; a driver-placed bed stays.
  After a successful pick-up the `<me>-bed` mark goes back to what it was before placement (restored, or removed if
  there was none), so no mark points at an empty spot.
- Events: `bed_placed` when the reflex places a bed, `bed_picked_up` when it picks it back up. Same style as the
  existing `bedtime` event: `emit(type, { ... })` with `at: 'x,y,z'`, `item`, and a short `note`.
- Never dig terrain; the only block broken is the reflex's own bed when picking it up.
- No new dependencies. Comments only say why. Functional style, early returns. Tests: `node:test`, no conditionals
  in test bodies, table-driven where natural; extend `test/automatic-sleep.test.mjs` for sleep.mjs functions and
  the existing `bedtimeCases` table in `test/lib.test.mjs` for `bedtime`.
- Commits end with `Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>`. Never `--no-verify`.

## Task 1: pure decisions (src/lib/sleep.mjs, src/cli.mjs)

Files: `src/lib/sleep.mjs`, `src/cli.mjs` (`bedtime`), `src/lib.mjs` re-exports if that is how sleep.mjs functions
reach bot.mjs (check), `test/automatic-sleep.test.mjs`, `test/lib.test.mjs` (`bedtimeCases`).

1. `export const HUMAN_BASE_MARGIN = 50`.
2. `nearHumanBase(point, { zones, places, me, margin = HUMAN_BASE_MARGIN })` → the offending zone/place name or
   null. Zone distance is point-to-box (clamp each axis to [min(x1,x2), max(x1,x2)] etc). A zone named `<me>-...`
   and a base marked by `<me>` do not count.
3. `carriedBedSpot({ feet, cellAt, zones, places, me, residents = [] })` → `{ x, y, z, facing }` for the bed's FOOT
   cell, or null.
   - `feet`: the body's integer feet cell. `cellAt(x, y, z)` → `{ name, boundingBox }`.
   - Try directions in the fixed order north (0,0,-1), south (0,0,1), east (1,0,0), west (-1,0,0). For direction d:
     foot = feet + d, head = feet + 2d, `facing` = d's name (a player standing at feet looking towards d places the
     foot at the clicked cell and the head one further along d).
   - Each of foot and head: the cell and the cell above are air (`isAir` from world.mjs), the cell below has
     boundingBox `block` and is not water/lava.
   - null when `nearHumanBase(feet, ...)` is non-null, or any resident (villager position) is within 16 blocks of
     the foot (the same 16 that `automaticBeds` uses, so the reflex never places a bed it would then refuse).
   - Returns the first direction that passes.
4. `bedtime` in src/cli.mjs: go to bed when `s.bedNear || s.bedCarried` (all other conditions unchanged). Add
   `bedtimeCases` rows: no bed near but a carried bed with a spot → true; neither → false; carried but a monster
   close → false.
5. Tests in `test/automatic-sleep.test.mjs` (a small grid `cellAt` built from a Map/object, default stone floor at
   y=64, air above): open ground → north spot with facing north; north blocked (stone in the head cell) → south;
   foot over water → that direction skipped; low ceiling (block 2 above floor) → skipped; enclosed on all sides →
   null; inside a foreign zone → null; 49 blocks from a foreign zone → null, 51 → a spot; own zone `observer-base`
   → a spot; a human-marked `kind: 'base'` place within 50 → null, the body's own base → a spot; a villager within
   16 → null. Use `@`-style parametrised tables where it keeps the bodies conditional-free.

## Task 2: wiring (src/bot.mjs, AGENT_GUIDE.md)

1. Reflex tick (bot.mjs, the `bedtime({...})` interval near `automaticSleepBeds`): pass `bedCarried`: true when no
   automatic bed is near, a `*_bed` item is in the inventory, and `carriedBedSpot` finds a spot from the body's
   feet (`feetCell` or the floored position), `zones`, `readPlaces()`, `cfg.username` and villager positions (the
   same list `automaticSleepBeds` builds; factor it if used twice). Compute it only at night. The `bedtime` emit
   note says it is placing its carried bed when that is the case.
2. `sleep(a)` with `a.automatic`: when `bedChoice` errors and it has not placed yet this call, and the inventory
   holds a bed and `carriedBedSpot` finds a spot and `nearbyHostiles(8)` is empty: `await long.place({ item, x, y,
   z, facing })`; save the prior `<me>-bed` place record (or null); write the mark `{ name: '<me>-bed', kind: 'bed',
   x, y, z, by: cfg.username, note: 'placed by the bedtime reflex' }` via `savePlaces` (replacing any of that name);
   remember module-level `reflexBed = { x, y, z, item, prior }`; `emit('bed_placed', { at, item, note })`; then
   `continue` the loop so it sleeps in it. A failed placement throws as any sleep error does (bedtimeReport path).
3. On waking (`bot.on('wake')` is where `woke_up` is emitted): when `reflexBed` is set, pick it up through the job
   scheduler as an automatic job so it never races the driver's work (`submitJob(..., { automatic: true })` with an
   existing action that breaks a block and collects its drop, e.g. `dig` on the foot cell; read how `dig` treats
   zones and collection first). Only when the carried count of that bed item has risen: restore the prior mark
   (or remove `<me>-bed` if there was none), `emit('bed_picked_up', { at, item, note })`, clear `reflexBed`. If the
   pick-up fails, the bed stays standing and marked (a usable bed), `reflexBed` is cleared, nothing else.
   A driver-placed bed (reflexBed unset) is never touched.
4. AGENT_GUIDE.md: one sentence where the bedtime reflex is described.
5. Tests: any pure helper extracted here gets a table test; the bot.mjs wiring is verified by `npm test` staying
   green and `node tools/check-code.mjs` clean.
