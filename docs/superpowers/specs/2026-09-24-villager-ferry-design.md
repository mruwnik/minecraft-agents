# Ferry one villager across a river and back

Ferrying moves one identified adult between prepared docks. It does not change the villager's job or lock trades; keep
that work in `villager.roll`. Use the same UUID on both crossings, and do not start the return leg until the villager is
on foot inside the closed far dock.

## Runbook

1. Prepare both empty docks, leaving their two-cell boat gates open:

   ```text
   villager.dock prepare=true x=<sourceX> y=<sourceY> z=<sourceZ> riverX=<sourceRiverX> riverZ=<sourceRiverZ>
   villager.dock prepare=true x=<farX> y=<farY> z=<farZ> riverX=<farRiverX> riverZ=<farRiverZ>
   ```

2. Before placing a boat or opening the source dock, check the entire hull route through loaded terrain:

   ```text
   villager.route fromX=<launchX> fromY=<launchY> fromZ=<launchZ> x=<landingX> y=<landingY> z=<landingZ>
   ```

   Water-cell targets use the boat's float center, half a block above the water block. The route must be nonascending,
   have clearance for the 1.375-block hull at turns, and contain no unknown cells.

3. Record the UUID of the chosen villager. At the source, call `villager.undock uuid=<uuid> x=... y=... z=... riverX=... riverZ=...`.
   It boards that adult, verifies the lead, then opens the gate. Keep the bot on foot; a player-controlled boat pushes
   villagers away instead of boarding them.

4. Tow the occupied boat using the observed boat ID. Name the boat landing in `x/y/z`; use `centerX/centerZ` when a
   particular position inside a wide gate is needed. `pullVia=x:y:z,...` names short bot waypoints beyond the landing;
   finish with `pullX/pullY/pullZ` to draw the boat to its landing center. For example, the observed west-side route was:

   ```text
   villager.ferry uuid=<uuid> boat=<boatId> x=-132 y=62 z=-169 centerX=-131 centerZ=-168 pullVia=-133:64:-168,-135:64:-168 pullX=-137 pullY=65 pullZ=-170 radius=0.8
   ```

   `plan=true` checks the route without moving. While the bot is in water, ferrying uses bounded swim strokes and checks
   the passenger UUID and leash after each stroke; it stops after two strokes without progress and switches to walking
   after the bot reaches dry ground. Exact arrival is used at explicit `pullVia` points so a narrow shore doorway is not
   mistaken for an adjacent wall cell.

5. Close the far dock around the boat, then call `villager.dock uuid=<uuid> boat=<boatId> x=... y=... z=... riverX=... riverZ=...`.
   It releases the named passenger through one lower service cell, retries stale position observations, and seals that
   cell. If confirmation fails, it reseals the service cell before returning an error. Confirm the same UUID on foot
   inside the closed dock before reboarding with `villager.undock`.

6. Preflight the return route, repeat `villager.undock` and `villager.ferry` with the same UUID, then call `villager.dock`
   at the source. Report success only after the UUID is observed on foot inside the closed source dock.

## Dock and tow constraints

Each dock has a five-by-five perimeter, a three-by-three interior and a two-cell river gate for the 1.375-block boat.
Preparation leaves both gate cells and the underwater channel open. Closing a dock leaves only one lower service cell
open over a temporary sill; the other three gate cells remain closed until the passenger boards and the lead is
verified. Remove the sill last when reopening the channel. For a two-block rear pedestrian gap over an intact floor,
repair the upper wall block first so the bot can still pass the lower cell.

Carry a spare lead and enough supplies to pause safely. Logging out detaches the boat's lead; the dropped item can
despawn while the body is offline. Before resuming, observe the exact passenger and boat, check the current leash,
and attach a carried replacement if needed.

The boat route must never rise, including across turns and the final hull footprint. Reject a route with unloaded terrain
or blocked hull clearance before attaching the lead or moving. A player-walkable uphill bank can still be unreachable to
the boat. On a wet tow, verify movement progress and the exact passenger/leash every stroke; stop on UUID loss, leash
loss, or excessive boat lag.

## Verification

Focused tests cover monotonic descent and turns, uphill rejection, water float-height normalization, hull width and
destination clearance, unloaded terrain, water-stroke progress and stall handling, water-to-dry switching, UUID/leash
loss, exact pull waypoint arrival, dock closure before release, stale release observations, and recovery sealing.

### Live progress (2026-09-24)

The forward crossing carried villager UUID `95299feb-5cfb-428a-9ac1-72de2d36dc12` in boat 6468 from the hilltop source
to the prepared west dock at `(-132,63,-169)`. The boat reached `(-131.31,62.41,-168.31)` with the passenger UUID and
lead observed. The west dock then released that UUID on foot at `(-131.46,62.13,-168.42)`, reported secure with its
gate closed, and removed the boat. The bot used the rear exit at `(-134,64,-168)` and `(-134,65,-168)`; the final pull
point `(-137,65,-170)` centered the boat inside the gate. A prior one-cell arrival radius stopped at an adjacent wall
cell, so the explicit pull waypoints now use exact arrival.

After 108 seconds, the same UUID was recaptured as the sole passenger in boat 33954 at `(-131.31,62.41,-168.31)`;
the bot held its lead and opened the west gate. On the return crossing, boat 33954 carried that UUID to the east dock
at `(-113.69,62.52,-172.73)` with the passenger and lead verified. `villager.dock` released it on foot at
`(-113.45,62.41,-172.71)` and reported the gate
closed and secure, removed boat 33954, and a fresh entity observation independently found the villager walking at
`(-112.52,63,-173.23)`. Two grass obstructions at `(-116,62,-174)` and `(-115,62,-174)` were cleared from the east
channel during preparation.

The full roundtrip is physically verified for UUID `95299feb-5cfb-428a-9ac1-72de2d36dc12`: it was released on foot
inside the closed west dock, recaptured as the sole passenger in boat 33954, then released on foot inside the closed
east dock. The return needed bounded retries for water-surface selection and transient lag, with manual channel
preparation; this evidence does not represent one uninterrupted automatic run.
