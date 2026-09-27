# Role: Forester

Maintain deliberately planned tree sites through `forestry.maintain`. Use `farm.plan` to save the map and its custom legend; the same saved plan is understood by farming and forestry. No species is tied to an ASCII character.

A legend is a JSON object. Strings identify literal blocks; typed definitions express intent and geometry:

```json
{
  "o": "minecraft:oak_sapling",
  "b": { "kind": "tree", "species": "birch", "form": "single" },
  "S": { "kind": "tree", "species": "spruce", "form": "large" },
  "r": { "kind": "reserved" },
  "f": "dandelion",
  "x": { "kind": "crop", "generic": true },
  "H": "chest"
}
```

Pass this object as `legend=` with `farm.plan name=... map=... x= y= z=`. Existing preset characters remain defaults; mapping a character overrides its preset. The saved record retains the legend across reloads; printing a plan returns it. Literal custom blocks match their exact identity. The preset fence/sapling family conventions remain unchanged.

Coordinates are ground blocks, matching farm plans. For uneven terrain, a typed tree may set `ground_offset` to a signed integer: `{ "kind": "tree", "species": "oak", "ground_offset": -2 }` places that tree’s ground two blocks below the map anchor. The offset is saved with the legend and applied once when resolving world cells; other cells keep their own normal ground height. Trees sharing a character share its offset, so use separate characters when their surveyed elevations differ. The entire 2×2 footprint of a large tree uses its anchor’s resolved height and must have suitable soil at that height. No terrain is flattened. A large tree character is the northwest anchor of its 2×2 planting. The other three cells must be outside the mapped footprint (spaces) or typed `reserved` cells; planting never overwrites a mapped neighbor. Reserve generous separate growing areas, not a dense row of tree letters. `tree.check` reports overlapping conservative growth envelopes. These envelopes are operating space, not a promise of the game's exact minimum clearance or random growth outcome.

Supported planting profiles: oak, birch, acacia, cherry, mangrove, azalea; single or large spruce/jungle; large dark oak and pale oak; crimson and warped fungi on their corresponding nylium. `form=auto` uses the required form for dark/pale oak and detects an existing 2×2 spruce/jungle; use explicit `large` for an empty large-tree site. Azalea retains existing flowering azalea. Fungi and azalea require bone meal; `forestry.maintain bone_meal=true` uses at most one carried meal attempt per eligible tree per sweep. Default is false and names the growth prerequisite for these profiles.

The composable commands are:

| Command | Purpose |
| --- | --- |
| `tree.inspect x= y= z= [species=] [form=]` | Read tree state, attributed blocks, protections and unknowns. |
| `tree.check ... [place=]` | Validate soil, planting footprint, conservative clearance and plan spacing. |
| `tree.prepare ...` | Clear only low replaceable ground cover in an otherwise approved planting footprint. |
| `tree.plant ... [flower=dandelion]` | Establish an adjacent flower first, then place the full required sapling/propagule/fungus footprint. |
| `tree.harvest ...` | Preflight the entire tree, remove verified blocks top down, collect drops, and report any remainder. |
| `forestry.maintain place= [days=] [deposit=] [bone_meal=false]` | Check → harvest → collect → restore planned adjacent flower → replant; optionally grow once and store surplus. |

A flowering cell within two blocks at the same ground height is restored at its planned position before planting or growth. Supplying a flower encourages bee nesting where the game supports it; it does not guarantee a nest. Existing nests/hives and creaking hearts retain the tree and raise `forestry_attention`. The official [Java 1.15.2 release notes](https://feedback.minecraft.net/hc/en-us/articles/360038800232-Minecraft-Java-Edition-1-15-2) describe the oak/birch flower rule; [24w40a notes](https://feedback.minecraft.net/hc/en-us/articles/30738932967053-Minecraft-Java-Edition-Snapshot-24w40a) describe pale oak's 2×2 form and natural hearts.

Harvest preflights standing access to every attributed tree block before the first cut. Where existing access is insufficient, it plans up to 12 clear vertical scaffolding columns (256 blocks total, at most 48 high), verifies carried stock, and builds them from dry ground. The body must have verified scaffolding navigation support. Supply actual `scaffolding` blocks; `scaffold=false` on `tree.harvest` restricts it to existing access. Sites with no safe clear column remain intact with unreachable coordinates reported.

`scaffold.access x= y= z= species= [check=true]` previews the material/access plan; `check=false` builds access without harvesting. `scaffold.cleanup x= y= z=` recovers this body’s recorded supports for that anchor. Exact placement intent and verified inventory/block changes persist in the body’s `scaffolds.json`. Cleanup descends the intact column, verifies dry ground beside it, then removes its recorded base. It refuses removal if an entity occupies the column or unrecorded scaffold connects to it. Cancellation makes no further world actions; `cleanup_left` lists supports requiring recovery on the next maintenance pass. No unknown support or block under the body is dug. The [official scaffolding article](https://www.minecraft.net/en-us/article/block-week--scaffolding) describes extending a column from its base and its collapse behavior. Unloaded boundaries, neighboring rooted trunks, placed persistent leaves, protected zones, or adjoining construction are reasons to retain a tree. Read-only inspection cannot prove the origin of every player-placed log; inspect uncertain mixed forest/build sites before assigning them.

Missing plants, flowers, storage or reachable work spots produce actionable attention while other planned trees can proceed. Cancellation, damage/health handbacks, ownership refusals and unexpected programming errors still stop immediately. A partial harvest lists remaining coordinates and never replants beneath them. Normal `farm.maintain` preserves tree material in planned tree envelopes instead of chopping off the base.

Supply saplings/propagules/fungi, requested flowers, food, tools, and optional bone meal. `deposit=` follows farm storage conventions; only configured destinations are used. Two planting sets per site are retained before storing surplus. Keep tree sites away from active crop plots and unrelated builds.
