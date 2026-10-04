# Farm and forest plans in three dimensions

Saved plans use `structure.legend` and `structure.layers`, the same block-coordinate geometry as building structures. Each layer has an integer `y` and rectangular `rows`; columns increase east (`x`) and rows south (`z`) from the saved anchor. Layer heights locate the actual block, not its supporting ground. A wheat or sapling block at layer 1 stands on ground at layer 0.

```json
{
  "legend": {
    "w": "minecraft:wheat",
    "c": "carrots",
    "~": "water",
    "o": { "kind": "tree", "species": "oak", "form": "single" },
    "f": "dandelion",
    "*": { "kind": "crop", "generic": true },
    "r": { "require": "preserve" }
  },
  "layers": [
    { "y": 0, "rows": ["~___", "____"] },
    { "y": 1, "rows": ["_ww_", "____"] },
    { "y": 4, "rows": ["~___", "____"] },
    { "y": 5, "rows": ["_cc_", "____"] }
  ]
}
```

Pass the JSON as `farm.plan name=... x=... y=... z=... structure='...'`; `check=true` validates without saving. Printing the name returns the canonical structure. An unspecified `_` is unconstrained. A `.` explicitly requests air; it is **not** a path. A path can be mapped explicitly, for example `{ "kind": "path", "ground": "dirt_path" }`. A preserve/reserved cell remains part of the claimed footprint but generates no clearing or placement work.

Legend strings identify literal blocks. Typed crop/tree definitions express ongoing maintenance intent, which a building block alone cannot describe. Generic crops preserve compatible planted crops and sow suitable carried seeds. Tree profiles specify species and optional `single`, `large` or `auto` form; large trees use the northwest sapling as their anchor. Reserve their other three planting cells and adequate growth space. Trees at different heights belong on different layers. Construction material slots and specialized building recipes are rejected rather than silently ignored. Farm layers span -4 through 51 and are bounded to 64×64 and 16,384 declared cells.

Literal water represents only water. To request the legacy covered channel, use a typed water definition with `cover: "oak_slab"`. Typed legacy torch-post intent creates both its post and upper torch; a literal torch names the single actual torch block.

`farm.plan map=... legend=...` is an explicit legacy importer. It immediately converts to the canonical structure before saving. Preset characters and legacy tree `ground_offset` are interpreted only during that conversion; layered definitions use their layer elevation directly. Runtime farm, forest and pen consumers require canonical saved structures.

## Migrating a saved world map

`node tools/migrate-plans.mjs --world main` is a read-only preview. It verifies every cell's world coordinates and maintenance specification plus its material bill, and prints the file digest. All saved plan kinds are covered, including pens; ownership and unrelated records remain unchanged. `--file=<path>` points at a map directly instead, for a backup or a copy outside `state/worlds/`.

After coordinating all old body writers, run `node tools/migrate-plans.mjs --world main --apply --expect=<reviewed sha256>`. The tool takes the shared map writer lock, refuses a changed digest, creates an exact backup, rechecks the source and replaces the file atomically. New body writers use the same lock. Old processes must be stopped or coordinated before applying because their older code does not honor that lock. Reload bodies after applying so they consume the canonical map. Repeating the tool on migrated records is a no-op.
