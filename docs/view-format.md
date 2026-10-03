# View dump format, version 1

The body (`out/body.cjs`) dumps what mineflayer knows about the world to disk so an external process can render what the
body sees. The body does no rendering and no per-block work. It copies mineflayer's packed chunk data, deflates it off
the main thread and writes files. Writer: `engine/js/view.mjs`. Reader helper: `decodeColumnFile` in the same module.

Disable everything with `BODY_VIEW=0` (no directories are created). Enabled by default.

## Files

| File | Shared by | Written |
| --- | --- | --- |
| `state/worlds/<world>/chunks/<cx>.<cz>.bin` | every body in the world | on chunk load and block update |
| `state/worlds/<world>/biomes.json` | every body in the world | on attach, only if the content differs from the file |
| `state/agents/<name>/view/pose.json` | one agent | a write attempt on every physics tick (about 20 a second), only if changed, at least every 2 s; `BODY_VIEW_POSE_HZ` > 0 switches to a timer at that rate |
| `state/agents/<name>/view/hud.json` | one agent | on change, at most once per second |

`biomes.json` is `{"v":1,"mcVersion":...,"biomes":[{"id":0,"name":"badlands"},...]}`, sorted by id, names without the
`minecraft:` prefix. It is the server's own biome registry (from its login `registry_data`), which the biome ids inside
chunk data index; ids differ between servers and versions. Not written when the bot has no biome registry.

All writes are atomic: write `<file>.tmp.<pid>`, then `rename`. A reader never sees a partial file.
`state/` is ignored by the root `.gitignore`.

## Chunk column file

`cx`, `cz` are chunk coordinates and may be negative (`-3.12.bin`). The file is `zlib.deflate` (level 1) of:

1. uint32 little-endian `N`, the byte length of the JSON header.
2. `N` bytes of UTF-8 JSON header:
   `{"v":1,"x":cx,"z":cz,"t":<ms epoch>,"body":"<agent>","mcVersion":"<bot.version>","minY":<int>,"worldHeight":<int>,"parts":[{"name":"sections","len":L},{"name":"biomes","len":L},{"name":"light","len":L,"meta":{...}}]}`
3. The parts' raw bytes concatenated in header order. Parts are ordered, `len` is in bytes.

`decodeColumnFile(buffer)` returns `{header, sections, biomes, light: {meta, buffer}}`. `restoreColumn(ChunkColumn, decoded)`
loads a fresh prismarine `ChunkColumn` (constructed with `{minY, worldHeight}` from the header) from it.

### Parts, as built for prismarine-chunk 1.41.0 / mcVersion 26.1 (the 1.18 ChunkColumn)

- **sections**: the exact Buffer from `column.dump()`. For this chunk format it holds, for each of the `worldHeight>>4`
  sections from the bottom, the block-state container followed by the biome container, interleaved. Load with
  `column.load(buffer)`.
- **biomes**: empty (`len` 0). In the 1.18+ format `dumpBiomes()` returns `undefined` because the biomes are already
  inside `sections`.
- **light**: prismarine-chunk does have a light dump API, `column.dumpLight()`, returning
  `{skyLight, blockLight, skyLightMask, blockLightMask, emptySkyLightMask, emptyBlockLightMask}`. `skyLight` and
  `blockLight` are arrays of 2048-byte nibble buffers (4 bits per block, 4096 blocks), one per set bit of the matching
  mask, in section order. The masks are arrays of `[hi, lo]` signed int32 pairs (a long array). The light part is
  `skyLight` buffers then `blockLight` buffers, concatenated. `meta` is
  `{"skyCount":n,"blockCount":n,"sectionBytes":2048,"skyLightMask":[...],"blockLightMask":[...],"emptySkyLightMask":[...],"emptyBlockLightMask":[...]}`.
  To restore, slice the buffer into `sectionBytes` pieces (first `skyCount` are sky) and call
  `column.loadParsedLight(skyLight, blockLight, skyLightMask, blockLightMask, emptySkyLightMask, emptyBlockLightMask)`.
  Light section index 0 is the layer one section below `minY`, index `numSections+1` one above the top.

## pose.json

```
{"v":1,"t":ms,"world":"<world>","status":"online"|"offline","dimension":"<bot.game.dimension>","mcVersion":"...",
 "pos":{x,y,z},"eye":{x,y,z},"yaw":rad,"pitch":rad,"velocity":{x,y,z},"onGround":bool,
 "entities":[{id,type,name,kind,username?,pos:{x,y,z},yaw,pitch,height,width,health}],"timeOfDay":n,"rain":n}
```

`eye` is position plus `bot.entity.height - 0.18` (1.62 standing, 1.27 sneaking, 1.62 if height is unknown). Yaw and
pitch are raw mineflayer radians. `entities` lists every entity except the body within 48 blocks (`kind` and `health`
are `null` when unknown). Change detection rounds position, yaw and pitch to two decimals. When offline the file is
written once: the last full pose (position, eye, yaw, pitch, entities as they were) with `status` `"offline"` and a fresh
`t`, and not updated again until the body is back. If no pose was ever written it is the short record
`{"v":1,"t","world","status":"offline","mcVersion"}`.

## hud.json

```
{"v":1,"t":ms,"health","food","saturation","oxygen","xp":{level,points,progress},"effects":[{name,amplifier,duration}],
 "held":{name,count}|null,"inventory":[{slot,name,count}],"window":<type>|null}
```

`armor` is omitted. Empty inventory slots are not listed.

## Behaviour and limits

- Chunk load queues the column; a block update marks its column dirty (coalesced). A flush runs every 500 ms and writes at
  most 8 columns, so a login burst of hundreds of columns spreads over several seconds. The `dump()` runs on the main
  thread, the deflate, write and rename are async. A column whose previous write is still in flight stays queued.
- Columns are never deleted on unload. The directory is a persistent mirror of every column any body has visited.
- Known limitation: last writer wins. Two bodies that see different versions of a column (different times, or one has
  stale data) overwrite each other. The `t` and `body` header fields say who wrote it and when.
- Light: what the server sent with the chunk, kept current by the body itself. The server sends no light update after
  a block change (the vanilla client relights locally; mineflayer does not: a probe that placed a torch saw only
  `block_change` packets), so `engine/js/view.mjs` relights locally (`engine/js/light.mjs`). For each changed block P
  (state actually changed) it recomputes the box P±16 (extended down through a sky-transparent run below P, plus 15),
  holding the box's one-cell shell fixed and recomputing the interior from emitters and the shell with vanilla rules
  (per-state emission and filter, shape-occluded faces of slabs, stairs, snow layers..., lossless sky going straight
  down). A change cannot reach further than 15, so the result is exact. Results live in an overlay of per-section
  light owned by the writer (mineflayer's column light is never written: its accessors scramble the order), used when
  the column is encoded, and dropped when the server sends the column again (chunk load) or unloads it. Every column
  with a changed cell is rewritten. Overlapping boxes of one flush merge up to 48x48 by 64. The relight runs at the
  500 ms flush and is budgeted to 10 ms per flush (a box in progress finishes); changes over budget are carried to
  the next flush, and their column is not written before it is relit. An `update_light` packet, if a server sends one,
  marks its column dirty too.
  Guarantee and evidence: on five regions captured from the live server (`tools/light-capture.mjs`,
  `engine/js/fixtures/light/`: a forest, a lava cave, an ocean, a lit room with torch, lantern, lava, glowstone, lit
  furnace, campfire, slab, stairs, glass, leaves, sea pickle, snow layer, and someone's torch-lit build), relighting
  from the shell reproduces the server's light with 0 mismatches (`engine/js/light-oracle.test.mjs`). Live, a torch,
  a lantern and a roof hole relit by the body matched the server's light after the chunks were re-sent, 0 of 29,791
  cells different. A change shows in the dump 0.5-1.2 s after the command.
  Cost (`view.stats`: `relightMs`, `relightMaxMs`, `relightBoxes`, `relightCells`, `relightCarried`, the breakdown
  `relightStatesMs/LightMs/FloodMs/WriteMs`, and `relightTableMs` once per attach, ~100 ms): one torch or a steady
  1x2 tunnel dig stays at or under ~10 ms per flush (about 4 ms per box); a 2000-block console `fill` runs 19-25 ms
  in a single flush (one capped box over the budget) and 83 ms in the first flush after a login (cold code).
- Hazard for anything that reads light: prismarine-chunk's `loadParsedLight` (prismarine-chunk 1.41.0, checked on
  26.1) reads the vanilla 2048-byte nibble arrays as big-endian longs, so `column.getSkyLight/getBlockLight`, and
  mineflayer's `block.light` / `block.skyLight`, return the value of another x in the same 16-cell row: the cell at
  x is read from x' = 14 - 2*(x >> 1) + (x & 1). Evidence: a torch at 3520 71 -3521 read block 0 / sky 15 through
  prismarine, while the dumped bytes read in vanilla order gave 14 at the torch, 15 inside a glowstone and 2-9 sky
  inside a roofed room. `dumpLight` undoes the same swap, so the light part of the file is in vanilla order (cell i of
  a section: byte i >> 1, low nibble when i is even), which `decodeLight` in `tools/view/web/decode.mjs` reads. Read
  light from the file that way, not through a restored prismarine column.
- Known limitation: the pose shows the client's belief, not the server's facing. After a console `tp ... 180 0` the pose
  yaw reads 0 for about 3.5 s, then flips to the body's last look direction, while the server reports -180 throughout.
  This is a mineflayer teleport-rotation issue on this server version (26.1), not a dump bug.
- Errors never escape a bot event handler. A write error emits a `view.error` warn event at most once a minute.
- `BODY_VIEW_POSE_HZ` (read once at start) sets the pose write rate. The default `0` is a write attempt on every physics tick (about 20 a second), only if the pose changed and at least every 2 s; a value above 0 switches to a timer at that rate.
- `view.stats` (info, once a minute): `{columns, bytes, ms, poses, poseMs, poseBytes, huds}` since the last stats event. `poseMs` is main-thread time building, change-checking and stringifying poses, apart from `ms`; `poseBytes` is pose JSON bytes written. `ms` is main-thread
  time in `dump()` plus JSON building (`performance.now()`), `bytes` is compressed column bytes written.

## Driving

The page can take over a body and drive it by hand (movement only). The "take over (G)" button and the keys:

- G takes over or releases. W/A/S/D forward/left/back/right, Space jump, Shift sneak, R sprint (not Ctrl: Ctrl+W closes the tab).
- Arrow keys turn 15 degrees and tilt 10 degrees. Click the canvas for pointer-lock mouse look.
- F (free camera) is ignored while driving.

While this page holds the body the view also has a red border (`#drive-border`, shown only under `body.driving`). The moment it no longer holds the body, for any reason (own release, lease timed out, forced release, body offline, engine restarted, failed request), every marker of control is cleared, keys and mouse stop, and pointer lock is exited; the poll also runs when a hidden tab returns.

A red banner, shown to every viewer while anyone drives, names who. Leave rule: losing pointer lock while driving
(after having had it, so a single Esc) sends stop, then releases; a hidden tab or window blur sends stop and keeps the
takeover; pagehide/beforeunload sends stop (fetch keepalive) but not release, so the body's 15 s idle timeout gives the
body back if the page does not return (an embedding dashboard releases when its popup closes). The page pings every 500 ms (the body releases untimed controls after 1 s of
silence). Requests are serialised, so a key-up never overtakes its key-down. Every POST times out after 1500 ms, so a
hung request cannot hold the queue (a queued stop must not sit behind it); the body's 1 s dead-man covers the gap.

- `?who=<name>` sets the driver name (default `view`; must match `[A-Za-z0-9:_-]{1,40}`, else `view`).
- Esc while driving without pointer lock releases; with lock, the Esc exits the lock, which itself releases (see the leave rule).
- `?embed=1` (the dashboard embeds the page same-origin in an iframe): a click on the canvas takes over when this page is
  not driving and nobody else holds the body; while driving, a click requests pointer lock as usual.
- When framed, every render posts `{type: 'drive', driving, manual, expiresAt}` to the parent (`manual` is the lease view
  or null, `expiresAt` epoch ms or null), target origin `location.origin`. `window.__drive` offers `take()`, `release()`
  and `state()`.

Server route `GET|POST /drive/<name>` relays to the body's control socket (`engine/README.md`, Manual takeover); `who`
defaults to `view`. It is the first write route of the view server. The server stays bound to 127.0.0.1 and answers 403 to
a Host that is not 127.0.0.1, localhost or [::1], to an Origin that is not `http://<Host>`, and to a POST whose
Content-Type is not `application/json` (this blocks cross-site form posts and DNS rebinding). It answers 503 `no-body`
when the body is not running.
