# View dump format, version 1

The body (`out/body.cjs`) dumps what mineflayer knows about the world to disk so an external process can render what the
body sees. The body does no rendering and no per-block work. It copies mineflayer's packed chunk data, deflates it off
the main thread and writes files. Writer: `engine/js/view.mjs`. Reader helper: `decodeColumnFile` in the same module.

Disable everything with `BODY_VIEW=0` (no directories are created). Enabled by default.

## Files

| File | Shared by | Written |
| --- | --- | --- |
| `state/worlds/<world>/chunks/<cx>.<cz>.bin` | every body in the world | on chunk load and block update |
| `state/agents/<name>/view/pose.json` | one agent | every 100 ms if changed, at least every 2 s |
| `state/agents/<name>/view/hud.json` | one agent | on change, at most once per second |

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
- Known limitation: the pose shows the client's belief, not the server's facing. After a console `tp ... 180 0` the pose
  yaw reads 0 for about 3.5 s, then flips to the body's last look direction, while the server reports -180 throughout.
  This is a mineflayer teleport-rotation issue on this server version (26.1), not a dump bug.
- Errors never escape a bot event handler. A write error emits a `view.error` warn event at most once a minute.
- `view.stats` (info, once a minute): `{columns, bytes, ms, poses, huds}` since the last stats event. `ms` is main-thread
  time in `dump()` plus JSON building (`performance.now()`), `bytes` is compressed column bytes written.
