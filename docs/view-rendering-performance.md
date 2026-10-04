# View rendering: performance and the browser renderer

A body dumps what it sees (`docs/view-format.md`): chunk columns in `state/worlds/<world>/chunks/`, plus `pose.json`
and `hud.json` in `state/worlds/<world>/agents/<Name>/view/`. There are two renderers for those files.

## 1. Node PNG path (snapshots)

`node tools/view-render.mjs <Name> --world <world> [--width --height --fov --dist --watch ms --bench s]` loads the columns with
prismarine-chunk, builds a block grid around the eye and raycasts on the CPU with `src/vision/renderer.mjs`. Textured,
and good for an agent that wants a snapshot every couple of seconds.

| Size | Frame time | fps |
| --- | --- | --- |
| 320x180 | 46.6 ms mean (grid 8.4, raycast 37.5, PNG 0.7) | 21.4 |
| 640x360 | raycast ~136 ms | ~7 |
| 1280x720 | raycast ~729 ms | ~1.4 |

(ProbeFight, `--bench 10`, dist 64.) There is unused groundwork that is not wired in: `tools/view/grid.mjs`
`gridCache()` keeps the grid across frames, which would remove the 8 ms grid build, and `tools/view/pool.mjs` +
`raycaster.mjs` + `raycast-worker.mjs` raycast in bands on worker threads. `view-render.mjs` has no threads flag.
The cost per pixel is linear, so the CPU path cannot reach a live 720p view. The live view therefore runs in the browser.

## 2. Browser renderer (live view)

`node tools/view-serve.mjs [--port 3702] [--host 127.0.0.1]`, then open `http://127.0.0.1:3702/?agent=<Name>`.
URL params: `agent`, `radius` (columns, default 8), `w`/`h` (fixed render size), `fov` (default 70, horizontal, the
same as the Node renderer), `dist` (blocks, default radius*16), `time` (ticks, overrides the pose's `timeOfDay`, e.g.
`&time=18000` for midnight) and `rain` (0..1, overrides the pose's `rain`). `time` only changes the shading: the
dumped sky light does not depend on the time of day.

Server (`tools/view/serve.mjs`, no dependencies beyond the repo's own):
- `GET /agents`: the agents that have a pose file.
- `GET /pose/<world>/<Name>?radius=R`: SSE. It polls the files' mtimes every 50 ms and sends `pose` `{mtime, sentAt, pose}`,
  `hud` `{mtime, hud}`, and `column` `{cx, cz, mtime}` when a column file within R of the eye changes. Columns are
  polled every 250 ms.
- `GET /columns/<world>/<cx>.<cz>.bin`: the raw deflated file. `GET /hud/<world>/<Name>`.
- `GET /blocks/<mcVersion>.json`: the material table, ~700 KB for 26.1 (29873 states, 3441 materials). It maps each
  state id to a material, deduplicated by content. A material has a `kind` (cube/box/cross/water/lava), `tex` (texture
  layer for top, side and bottom, -1 when there is no texture file), `box` (bounding box of the collision shapes in
  1/16 units, with visual overrides for snow layers, rails, pressure plates, powder snow, soul sand, mud), `flags`
  (CUTOUT 1, TRANSLUCENT 2, AXIS_X 4, AXIS_Z 8, EMISSIVE 16, CULL_SAME 32), `emit` (registry light emission) and the
  old average top/side/bottom colours (the fallback when there is no texture). The table also includes the column
  `format` the decoder needs and `textures: {size, levels, names}` (`tools/view/materials.mjs`,
  `tools/view/web-format.mjs`).
- `GET /textures/<mcVersion>.bin`: the texture array, ~1.1 MB for 797 layers: 16x16 RGBA, 5 mip levels, level-major
  (all layers of level 0, then level 1, ...). Built from `textures/` by `tools/view/textures.mjs`: the first frame of
  animated strips, 32-wide textures box-downsampled, the fixed temperate grass/foliage/water tints of
  `src/vision/renderer.mjs` baked in (there is no biome colormap in `textures/`, so no per-biome tint). Mips use an
  alpha-weighted colour, and binary-alpha textures (leaves, plants, glass) keep their alpha coverage at each level so
  the shader's 0.5 alpha test neither empties nor fills them at a distance. Table and textures are built together once
  per version (about 1.8 s) and cached.

Page (`tools/view/web/`):
- `decode.mjs` inflates the file with `DecompressionStream`. It decodes the 1.18+ paletted containers (single,
  indirect and direct; 26.1 has no size prefix and has a fluid count) in the browser. Tests check it cell by cell
  against prismarine on synthetic columns (1.20.4, 1.21.4, 26.1) and on 3 real files.
- `camera.mjs` builds the basis of `cameraFor` in `src/vision/renderer.mjs`. It is tested against `directionFor` and
  the renderer's side convention.
- `decode.mjs` also decodes the `light` part (`decodeLight`): one byte per cell, `sky << 4 | block`, in the vanilla
  nibble order of the dumped bytes. A section with no sky data that is not flagged empty counts as open sky (15).
  Note: prismarine's `getSkyLight`/`getBlockLight` on a restored column (and so mineflayer's `block.light`) scramble
  the x order inside each 16-cell row, because `loadParsedLight` reads the vanilla byte arrays as big-endian longs.
  The dump file itself is right; do not use prismarine light values as ground truth.
- `gl.mjs` keeps a toroidal R16UI 3D texture of material ids for (2R+1)² columns at full height (272x384x272 at
  R=8), an R8UI light texture of the same size, both with one 16x384x16 `texSubImage3D` per column, and an R8UI
  section-occupancy texture. A WebGL2 fragment shader does a two-level DDA: it skips empty 16³ sections, then steps
  through blocks. At a hit:
  - **Textures**: a `TEXTURE_2D_ARRAY` with mips, Minecraft's face UVs (`faceUV` in `shading.mjs`, mirrored in GLSL),
    logs with `axis` x/z turned, the mip level from distance and the angle to the face (derivatives are discontinuous
    across voxels). Cutout blocks (leaves, glass, plants, doors) are alpha-tested at 0.5 and the ray goes on through
    transparent texels; faces between two blocks of the same glass are culled. Plants are Minecraft's two diagonal
    quads. Water and translucent blocks (stained glass, ice, slime) blend once per run of the same material.
  - **Partial blocks**: kind `box` is intersected with its bounding box inside the cell, so slabs, snow layers,
    carpets, farmland, paths, doors, trapdoors and fence posts have their real size, and a ray passes over them.
    Stairs, connected fences and walls are their bounding box (stairs a full cube).
  - **Light**: Minecraft's lightmap (`lightColor` in `shading.mjs`: the f/(4-3f) brightness curve, block light ×1.5
    with the warm tint, sky light scaled by `skyDarken(time, rain)` and tinted blue at night, the 0.04 grey floor and
    the default 0.5 gamma). Full-cube and box faces get smooth lighting with ambient occlusion: each corner averages
    the light of the cell the face looks into and its three neighbours around that corner (an opaque neighbour takes
    the centre's light and darkens the corner to 0.2), interpolated across the face; a face inside its cell (a slab
    top) looks into the cell itself. Plants, water and glass take their own cell's light, emissive blocks are full
    bright, entities take the light of the cell in front of them. Outside the window counts as open sky. Face shading
    is Minecraft's (1/0.5/0.8/0.6), the sky fades to a night colour with the same darkening, and fog is unchanged.
  Entities are drawn as boxes, up to 64.
- `app.mjs` provides the agent picker and the SSE connection. It fetches columns nearest-first with 6 in flight and
  refetches a column on its `column` event. The overlay shows fps, pose age, file→frame latency, columns
  loaded/wanted and HUD numbers. Press F for a free camera (mouse look + WASD). `window.__view` exposes the stats for
  automation.
- `tools/view-web-bench.mjs <url> [--angle vulkan|gl] [--screenshot f.png]` runs headless Chromium over CDP and
  prints the fps, the latency percentiles and the WebGL renderer string.

### Numbers (headless Chromium, `--enable-gpu`, real GPU: NVIDIA RTX 3070, `/dev/dri/renderD128`)

| ANGLE backend | Size | fps (vsync) | fps (`--disable-gpu-vsync --disable-frame-rate-limit`) | file→frame p50/p95 |
| --- | --- | --- | --- | --- |
| Vulkan | 1280x720 | 60 | 1633 | 43 / 61 ms |
| Vulkan | 1920x1080 | 60 | 729 | 38 / 55 ms |
| OpenGL | 1280x720 | 60 | 2184 | 41 / 58 ms |
| OpenGL | 1920x1080 | 60 | 584 | 27 / 38 ms |

Renderer strings: `ANGLE (NVIDIA, Vulkan 1.4.351 (NVIDIA GeForce RTX 3070), NVIDIA)` and `ANGLE (NVIDIA Corporation,
NVIDIA GeForce RTX 3070/PCIe/SSE2, OpenGL ES 3.2)`. The unthrottled fps omits `gl.finish()` per frame, so treat it as
a rough measure of headroom and not as a precise number. Latency runs from the pose file's mtime to the end of the
next drawn frame. Most of it is the server's 50 ms mtime poll plus up to one 16 ms frame. Pose writes are themselves
capped at 10 Hz by the body (`BODY_VIEW_POSE_HZ`).

Compared with the CPU path at 720p, this is 60 fps (vsync-bound) against about 1.4 fps.

The orientation was checked against the Node PNG for the same frozen pose at 640x360, front view and top-down: the
same landmarks appear on the same side and the horizon sits at the same height.

### Pose interpolation (`tools/view/web/interp.mjs`)

Without interpolation the camera snaps to each pose. Poses come at the body's write rate and arrive 0–50 ms late
because of the server's mtime poll, so the camera sits still most frames and then jumps. The page now plays poses back
a short delay behind the body's clock and blends between the two poses around that moment. `?interp=0` turns this off,
and the overlay shows `interp <delay> ms`.

How it works:
- **Clock mapping.** Poses are keyed by their own `t` (the body's clock), so poll jitter does not move them.
  offset = a running minimum of (arrival − t). It rises by 2 ms/s so it can follow clock skew, and it falls toward a
  new minimum at no more than 20 ms/s. A late arrival never moves the playhead, and the playhead never steps.
- **Delay.** target = interval + p90 of the arrival jitter (skew − offset) over the last 20 arrivals + 10 ms, clamped to
  50–200 ms. The interval is the 25th percentile of the last 12 gaps that are ≤ 500 ms, so heartbeats and pauses don't
  count. The delay rises at ≤ 20 %/s, falls at ≤ 100 %/s, and has a deadband. It settles at about 150 ms at 10 Hz and
  about 100 ms at 20 Hz.
- **Blending.**
  - eye and pos are interpolated linearly, yaw along the shortest arc, pitch linearly. All other fields come from the
    newer pose.
  - Entities are blended by id. An entity only in the newer pose appears at its new position; one only in the older
    pose vanishes.
- **Special cases.**
  - A jump of more than 8 blocks (a teleport or respawn) snaps instead of blending. A body moved by smaller `tp` hops
    glides each hop over one pose interval.
  - When a gap is longer than 2 intervals (a standstill before a write), the camera holds the older pose until one
    interval before the newer one.
  - Underrun: if no newer pose has arrived, the camera holds the newest pose. It does not extrapolate, because the pose
    velocity is mineflayer's per-tick value and correcting an extrapolation causes rubber-banding. An offline pose
    freezes the camera.

Measured with `tools/view-pose-replay.mjs --synthetic walk`: a body walking a straight line at 4.317 blocks/s, back and
forth over 20 blocks, with exact `t` and real timer jitter on the writes. The run served from a temp state directory
and used `tools/view-web-bench.mjs --trace 30` at 1280x720 with vsync, on the same input for both modes. Columns:
- **cv**: the coefficient of variation of the camera speed per frame, over the frames where the camera moves.
- **still frames**: frames with no camera motion while the body walks.
- **shown latency**: from the pose file's mtime to the first frame whose playback time has reached that pose.

| Pose rate | interp | cv | max/median speed | still frames (of 1800) | underruns | file→frame p50/p95 | shown latency p50/p95 |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 10 Hz | off | (0.010 over only ~300 moving frames) | 1.03 | 1495 | – | 36 / 54 ms | 36 / 54 ms |
| 10 Hz | on | 0.054 | 1.32 | 0 | 0 | 35 / 52 ms | 153 / 169 ms |
| 20 Hz | off | 0.093 | 1.97 | 1202 | – | 34 / 51 ms | 34 / 51 ms |
| 20 Hz | on | 0.069 | 1.59 | 0 | 0 | 32 / 50 ms | 99 / 117 ms |

Interpolation adds about 115 ms of shown latency at 10 Hz and about 65 ms at 20 Hz. The remaining cv comes from the
turnaround every 4.6 s and from the 50 ms poll, which merges two 20 Hz writes into one. A recorded ProbeMove go-to walk
(`tools/view-pose-record.mjs`, then replay) shows the same pattern: still frames drop from 973 to 15, and shown latency
goes from 40 to 159 ms.

#### Push: `fs.watch` instead of the 50 ms poll

The server now pushes pose and hud changes from `fs.watch` on the agent's view directory. It watches the directory
because the body replaces `pose.json` by rename. A 250 ms poll stays on as a fallback, and the watcher closes on
disconnect. `--push poll --poll-ms N` restores polling.

Measured on the synthetic 20 Hz replay at 1280x720, 30 s per run:

| Mode | interp | file→frame p50/p95 | shown latency p50/p95 | delay | underruns | server CPU |
| --- | --- | --- | --- | --- | --- | --- |
| watch (default) | on | 8 / 9 ms | 91 / 92 ms | 80 ms | 0 | 7.7 % |
| watch (default) | off | 7 / 8 ms | 7 / 8 ms | – | – | 3.3 % |
| poll 16 ms | on | 22 / 25 ms | 105 / 108 ms | 85 ms | 0 | 9.2 % |
| poll 16 ms | off | 20 / 21 ms | 20 / 21 ms | – | – | 4.0 % |
| poll 50 ms (old) | on | 35 / 52 ms | 102 / 118 ms | 90 ms | 0 | 7.7 % |
| poll 50 ms (old) | off | 34 / 51 ms | 34 / 51 ms | – | – | 3.6 % |

Server CPU covers the whole run, including the page's initial column load. With `watch`, the file→frame latency is
about one frame, and interpolation adds only its delay (about 80 ms at 20 Hz). Between runs the interp=1 speed cv
varied from 0.06 to 0.17 and did not follow the push mode. Each run covers only about six turnarounds, so the cv
depends on where they fall.

### Column loading: decode Worker, cheap slot clears

Columns are fetched on the main thread. The bytes are then transferred to a small pool of decode Workers
(`tools/view/web/decoder.mjs` and `decode-worker.mjs`, 1–3 workers, at most 8 fetches in flight). The workers run
`decodeColumn` (`column-work.mjs`: inflate, palette decode, state→material and light) and return the typed arrays as
transferables. Jobs are dispatched nearest-to-the-eye first, with the priority evaluated at dispatch time, and a
retarget cancels jobs that are no longer wanted. Uploads are capped at 4 ms of upload time per frame, with at least one
upload per frame. `?worker=0` (or a browser with no Workers) runs the same `decodeColumn` on the main thread, one
column per macrotask.

A retarget (a teleport or walking into a new chunk) used to zero every changed slot in the 3D textures in one frame.
A full re-window is about 57 MB of `texSubImage3D` and took 79 ms (p50, measured). Now `clearSlot` zeroes only the
slot's coarse section flags (0 = no column, 1 = blocks, 2 = all air), and the shader treats an unflagged section as
air with open-sky light. Its stale blocks are never read, so a retarget now takes 2.4 ms (p50, max 11.7 ms).

Measured with `view-pose-replay.mjs --synthetic teleport|sprint` and `view-web-bench.mjs --trace 30`, 1280x720,
interp on. The teleport replay jumps between two places ≥ 1000 blocks apart every 6 s; the sprint replay moves at
5.6 blocks/s across column borders. "before" is the page before this change.

| Run | max frame | frames > 25 ms | long tasks (n, total) | sync decode on main thread |
| --- | --- | --- | --- | --- |
| teleport, before | 66.7 ms | 10 | 4, 235 ms | 3088 ms over 1450 columns |
| teleport, Worker | 16.8 ms | 0 | 0 | 0 |
| teleport, `?worker=0` | 16.8 ms | 0 | 0 | 2988 ms over 1389 columns |
| sprint, before | 16.8 ms | 0 | 0 | 421 ms |
| sprint, Worker | 16.8 ms | 0 | 0 | 0 |
| sprint, `?worker=0` | 16.8 ms | 0 | 0 | 450 ms |

The fix that mattered was the cheap slot clear. On this machine the Worker has no measurable effect on dropped frames or
long tasks: the synchronous part of a decode is about 2.2 ms per column, which fits inside a frame. The old `decodeMs`
of about 9 ms included the awaited inflate's wall time, so it overstated the cost. The Worker is kept because it
takes about 3 s of main-thread work off each teleport burst. That matters on a slower CPU, and the main-thread
fallback is the same code.

Column file change → drawn (60 s sprint replay, one touched column every second): with only the 250 ms column poll it
was p50 136 / p95 267 ms. The server now also watches the world's chunks directory with `fs.watch`, keeping the poll as
the fallback, and the latency is p50 14 / p95 17 ms (n = 60, no dropped frames).

A slot that is reused keeps its old voxels and light behind its flags. `node tools/view-web-check.mjs --ghost` checks
that none of that stale data is ever drawn. It uses a fixture (`tools/view/ghost-fixture.mjs`) with two places A and B
that share all 9 slots at radius 1, and B has 4 columns missing on disk. One page teleports A → B → A, and each view is
compared pixel by pixel with a fresh page load at the same pose, including a view across the missing-column border.
The result is identical: mean abs diff 0.000, 0 % of pixels over 8. With `clearSlot` made a no-op, the check fails:
mean diff 10.3, 8.3 % of pixels.

The fps table above was measured with the flat-colour shader; the cost of textures and lighting is in the next
section.

### Textures, lighting and partial blocks: cost

Measured on a frozen copy of a test scene (a row of blocks, slabs, snow layers 1–8, plants, a tree, a stone-brick
room with a torch and a glowstone; 289 columns, Vulkan, `fov=90`, unthrottled, 4 s per run). The flat-colour page is
the one from commit 03e2b61 served over the same state:

| Page | SceneDay 1280x720 | SceneDay 1920x1080 | SceneRoom 1280x720 | SceneRoom 1920x1080 |
| --- | --- | --- | --- | --- |
| flat colours (before) | 2289 fps | 1051 fps | 7404 fps | 4195 fps |
| + textures | 2056 | 957 | 7846 | 3799 |
| + lighting, smooth light, AO | 1387 | 598 | 3098 | 1409 |
| + partial boxes (now) | 1528 | 695 | 3493 | 1577 |

With vsync it stays at 60 fps everywhere; the worst case above (1920x1080 outdoors, 695 fps) is 1.4 ms of GPU time
a frame. The run-to-run spread of these numbers is about ±10 %. Live, with `fs.watch` push, file→frame latency was
9 / 18 ms (p50/p95) at 1280x720 and 12 / 17 ms at 1920x1080 on a standing body.

Per column, on the main thread (289-column load, `window.__view.decodeMs/lightMs/uploadMs`): inflate + block decode
p50 8.7 ms / p95 12.4 ms (unchanged), light decode and reorder p50 0.5 / p95 0.7 ms, the three `texSubImage3D` calls
p50 0.1 / p95 0.2 ms of CPU (98 KB more per column for light; the GPU copy is asynchronous). Texture array upload:
once per page, 1.1 MB. So the new work per column walked into is about 0.6 ms on top of the existing 9 ms decode.

Regression check: `node tools/view-web-check.mjs [--lighting] [--out dir]` writes a synthetic world
(`tools/view/fixture.mjs`: a wall with lit, dark, leaf-with-red-wool-behind and diamond stripes, a torch-lit floor
patch, a bottom slab and a snow layer with blocks behind them), serves it, renders it in headless Chromium on the GPU
and asserts pixel statistics per region: textured (std), the diamond face colour against its texture's average, leaf
holes showing red, the dark stripe below 0.35 of the lit one, night below 0.5 of day, the torch patch warm, the space
above the slab and the snow layer showing what is behind them. It exits 1 on a failure.

### Known gaps
- Animated textures (water, lava, fire, portals) show their first frame. Water is a full cube (a source's surface is
  at 14/16), with no flow texture.
- Stairs are full cubes; connected fences, walls and panes are their bounding box; torches, signs, levers and
  buttons are crosses; chests use a plank texture. Doors and trapdoors use their bounding box with the door texture
  on every face.
- Smooth lighting treats only full opaque cubes as occluders and does not light box sides from inside the box.
- Column change notices come from a 250 ms poll of the chunk files around the eye (see "Column loading").

## 3. Folding it into the dashboard (`dashboard/`, ClojureScript)

The page is plain ES modules and has no build step, so the dashboard can host it in either of two ways:
1. **iframe** (smallest step): proxy or link `http://<host>:3702/?agent=<Name>&w=..&h=..` from the agent panel.
   Nothing in `dashboard/` needs to know about WebGL.
2. **Native**: import `tools/view/web/{decode,camera,gl}.mjs` as ES modules through shadow-cljs (`:js-options` /
   `["/view/gl.mjs" :as gl]`). Mount a `<canvas>` in a Reagent/UIx component with a ref, and drive `app.mjs`'s logic
   from the component lifecycle. That means splitting `app.mjs` into a `createView(canvas, {agent, radius})` that
   returns `stop()`, with the DOM overlay replaced by dashboard components that read `window.__view`-style stats.

Either way, still to do:
- Serve `/pose`, `/columns`, `/blocks` and `/hud` from the dashboard's server, or proxy to `view-serve`.
- Choose the agent from the dashboard's agent list.
- Close the EventSource when the panel unmounts.
- Make the radius and resolution settings per panel.

## Blocks the view draws wrong

The page draws every block state from one row of the material table (`tools/view/materials.mjs`): a kind (cube, box,
cross, water, lava), a texture per face, a box. That table is a guess from collision shapes and texture names, not the game's
visual model, so some blocks come out wrong. A durable list of them lives in `state/worlds/<world>/view-block-issues.json`,
so the owner can fix them later. Nothing in it is fixed automatically.

**How it is made.** `tools/view/block-issues.mjs` (pure) compares each block's material with its real model, read from the
client jar (`tools/view/block-models.mjs`: `blockstates/<name>.json` and `models/block/*.json`, parents followed; the jar is
`$MC_CLIENT_JAR`, else the newest release the launcher installed, as for `tools/textures.mjs`). One record per block name and
reason: `{ name, reason, severity, drawnAs, drawnVariants?, detail, model?, parents?, ignored?, example: { stateId, props }, states, statesTotal, seen, firstSeen, positions? }`. `states` of `statesTotal` is how many states the record is true for; `drawnAs` is the first affected state's picture and `drawnVariants` counts them all when there are several; `model` and `parents` are the model id and its parent chain; `ignored` lists the ignored properties. Severity is `missing` (the block vanishes or is a hash colour), `wrong` (reads as a different thing) or `approximate` (right place and colour, simplified shape); each reason has a default and a record can be milder (upright planes drawn as a cross, an untinted overlay, a lectern's book). The route and the file sort by severity, then seen, then name. The reasons:

| reason | meaning |
| --- | --- |
| `unknown-state` | state ids in a column that are past the table or in no block; the page draws them as air. One record per contiguous id range (name `state:<lo>-<hi>`) with up to 5 positions in `positions`. The dump stores ids (prismarine's section palette), not names |
| `no-texture` | a face found no texture file, so it is a flat colour (hash of the name) |
| `shape-mismatch` | drawn as cross, cube or box, but the model is something else (leaf litter, pink petals and wildflowers are flat multi-part models; torches, signs and levers are crosses) |
| `shape-approximated` | a box (or cube) for a model with several elements (stairs, fences, panes, cauldrons, the grass block's overlay) |
| `tint-missing` | model faces carry a `tintindex` but `tintOf` (src/vision/renderer.mjs) has no tint for the texture, so a grey colormap texture stays grey |
| `state-ignored` | the blockstate depends on properties (facing, half, segment_amount, ...) that give every value the same material |
| `block-entity` | the model has no elements (particle-only or `builtin/entity`): chests, beds, signs, banners, heads, shulker boxes, decorated pots, conduit, bell. Lectern, enchanting table and campfires have geometry and are listed by name, as `approximate` |
| `no-model-data` | no blockstate in the jar (block newer than the jar), or no jar at all (one record named `*` says so; only `no-texture` is then reported) |

`cross` means the model's parent chain reaches `block/cross`, `block/tinted_cross` or `block/flower_pot_cross`; `cube` means one
unrotated 0..16 element; anything else is complex. Not handled: crop-style models (wheat, carrots: four planes) count as complex,
so they are `shape-mismatch` although a cross is a fair picture; element rotation, `display`, models built in code
(`builtin/entity`), and a property that matters only for some values. A property counts as honoured when two states differing
only in it get different materials, so a property that changes the texture but not the table is a false `state-ignored`.

**Seen in the world.** `tools/view/block-scan.mjs` counts, per column file, how many blocks of each flagged name occur and where
the first one is (`firstSeen: { world, x, y, z, agent }`, the agent being the column header's `body`). It runs in a worker
thread (`block-scan-worker.mjs`), started the first time a world is requested through the route or streamed through `/pose`;
it scans every `chunks/*.bin` once, then rescans the columns whose mtime changed (a directory watch, with a 5 s mtime sweep as
the fallback). Each column keeps its own contribution, so a rewritten column replaces its counts. State ids at or above the
table's size count under `state:<id>`. Columns dumped with a different `mcVersion` from the world's first column are decoded
with the first one's format, which is wrong; mixed-version worlds are not handled.

**Files and routes.** The file is `{ version, mcVersion, jar, updatedAt, scan, records }`, `jar` being the jar's file name or
null, written atomically (tmp + rename) at most every 5 s and only when the records changed. It is bounded by construction:
one record per name and reason. `GET /block-issues/<world>` returns the same JSON, records sorted by seen (descending) then
name; the first call for a world waits for the initial scan (unless the file is already there), and the world must have a
`chunks/` directory.

**`?debug=1` / `?debug=2`.** `http://host:port/?agent=<Name>&debug=1` asks for `/blocks/<version>.json?debug=1`, whose materials carry
`issue` (the worst severity of their block: missing, wrong or approximate). The renderer draws materials whose issue is
missing or wrong as a magenta/black checker (squares two texels wide); `debug=2` also draws the approximate ones. Without
the parameter nothing changes.

`tint-missing` fires only for blocks on a hard-coded list of the game's colour-provider registrations (`TINTED_BLOCKS` in
block-issues.mjs), written from memory of BlockColors and possibly incomplete for 26.1: a `tintindex` in a model does not mean
the game tints the block (cherry leaves, bamboo). `bubble_column` is reported as a wrong shape (the game draws water);
`moving_piston` is never reported.

**What this cannot detect**, and how one would find it:
- *Cutout or alpha drawn black.* A texture whose transparent pixels render black or opaque shows no sign in the model data.
  Render each block state alone in a fixture grid (`tools/view/fixture.mjs`) and compare against a reference screenshot of the
  game, or flag textures with alpha between 0 and 255 whose material has no CUTOUT or TRANSLUCENT flag.
- *Plausible but wrong colours.* Biome tints are one fixed colour each (temperate); the tint table has no colormaps, so a
  block with a tint entry is never flagged although swamp, desert and ocean differ. Compare against screenshots in several
  biomes, or read `tintindex` users in `models/block` and check each against the colormap it should use.
- *Animated textures.* Water, lava, fire, portals, sea lanterns, magma and prismarine show their first frame. List every
  texture with a `.png.mcmeta` beside it in the jar.
- *Block entities the game draws with entity renderers.* Chests, signs, beds, banners, heads, shulker boxes, decorated pots,
  bells, conduits: their blockstate model is `builtin/entity` or only a particle, so most are `no-texture`, `shape-mismatch` or a
  plank stand-in. List blocks whose resolved model has no elements, or that have a block entity in the registry, and screenshot them.
- *Emissive and glow differences.* Only the registry's `emitLight` and `lit` set the EMISSIVE flag; blocks that glow in game
  without light emission (glow lichen's face, ochre froglight) are not compared. Diff the light levels against the game's.
- *Blocks newer than the jar.* A jar older than the server's version has no blockstate for them (`no-model-data`), and a block
  the jar knows but the registry does not never appears. Run the classifier with the jar of the server's version.
- *Model rules the classifier does not handle* (above). To find them, list the blocks that have a blockstate and a model but
  where the rules gave no verdict they could defend: models with `rotation` on an element, parents outside `block/*`, `display`
  only models; or render every state in a fixture grid and diff against the game, which is the only complete check.

## Block models from the game's own assets

The view draws blocks from the client jar's blockstates and models. The jar is the newest release found in the
launcher's versions dirs or in `~/.cache/minecraft-agents/client/`, and `$MC_CLIENT_JAR` takes precedence; here it
is 26.1.2. The bake runs once per version on the server, in `tools/view/block-bake.mjs` and `materials.mjs`:
- **States:** each state resolves its variant or multipart parts and its model parents, with the x/y rotation and
  uvlock folded in.
- **Cubes:** a state that is one full cube stays on the fast path, with six face layers. That puts the dispenser front
  on the facing side and fixes furnaces, observers and the like.
- **Other models:** every other state is kind `model`, with an offset into the element table
  (`GET /elements/<ver>.bin`, RGBA32F, layout in `tools/view/element-table.mjs`; 8,519 distinct elements, 2.2 MB).
  The shader loops over at most 24 elements per model voxel. It applies the element rotation with rescale, the vanilla
  uv and face rotation, an alpha test at 0.5, and light from the neighbour cell for faces on the voxel boundary.
- **Tints:** each face carries a tint group: grass, foliage, dry foliage, water, or a constant from minecraft-data
  `tints.json`. The colour comes from one shader function, `tintFor()`. For grass, foliage, dry foliage and water it
  reads the biome of the hit's 4x4x4 cell from a per-world R8UI 3D texture (n*4 x height/4 x n*4: 0.44 MB at radius 8,
  75 KB for a radius-3 preview) and that biome's colour from a 4 x 256 table (`GET /biomes/<world>.json`, built by
  `tools/view/biome-colors.mjs` from the client jar's colormaps and biome JSON, by the names in the world's
  `biomes.json`, i.e. the server's own registry). Approximations: hard edges at biome borders (the game blends over a
  5x5 neighbourhood; a 3x3 blend measured +0.39 ms per frame at 1920x1080 outdoors and is not shipped), swamp grass is
  its cold colour #6A7039 (the game picks one of two by noise). Without a usable `biomes.json` (missing, or more than
  255 biomes) the page keeps the fixed group colours and the server notes it once in the block-issues log; ids are
  never mapped through minecraft-data's order (this server has `sulfur_caves`, which shifts every id from 53 up).
  Cost: no measurable frame or decode cost (1920x1080 frozen scene, 3 runs each: 1.46 ms per frame with biomes vs
  1.61 ms without, within the run-to-run spread; decodeSections 1.31 vs 1.30 ms per column).
- **No jar:** the legacy table is used unchanged.

Effect on the log (world claude, 26.1 server, 26.1.2 jar):

| | missing | wrong | approximate |
| --- | --- | --- | --- |
| before | 29 | 283 | 645 |
| after | 0 | 150 (all `block-entity`) | 63 (18 `tint-approximate`, 41 `element-outside-voxel`, 4 partial block entities) |

The `element-outside-voxel` blocks have parts outside their own cell, and those parts are clipped. The visible losses
are fire flames above the block, sculk sensor tendrils, campfire smoke planes, the pink_petals/wildflowers stems
reaching into the next cell, and the repeater/comparator/piston knobs.

Checks: `node tools/view-web-check.mjs --models` covers leaf litter flat and tinted, a stair notch, fence gaps, a
dispenser front, and the grass side overlay. Each check fails on the no-jar page. A world of only full cubes renders
the same with and without the jar (mean abs diff 0.024, 0 % of pixels over 8). Logs lying along x or z are left out
of that comparison on purpose: the old renderer mirrored them, and the jar draws them as vanilla does.

Cost at 1920x1080, unthrottled, RTX 3070:

| View | Before | After |
| --- | --- | --- |
| Grazing worst case (tall grass, bamboo, kelp, fence wall, low angle) | 0.80 ms | 1.11 ms |
| Real leaf-litter view | 0.94 ms | 1.34 ms |

Both views stay far above 60 fps.

The software thumbnail renderer (`src/vision/renderer.mjs`, the Node PNG path, and dashboard thumbnails that use it)
keeps the old look. Block models are browser-only, and that difference is expected, not a bug.
