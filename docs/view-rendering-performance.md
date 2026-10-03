# View rendering: performance and the browser renderer

A body dumps what it sees (`docs/view-format.md`): chunk columns in `state/worlds/<world>/chunks/`, plus `pose.json`
and `hud.json` in `state/agents/<Name>/view/`. There are two renderers for those files.

## 1. Node PNG path (snapshots)

`node tools/view-render.mjs <Name> [--width --height --fov --dist --watch ms --bench s]` loads the columns with
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
same as the Node renderer), `dist` (blocks, default radius*16).

Server (`tools/view/serve.mjs`, no dependencies beyond the repo's own):
- `GET /agents`: the agents that have a pose file.
- `GET /pose/<Name>?radius=R`: SSE. It polls the files' mtimes every 50 ms and sends `pose` `{mtime, sentAt, pose}`,
  `hud` `{mtime, hud}`, and `column` `{cx, cz, mtime}` when a column file within R of the eye changes. Columns are
  polled every 250 ms.
- `GET /columns/<world>/<cx>.<cz>.bin`: the raw deflated file. `GET /hud/<Name>`.
- `GET /blocks/<mcVersion>.json`: the material table, ~226 KB for 26.1 (29873 states, 1269 materials, 0.3 s to build
  and then cached). It maps each state id to a material (`name|kind`, kind is cube/partial/cross/water/lava). Each
  material has a top, side and bottom colour, which are the alpha-weighted average of the face texture in `textures/`,
  tinted. The table also includes the column `format` the decoder needs (`tools/view/materials.mjs`,
  `tools/view/web-format.mjs`).

Page (`tools/view/web/`):
- `decode.mjs` inflates the file with `DecompressionStream`. It decodes the 1.18+ paletted containers (single,
  indirect and direct; 26.1 has no size prefix and has a fluid count) in the browser. Tests check it cell by cell
  against prismarine on synthetic columns (1.20.4, 1.21.4, 26.1) and on 3 real files.
- `camera.mjs` builds the basis of `cameraFor` in `src/vision/renderer.mjs`. It is tested against `directionFor` and
  the renderer's side convention.
- `gl.mjs` keeps a toroidal R16UI 3D texture of material ids for (2R+1)² columns at full height (272x384x272 at
  R=8), with one 16x384x16 `texSubImage3D` per column, and an R8UI section-occupancy texture. A WebGL2 fragment shader
  does a two-level DDA: it skips empty 16³ sections, then steps through blocks. It applies face shading
  (1/0.5/0.8/0.62), draws plants (cross) as an inner box and blends one water layer, then adds a sky gradient, fog and
  night dimming. Entities are drawn as boxes, up to 64.
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

### Known gaps
- 'partial' blocks (slabs, snow layers, stairs) are drawn as full cubes, which makes snow layers look like walls at eye
  level. The fix is to send per-state heights or boxes in the material table.
- The page uses flat average colours and no textures yet. Next step: a 16x16 `TEXTURE_2D_ARRAY` atlas with
  top/side/bottom layer indices per material.
- Columns are decoded on the main thread. A login burst of 289 columns stutters for a moment. Next step: a Worker.
- Pose updates are polled. `fs.watch` or pushing poses straight from the body would cut ~25 ms of the latency.

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
