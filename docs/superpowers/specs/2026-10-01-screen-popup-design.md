# The look popup as the game's screen, and a faster live stream

Dan, 2026-10-01. The dashboard's look popup already streams the body's view and draws its inventory as the game's
inventory screen. This adds the rest of what a player sees, and makes the stream faster.

## Measured

On the live body at the farm, a 320x180 live frame costs ~250 ms of the render worker (49 ms on a flat synthetic
scene). The stream therefore runs at 3-4 fps with the worker pinned at 100% of a core while the popup is open; the
body's main thread is 93% idle, so the body itself is unhurt. Worker profile: the ray march ~70%, `rayBox` against every
entity for every pixel 17%, per-pixel allocation and GC ~8%. Farmland (`boxes`) and crops (`cross`) make the farm the
worst case.

## 1. Faster frames (`src/vision/renderer.mjs`, `src/vision/eyes.mjs`)

- Entities: each entity's box is projected to a screen rectangle once a frame; a pixel tests only the boxes whose
  rectangle contains it. Entities behind the eye are dropped.
- No allocation per pixel in `render`: sky, colour and direction are scalars; `castRay` returns a hit only when it has
  one; a `boxes` block's intersections and a `cross` block's are computed without intermediate arrays.
- Same picture, byte for byte, as before: the vision tests pin that.
- Unchanged scene, no render: `eyes.mjs` remembers the last frame's key (eye cell and sub-block position rounded to
  1/16, yaw and pitch rounded to 0.5 degrees, time of day to the minute, every visible entity's rounded position, the
  grid's edit counter which `blockUpdate` bumps, size, panorama, fov, maxDist) and the answer. A look with the same key
  answers the same PNG and `seen` without the worker. The file is still written (the dashboard reads it).

Target: ~8-10 fps on the farm scene for the same CPU. Further ideas from the optimisation review may be folded in when
they keep the picture and are small.

## 2. `screen`: one quick action for what the player's screen shows

`inventory slots=true` (dashboard-only) becomes the `screen` action (`src/bot.mjs`, `src/lib/help.mjs`, `src/job-policy.mjs`
concurrent reads), answering:

```
{ hp, food, xp, oxygen, armor: <points, from the player's armor attribute>, selected,
  slots: [{slot, name, count}],                        // 5-45 as inventorySlots gives them
  window: null | { type, title, at, open, closedAt, slots: [{slot, name, count}] } }
```

`inventory` loses the `slots` argument. Armour points: the sum of the player's `armor` attribute (any key ending in
`armor`, not toughness) value plus its additive modifiers; 0 when the server has not sent it.

`window` is kept by `src/body/window-watch.mjs`: on `windowOpen` it records the window's type (e.g.
`minecraft:generic_9x3`, `minecraft:furnace`, `minecraft:hopper`), title, the container slots (0 .. inventoryStart-1)
and `at` (the block position the body opened, which `containerAt`/the furnace helpers pass when they open; null when
unknown); `updateSlot` on the window keeps the slots true; `windowClose` marks it closed with the time. `screen` reports
the window while open and for `LINGER_MS = 5000` after it closed (withdraw, deposit and smelt open and close a chest
inside one call, often under a second, so a 1 s poll would never see one otherwise), then null.

## 3. The popup (`tools/dashboard/index.html`, `tools/dashboard.mjs`)

`/api/inventory/<Name>` becomes `/api/screen/<Name>`, the body's `screen` answer verbatim; the popup's 1 s loop asks
it. Under the picture, in this order, as the game stacks them:

- HUD strip: ten hearts (hp/2, half hearts), ten drumsticks (food/2, halves), armour points as chestplate icons
  (armor/2, halves) only when armor > 0, air bubbles (oxygen/2) only when oxygen < 20, and the XP level as a number.
  Drawn with inline SVG/CSS shapes in the game's colours, no new image assets.
- Container screen, only while `window` is reported: the game's layout by `type` — `generic_9xN` N rows of 9,
  `hopper` 5 in a row, `furnace`/`blast_furnace`/`smoker` input above fuel with the output to the right, `generic_3x3`
  (dispenser/dropper) 3x3, `shulker_box` 9x3, `crafting` 3x3 and a result; any other type rows of 9. Captioned
  "<title> at x,y,z" while open, "<title> at x,y,z · closed Ns ago" after. Same `.slot` cells and icons as the inventory.
- The inventory screen as today.

Each part is redrawn only when its HTML changed (as the inventory is today).

## Tests

- `test/vision.test.mjs`: the render is byte-identical before and after; entity rectangles; the unchanged-scene cache.
- `test/window-watch.test.mjs`: open, slot update, close, linger, expiry, `at`.
- `test/dashboard-look.test.mjs`: the HUD at sample values, each container layout, the caption, the one-second loop
  unchanged.
- `test/dashboard.test.mjs`: `/api/screen` route.
