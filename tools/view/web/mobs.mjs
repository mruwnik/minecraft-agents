// Why JavaScript: browser-safe table and screen maths shared by the WebGL view and the Node software renderers (tools/view/renderer.mjs,
// raycaster.mjs), which must run without the gitignored cljs viewer bundle; imports nothing.
// What a mob looks like in a view: a colour of its own per species, and the name label drawn over it.
const HOSTILE = [225, 35, 35]
const hashColor = name => {
  let h = 0
  for (const c of name) h = (h * 31 + c.charCodeAt(0)) >>> 0
  return [90 + h % 130, 90 + (h >> 8) % 130, 90 + (h >> 16) % 130]
}
// [body, head, limbs, face] in the game's colours; the face is the front of the head, so it shows which way a mob looks
const plain = (c, face = c.map(v => v * 0.55)) => [c, c, c, face]
const PALETTES = {
  player: [[235, 60, 235], [215, 160, 125], [235, 60, 235], [120, 80, 60]],
  zombie: [[40, 150, 155], [95, 150, 80], [65, 60, 160], [40, 70, 40]],
  zombie_villager: [[110, 80, 60], [95, 150, 80], [90, 65, 50], [40, 70, 40]],
  husk: [[150, 125, 85], [185, 160, 110], [110, 90, 65], [90, 75, 50]],
  drowned: [[60, 140, 130], [85, 165, 150], [70, 110, 140], [35, 80, 75]],
  skeleton: [[205, 205, 195], [215, 215, 205], [190, 190, 180], [70, 70, 70]],
  stray: [[165, 185, 190], [205, 210, 210], [150, 170, 175], [70, 80, 85]],
  bogged: [[150, 165, 120], [190, 195, 170], [130, 140, 105], [60, 70, 50]],
  wither_skeleton: [[45, 45, 45], [55, 55, 55], [35, 35, 35], [20, 20, 20]],
  creeper: [[85, 175, 65], [95, 185, 75], [70, 150, 55], [25, 35, 25]],
  spider: [[105, 45, 40], [115, 55, 50], [85, 35, 30], [150, 25, 25]],
  cave_spider: [[25, 60, 70], [30, 70, 80], [20, 45, 55], [150, 25, 25]],
  enderman: [[25, 20, 30], [30, 25, 35], [20, 15, 25], [200, 90, 235]],
  witch: [[80, 45, 100], [145, 170, 105], [60, 35, 75], [70, 90, 50]],
  pillager: [[85, 90, 95], [150, 155, 145], [55, 55, 60], [80, 85, 80]],
  vindicator: [[40, 90, 80], [150, 155, 145], [45, 50, 50], [80, 85, 80]],
  evoker: [[40, 40, 45], [150, 155, 145], [130, 110, 50], [80, 85, 80]],
  piglin: [[200, 150, 90], [230, 160, 140], [110, 75, 50], [160, 100, 90]],
  zombified_piglin: [[225, 150, 140], [225, 150, 140], [110, 140, 80], [120, 80, 75]],
  blaze: plain([250, 190, 40], [120, 70, 20]),
  slime: plain([110, 190, 90], [40, 80, 35]),
  cow: [[60, 45, 40], [60, 45, 40], [225, 220, 210], [230, 225, 215]],
  mooshroom: [[170, 30, 30], [170, 30, 30], [225, 220, 210], [230, 225, 215]],
  pig: [[240, 160, 165], [240, 160, 165], [225, 140, 145], [250, 190, 190]],
  sheep: [[235, 235, 230], [215, 185, 160], [215, 185, 160], [150, 120, 100]],
  chicken: [[250, 250, 250], [250, 250, 250], [235, 165, 50], [235, 165, 50]],
  horse: [[150, 110, 70], [150, 110, 70], [120, 85, 55], [60, 45, 30]],
  wolf: [[215, 210, 210], [215, 210, 210], [200, 195, 195], [60, 55, 55]],
  cat: [[200, 150, 80], [200, 150, 80], [180, 130, 70], [90, 70, 40]],
  fox: [[225, 120, 45], [225, 120, 45], [60, 40, 30], [240, 235, 225]],
  villager: [[120, 85, 60], [200, 150, 120], [100, 70, 50], [150, 105, 85]],
  wandering_trader: [[50, 80, 150], [200, 150, 120], [40, 60, 115], [150, 105, 85]],
  iron_golem: [[150, 170, 150], [215, 210, 200], [185, 180, 170], [110, 90, 70]],
  item: plain([255, 225, 40], [255, 225, 40]),

  // species the renderers had no colours for (they fell back to a hash colour or the family's grey)
  bat: plain([95, 70, 55], [30, 20, 15]),
  rabbit: [[160, 130, 95], [175, 145, 110], [135, 105, 75], [235, 225, 215]],
  squid: plain([45, 70, 110], [20, 30, 50]),
  glow_squid: plain([60, 190, 190], [20, 80, 80]),
  cod: plain([190, 150, 105], [90, 70, 45]),
  salmon: plain([170, 60, 50], [80, 30, 25]),
  tropical_fish: plain([255, 140, 40], [120, 60, 15]),
  pufferfish: plain([225, 205, 60], [100, 90, 25]),
  dolphin: plain([110, 125, 150], [50, 55, 70]),
  bee: [[235, 190, 50], [235, 190, 50], [50, 40, 30], [30, 25, 20]],
  ghast: plain([245, 245, 245], [140, 140, 140]),
  magma_cube: plain([130, 45, 20], [250, 160, 40]),
  silverfish: plain([135, 135, 145], [60, 60, 65]),
  endermite: plain([75, 40, 100], [35, 20, 50]),
  phantom: plain([70, 90, 160], [40, 230, 120]),
  vex: plain([150, 175, 215], [60, 70, 100]),
  guardian: plain([90, 160, 145], [240, 140, 60]),
  elder_guardian: plain([170, 150, 125], [240, 140, 60]),
  shulker: plain([150, 105, 150], [80, 50, 80]),
  strider: plain([160, 40, 45], [90, 25, 30]),
  frog: plain([200, 120, 60], [90, 50, 25]),
  axolotl: plain([240, 175, 200], [120, 80, 100]),
  allay: plain([90, 190, 245], [40, 80, 110]),
  llama: [[225, 205, 165], [225, 205, 165], [205, 185, 145], [100, 85, 65]],
  trader_llama: [[60, 105, 170], [225, 205, 165], [205, 185, 145], [100, 85, 65]],
  donkey: [[130, 125, 120], [130, 125, 120], [105, 100, 95], [60, 55, 50]],
  mule: [[110, 80, 60], [110, 80, 60], [90, 65, 50], [55, 40, 30]],
  skeleton_horse: [[215, 225, 210], [215, 225, 210], [165, 180, 165], [55, 65, 55]],
  zombie_horse: [[110, 150, 85], [110, 150, 85], [90, 125, 70], [45, 65, 35]],
  goat: [[225, 220, 205], [205, 185, 150], [195, 190, 175], [90, 85, 75]],
  polar_bear: [[245, 245, 240], [245, 245, 240], [220, 220, 215], [50, 50, 50]],
  panda: [[240, 240, 240], [240, 240, 240], [35, 35, 35], [30, 30, 30]],
  ocelot: [[225, 190, 90], [225, 190, 90], [160, 125, 50], [80, 60, 25]],
  turtle: plain([80, 140, 60], [40, 70, 30]),
  camel: [[215, 175, 105], [215, 175, 105], [190, 150, 85], [90, 70, 40]],
  sniffer: plain([150, 55, 50], [70, 120, 70]),
  armadillo: plain([185, 130, 125], [90, 60, 55]),
  hoglin: [[185, 120, 100], [185, 120, 100], [150, 95, 80], [235, 225, 200]],
  zoglin: [[130, 135, 115], [130, 135, 115], [100, 105, 85], [225, 215, 190]],
  ravager: [[95, 90, 90], [95, 90, 90], [75, 70, 70], [215, 200, 160]],
  snow_golem: [[245, 245, 250], [235, 235, 240], [235, 235, 240], [235, 120, 30]],
  warden: [[20, 55, 65], [25, 70, 80], [15, 40, 50], [90, 230, 220]],
  creaking: [[100, 80, 70], [120, 95, 80], [80, 65, 55], [240, 130, 40]],
  piglin_brute: [[170, 90, 50], [225, 150, 130], [90, 55, 35], [140, 85, 75]],
  illusioner: [[50, 70, 140], [150, 155, 145], [40, 55, 105], [80, 85, 80]],
  parched: [[185, 165, 100], [205, 185, 120], [160, 140, 85], [80, 70, 45]],
  camel_husk: [[170, 145, 105], [170, 145, 105], [150, 125, 90], [80, 65, 40]],
  parrot: [[225, 50, 45], [225, 50, 45], [235, 195, 60], [235, 195, 60]],
  wither: [[40, 40, 50], [55, 55, 70], [30, 30, 40], [140, 150, 200]],
  ender_dragon: plain([35, 30, 45], [200, 60, 220]),
  breeze: plain([135, 210, 235], [60, 100, 120]),
}
// the species with a palette of their own (not the kinds 'player' as a fallback, nor the 'item' drop)
export const SPECIES = Object.keys(PALETTES).filter(name => name !== 'item')
export const hasPalette = name => name in PALETTES
// A dropped item takes the colour of the block it is (or the material it is made of); an item with no entry here gets a colour
// hashed from its name, and one the pose does not name (an older body) stays the plain item colour.
const ITEM_COLORS = [
  [/^(oak|dark_oak)_/, [150, 115, 65]], [/^spruce_/, [115, 85, 50]], [/^birch_/, [215, 205, 140]], [/^jungle_/, [160, 115, 80]],
  [/^acacia_/, [170, 90, 50]], [/^cherry_/, [230, 175, 175]], [/^mangrove_/, [120, 55, 50]], [/^bamboo_/, [200, 180, 80]],
  [/^(crimson)_/, [120, 50, 75]], [/^(warped)_/, [45, 110, 110]],
  [/^(white|light_gray)_/, [215, 215, 215]], [/^(gray|black)_/, [60, 60, 65]], [/^(red)_/, [170, 45, 40]], [/^(orange)_/, [225, 120, 35]],
  [/^(yellow)_/, [235, 200, 55]], [/^(lime|green)_/, [100, 160, 40]], [/^(cyan|light_blue|blue)_/, [60, 130, 200]],
  [/^(purple|magenta)_/, [150, 60, 170]], [/^(pink)_/, [235, 140, 170]], [/^brown_/, [110, 75, 45]],
  [/^(cobblestone|stone|andesite|diorite|granite|deepslate|cobbled_deepslate|gravel)$/, [125, 125, 125]], [/^(dirt|coarse_dirt|farmland|mud)$/, [120, 85, 55]],
  [/^(sand|sandstone)$/, [220, 205, 150]], [/^(grass_block|short_grass|oak_leaves)$/, [100, 160, 70]], [/^(coal|charcoal)$/, [40, 40, 40]],
  [/^(raw_)?iron(_ingot|_ore)?$/, [215, 180, 160]], [/^(raw_)?gold(_ingot|_ore)?$/, [240, 200, 50]], [/^(raw_)?copper(_ingot|_ore)?$/, [200, 110, 75]],
  [/^(diamond|diamond_ore)$/, [90, 220, 215]], [/^(emerald|emerald_ore)$/, [60, 200, 100]], [/^(redstone|redstone_ore)$/, [200, 30, 20]],
  [/^(lapis_lazuli|lapis_ore)$/, [40, 70, 170]], [/^(stick|bone_meal|string|feather|bone)$/, [225, 220, 200]], [/^(wheat|wheat_seeds|hay_block)$/, [210, 185, 70]],
  [/^(rotten_flesh)$/, [150, 95, 70]], [/^(snow|snowball|snow_block)$/, [245, 250, 250]], [/^(glass|ice)$/, [180, 215, 235]],
  [/^(apple|beef|porkchop|carrot|beetroot|sweet_berries)$/, [200, 60, 50]]
]
const itemColor = item => ITEM_COLORS.find(([re]) => re.test(item))?.[1] ?? hashColor(item)
const PLAIN_ITEM = 'item'
export const paletteFor = e => e.name === PLAIN_ITEM && e.item ? plain(itemColor(e.item)) : PALETTES[e.name] ?? PALETTES[e.kind] ?? plain(e.kind === 'hostile' ? HOSTILE : hashColor(e.name))
export const colorFor = e => paletteFor(e)[0]

// ---------------------------------------------------------------- labels
// What is written over a mob: a player's name as it is (the pose's username, as `label`), else the species with spaces for underscores.
export const labelFor = e => {
  const text = e.label ?? e.name ?? ''
  return e.kind === 'player' ? text : text.replaceAll('_', ' ')
}

const LABEL_BLOCKS = 0.32 // a label is this tall in the world, so it shrinks with distance ...
const LABEL_MIN_PX = 9 // ... down to a size that can still be read
const LABEL_MAX_PX = 26
const LABEL_RISE = 0.2 // blocks above the mob's head
export const MAX_LABELS = 16
// A body senses like a player: a mob is named only when it is lit enough to be seen (`seeing`, the lightmap's brightest channel,
// 0.2 is about block light 2) or within a couple of blocks (a player sees an adjacent mob in the dark).
export const SEEING_MIN = 0.2
export const SEE_NEAR = 2
export const canSee = (seeing, dist) => dist <= SEE_NEAR || seeing >= SEEING_MIN
// A label is a plate round 5x7 capitals with a one column gap between letters: its width for a height of `h` pixels. The software
// renderers draw those capitals (tools/view/labels.mjs); the WebGL view draws the text into an atlas and stretches it over the same plate.
export const LABEL_MARGIN = 1 // cells round the text
export const LABEL_CELLS_HIGH = 7 + 2 * LABEL_MARGIN
export const labelWidth = (text, h) => (text.length * 6 - 1 + 2 * LABEL_MARGIN) * h / LABEL_CELLS_HIGH

// project(p) for a camera basis (web/camera.mjs cameraBasis) in a width x height picture; p is relative to the eye.
// The same maths as the software renderer's cameraFor().project. null behind the eye.
export const projectorFor = ({ forward, right, up, half }, width, height) => p => {
  const f = p.x * forward.x + p.y * forward.y + p.z * forward.z
  if (f <= 1e-9) return null
  const sx = (p.x * right.x + p.y * right.y + p.z * right.z) / f
  const sy = (p.x * up.x + p.y * up.y + p.z * up.z) / f
  return { px: (sx / half + 1) / 2 * width - 0.5, py: (1 - sy / (half * height / width)) / 2 * height - 0.5 }
}

// Labels for the entities that are in front of the camera, nearest first, at most MAX_LABELS: {text, px, py, h, depth, dist, at}.
// (px, py) is the bottom centre of the label in pixels, h its height in pixels, at the mob's middle (where its light is read), depth the distance a label is hidden by terrain
// nearer than (the mob's own front, so its own box never hides it). Drops get none. Entities are world positions
// {x, y, z, height, width, name, label?, kind?}; the pixel testing against terrain is the caller's.
export const placeLabels = ({ eye, project, width, height, entities }) => {
  const focal = width / 2 // pixels per unit of tan(angle) for a 90 degree fov; scaled below by what project says
  const found = []
  for (const e of entities) {
    if (e.kind === 'item' || e.name === 'item') continue
    const text = labelFor(e)
    if (!text) continue
    const rx = e.x - eye.x
    const ry = e.y + (e.height ?? 1.8) + LABEL_RISE - eye.y
    const rz = e.z - eye.z
    const at = project({ x: rx, y: ry, z: rz })
    if (!at) continue
    const dist = Math.hypot(rx, e.y + (e.height ?? 1.8) / 2 - eye.y, rz)
    const h = Math.min(LABEL_MAX_PX, Math.max(LABEL_MIN_PX, LABEL_BLOCKS * focal / Math.max(dist, 0.5)))
    if (at.px < -width || at.px > 2 * width || at.py < -height || at.py > 2 * height) continue
    found.push({ text, px: at.px, py: at.py, h, depth: Math.max(0.1, dist - (e.width ?? 0.6)), dist, at: [e.x, e.y + (e.height ?? 1.8) / 2, e.z] })
  }
  return found.sort((a, b) => a.dist - b.dist).slice(0, MAX_LABELS)
}
