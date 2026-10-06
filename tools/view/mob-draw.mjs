// Why JavaScript: graphics/performance; the mobs of the software renderers (renderer.mjs, raycaster.mjs).
// A mob as the software renderers draw it: its parts in its own frame, turned to its yaw. A mob with a textured model (web/mob-models.mjs)
// is drawn from that, painted from the jar's entity sheets (mob-textures.mjs); any other as a few flat-coloured boxes of its family.
import { paletteFor } from './web/mobs.mjs'
import { FACES, faceUV, modelFor, yawBasis } from './web/mob-models.mjs'

// A mob is drawn as a few boxes in its own frame: x across and z forward in widths, y up in heights, so one table
// serves a chicken and a ravager. The last number picks the palette entry: 0 body, 1 head, 2 limbs.
const FAMILIES = {
  biped: [[-0.42, 0.75, -0.42, 0.42, 1, 0.42, 1], [-0.42, 0.375, -0.21, 0.42, 0.75, 0.21, 0], [-0.83, 0.375, -0.21, -0.42, 0.75, 0.21, 0],
    [0.42, 0.375, -0.21, 0.83, 0.75, 0.21, 0], [-0.42, 0, -0.21, 0, 0.375, 0.21, 2], [0, 0, -0.21, 0.42, 0.375, 0.21, 2]],
  quadruped: [[-0.5, 0.4, -0.8, 0.5, 0.8, 0.55, 0], [-0.33, 0.55, 0.55, 0.33, 1, 0.95, 1], [-0.45, 0, -0.75, -0.15, 0.4, -0.45, 2],
    [0.15, 0, -0.75, 0.45, 0.4, -0.45, 2], [-0.45, 0, 0.2, -0.15, 0.4, 0.5, 2], [0.15, 0, 0.2, 0.45, 0.4, 0.5, 2]],
  creeper: [[-0.42, 0.7, -0.42, 0.42, 1, 0.42, 1], [-0.42, 0.25, -0.25, 0.42, 0.7, 0.25, 0], [-0.42, 0, 0.25, 0, 0.25, 0.6, 2],
    [0, 0, 0.25, 0.42, 0.25, 0.6, 2], [-0.42, 0, -0.6, 0, 0.25, -0.25, 2], [0, 0, -0.6, 0.42, 0.25, -0.25, 2]],
  spider: [[-0.3, 0.25, -0.55, 0.3, 0.8, 0, 0], [-0.2, 0.25, 0, 0.2, 0.65, 0.3, 1], ...[-0.25, -0.1, 0.05, 0.2].map(z => [-0.5, 0.05, z, 0.5, 0.4, z + 0.06, 2])],
  bird: [[-0.5, 0.3, -0.5, 0.5, 0.75, 0.4, 0], [-0.3, 0.6, 0.25, 0.3, 1, 0.65, 1], [-0.3, 0, -0.05, -0.1, 0.3, 0.1, 2], [0.1, 0, -0.05, 0.3, 0.3, 0.1, 2]],
  blob: [[-0.5, 0, -0.5, 0.5, 1, 0.5, 1]]
}
const FAMILY_OF = Object.fromEntries(Object.entries({
  biped: 'player zombie husk drowned skeleton stray bogged parched wither_skeleton villager wandering_trader pillager vindicator evoker illusioner witch piglin piglin_brute zombified_piglin zombie_villager enderman iron_golem snow_golem creaking warden',
  quadruped: 'cow mooshroom pig sheep goat horse donkey mule skeleton_horse zombie_horse llama trader_llama camel camel_husk wolf fox cat ocelot polar_bear panda hoglin zoglin ravager sniffer armadillo turtle',
  creeper: 'creeper',
  spider: 'spider cave_spider',
  bird: 'chicken parrot'
}).flatMap(([family, names]) => names.split(' ').map(name => [name, family])))

// The mob's parts sized to it ([x1, y1, z1, x2, y2, z2, paint, layers?] in its frame), the eye turned into its frame (rays are turned per
// pixel), and the world-space box round its turned parts for screenRect. Turning keeps lengths, so a hit's t compares with the terrain's directly.
export const mobFor = (e, eye) => {
  const model = modelFor(e)
  const parts = model
    ? model.parts.map(p => [...p.box, p.paint, p.layers])
    : FAMILIES[FAMILY_OF[e.name] ?? (e.height >= 2 * e.width ? 'biped' : 'blob')]
      .map(([x1, y1, z1, x2, y2, z2, paint]) => [x1 * e.width, y1 * e.height, z1 * e.width, x2 * e.width, y2 * e.height, z2 * e.width, paint])
  const hull = model ? model.hull : [0, 1, 2].map(i => Math.min(...parts.map(p => p[i]))).concat([3, 4, 5].map(i => Math.max(...parts.map(p => p[i]))))
  const { right, forward } = model ?? yawBasis(e.yaw ?? 0)
  const corners = [[hull[0], hull[2]], [hull[3], hull[2]], [hull[0], hull[5]], [hull[3], hull[5]]]
    .map(([x, z]) => [e.x + x * right.x + z * forward.x, e.z + x * right.z + z * forward.z])
  const ox = eye.x - e.x
  const oz = eye.z - e.z
  return {
    e,
    parts,
    hull,
    right,
    forward,
    palette: paletteFor(e),
    eye: { x: ox * right.x + oz * right.z, y: eye.y - e.y, z: ox * forward.x + oz * forward.z },
    box: [Math.min(...corners.map(c => c[0])), e.y + hull[1], Math.min(...corners.map(c => c[1])), Math.max(...corners.map(c => c[0])), e.y + hull[4], Math.max(...corners.map(c => c[1]))],
    pixels: 0,
    sumX: 0,
    sumY: 0,
    x1: Infinity,
    y1: Infinity,
    x2: -Infinity,
    y2: -Infinity
  }
}

const FACE_INDEX = Object.fromEntries(FACES.map((f, i) => [f, i]))
const clamp01 = v => Math.min(1, Math.max(0, v))

// The colour [r, g, b] (unshaded) of mob `m`'s `part` where a ray `local` (in the mob's frame) hit it at distance t through `face`: the part's
// sheet picture at that point (`images(layerName)` -> {width, height, rgba} | null), else the flat palette.
export const mobPaint = (m, part, face, t, local, images) => {
  const layer = part[7]?.[FACE_INDEX[face]]
  const image = layer && images ? images(layer) : null
  if (image) {
    const [u, v] = faceUV(face, (m.eye.x + local.x * t - part[0]) / (part[3] - part[0]), (m.eye.y + local.y * t - part[1]) / (part[4] - part[1]), (m.eye.z + local.z * t - part[2]) / (part[5] - part[2]))
    const at = (Math.min(image.height - 1, Math.floor(clamp01(v) * image.height)) * image.width + Math.min(image.width - 1, Math.floor(clamp01(u) * image.width))) * 4
    if (image.rgba[at + 3] >= 128) return [image.rgba[at], image.rgba[at + 1], image.rgba[at + 2]]
  }
  // 'south' is the mob's own front: the ray came in through its +z face
  return m.palette[part[6] === 1 && face === 'south' ? 3 : part[6]]
}
