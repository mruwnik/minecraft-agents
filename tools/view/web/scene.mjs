// Why JavaScript: hands view.scene (dashboard/src/view/scene.cljs, in cljs/viewer.mjs) the JS it drives: the renderer's GL world
// (GPU uploads), the decoder pool (workers), the block tables and the camera and shading maths, which the :esm cljs build cannot
// import by path. Following the pose, the column window and the fetch and decode limits are view.scene; see it for the API.
//
//   const scene = createScene({ agent, radius, fov, interp, renderer, decoder, baseUrl, debugLevel, maxDist, urlParams, finishForLatency, ownStream })
//   renderer.draw(scene.world, { ...scene.frame(now, { camera }), width, height }); scene.drew()
//
// decoder: createDecoder with priority: decodePriority (the pool is shared, so it asks the scenes which job is nearest).
import { cameraBasis } from './camera.mjs'
import { createSceneCore, decodePriority } from './cljs/viewer.mjs'
import { skyDarken, sceneTime } from './shading.mjs'
import { paletteFor } from './mobs.mjs'
import { modelFor, worldBox } from './mob-models.mjs'
import { tablesFor } from './tables.mjs'

export { decodePriority }

// each mob box takes its species' colour (the software renderers' palettes, tools/view/web/mobs.mjs); the box carries name and kind.
// A mob with a model (web/mob-models.mjs) also carries it, turned to its yaw, and its box becomes the one round the model.
export const speciesColored = boxes => boxes.map(box => {
  const colored = { ...box, color: paletteFor(box)[0].map(v => v / 255) }
  const model = modelFor({ name: box.name, height: box.max[1] - box.min[1], yaw: box.yaw })
  if (!model) return colored
  const at = { x: (box.min[0] + box.max[0]) / 2, y: box.min[1], z: (box.min[2] + box.max[2]) / 2 }
  return { ...colored, ...worldBox(model, at), model: { parts: model.parts, right: model.right, origin: [at.x, at.y, at.z] } }
})

export const createScene = ({ renderer, decoder, baseUrl = '', debugLevel = 0, ...options }) => {
  const core = createSceneCore({
    ...options,
    baseUrl,
    decoder,
    world: renderer.createWorld(),
    tables: tablesFor(renderer, { decoder, baseUrl, debugLevel }),
    finish: () => renderer.finish(),
    cameraBasis,
    sceneTime,
    skyDarken
  })
  return { ...core, frame: (now, params) => {
    const drawn = core.frame(now, params)
    return drawn && { ...drawn, entities: speciesColored(drawn.entities) }
  } }
}
