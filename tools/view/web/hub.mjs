// Why JavaScript: the bare-metal side of the hub: the one shared WebGL2 context, its hidden canvas, the GPU timer queries and the
// drawImage copy onto each card's 2D canvas. Which scene renders when, the targets, the /poses stream and the stats are view.hub
// (dashboard/src/view/hub.cljs, in cljs/viewer.mjs), which drives this surface; see that namespace for the hub's API.
//
//   const hub = createViewHub({ maxScenes: 12, fps: 6, baseUrl: '' })
//
// window.__hub (set by hub-demo.html) exposes hub.stats() for measurement.
import { viewHub } from './cljs/viewer.mjs'
import { createDecoder } from './decoder.mjs'
import { createRenderer } from './gl.mjs'
import { createScene, decodePriority } from './scene.mjs'

const PLACEHOLDER = '#1a1a1d'

const tryRenderer = canvas => {
  try {
    return createRenderer(canvas)
  } catch (error) {
    console.warn(`view hub: no WebGL2 (${error.message}); cards keep what they show`)
    return null
  }
}

// The surface view.hub draws on; null without WebGL2.
const createSurface = ({ baseUrl = '', debugLevel = 0 } = {}) => {
  const hidden = document.createElement('canvas')
  const renderer = tryRenderer(hidden)
  if (!renderer) return null
  renderer.setDebug(debugLevel > 0)
  const gl = renderer.gl
  const decoder = createDecoder({ priority: decodePriority })
  const timer = gl.getExtension('EXT_disjoint_timer_query_webgl2')
  const pending = [] // {query, onGpuMs}

  // only a draw bigger than every attach grows the hidden canvas past what fit set
  const grow = (width, height) => {
    if (hidden.width < width) hidden.width = width
    if (hidden.height < height) hidden.height = height
  }

  return {
    rendererName: renderer.renderer,
    gpuTimer: timer !== null,
    createScene: options => createScene({ ...options, renderer, decoder, baseUrl, debugLevel, ownStream: false }),
    fit: (width, height) => {
      if (hidden.width !== width) hidden.width = width
      if (hidden.height !== height) hidden.height = height
    },
    // draws into the bottom-left of the hidden canvas, timed by a GPU query when there is one
    draw: (world, params, width, height, onGpuMs) => {
      grow(width, height)
      const query = timer ? gl.createQuery() : null
      if (query) gl.beginQuery(timer.TIME_ELAPSED_EXT, query)
      renderer.draw(world, { ...params, width, height })
      if (!query) return
      gl.endQuery(timer.TIME_ELAPSED_EXT)
      pending.push({ query, onGpuMs })
    },
    poll: () => {
      const waiting = []
      for (const item of pending) {
        if (!gl.getQueryParameter(item.query, gl.QUERY_RESULT_AVAILABLE)) {
          waiting.push(item)
          continue
        }
        const disjoint = gl.getParameter(timer.GPU_DISJOINT_EXT)
        const ms = gl.getQueryParameter(item.query, gl.QUERY_RESULT) / 1e6
        gl.deleteQuery(item.query)
        if (!disjoint) item.onGpuMs(ms)
      }
      pending.length = 0
      pending.push(...waiting)
    },
    // WebGL's origin is bottom-left, the 2D canvas's top-left: the corner the viewport drew is the hidden canvas's last `height` rows
    copy: (canvas, width, height) => {
      const started = performance.now()
      canvas.getContext('2d').drawImage(hidden, 0, hidden.height - height, width, height, 0, 0, width, height)
      return performance.now() - started
    },
    placeholder: (canvas, width, height) => {
      const ctx = canvas.getContext('2d')
      ctx.fillStyle = PLACEHOLDER
      ctx.fillRect(0, 0, width, height)
    },
    bitmap: (width, height) => createImageBitmap(hidden, 0, hidden.height - height, width, height),
    finish: () => gl.finish(),
    memory: () => renderer.memory(),
    close: () => gl.getExtension('WEBGL_lose_context')?.loseContext()
  }
}

export const createViewHub = (options = {}) => viewHub(options, createSurface(options))
