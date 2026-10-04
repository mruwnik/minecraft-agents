// Verbatim from 8d0327d9^:tools/view/web/hub.mjs lines 25-96 (scheduling, eventCache, percentile, keep), the JS view.schedule replaced,
// plus streamKey: the key computation inlined in that file's syncStream (lines 267-270), made a function. Used by tools/view/bench-cljs-vs-js.mjs.
// ---- scheduling (pure) ----

// The targets to render now: visible ones that are `raf` (every animation frame, first, all of them) or whose dueAt has passed
// (the longest-overdue first, ties keep their order, at most `max` of them).
export const dueScenes = (entries, now, max = Infinity) => {
  const visible = entries.filter(e => e.visible)
  const raf = visible.filter(e => e.raf)
  const rest = visible.filter(e => !e.raf && e.dueAt <= now).sort((a, b) => a.dueAt - b.dueAt).slice(0, max)
  return [...raf, ...rest].map(e => e.id)
}

// When a target rendered at `now` is due again: one period after it was due, so the rate stays exactly `fps` however the
// frames fall; a target that is more than a period behind is not caught up with, it restarts from now. 'raf': next frame.
export const nextDueAt = (dueAt, now, fps) => {
  if (fps === 'raf') return now
  const period = 1000 / fps
  const next = dueAt + period
  return next > now ? next : now + period
}

// One animation frame's renders. Every due raf target renders. The others render while the frame's total cost (raf ones included)
// is under budgetMs, so cards give way to a big view; but a card more than half a period late renders anyway, one per frame, so a
// slow big view slows the cards without starving them. run(id) renders one target and returns its cost in ms. Returns the ids rendered.
export const planFrame = ({ entries, now, budgetMs, run }) => {
  const byId = new Map(entries.map(e => [e.id, e]))
  const rendered = []
  let spent = 0
  let late = 0
  for (const id of dueScenes(entries, now)) {
    const e = byId.get(id)
    if (!e.raf && spent >= budgetMs) {
      const overdue = now - e.dueAt > 500 / e.fps
      if (!overdue || late) continue
      late++
    }
    spent += run(id)
    rendered.push(id)
  }
  return rendered
}

// The last pose and hud event of each agent seen on the shared stream. The stream reopens only when the set of agents changes, so a
// scene added later for an agent already streamed is fed these at once (replay) instead of waiting for a pose that may be far off.
export const eventCache = () => {
  const last = new Map() // `${event}|${agent}` -> data
  return {
    record: (event, data) => {
      if (event === 'pose' || event === 'hud') last.set(`${event}|${data.agent}`, data)
    },
    replay: (agent, feed) => {
      for (const event of ['pose', 'hud']) {
        const data = last.get(`${event}|${agent}`)
        if (data) feed(event, data)
      }
    },
    keepOnly: agents => {
      for (const key of [...last.keys()]) if (!agents.includes(last.get(key).agent)) last.delete(key)
    }
  }
}

const percentile = (values, p) => {
  if (!values.length) return null
  const sorted = [...values].sort((a, b) => a - b)
  return sorted[Math.min(sorted.length - 1, Math.floor(p * sorted.length))]
}

const keep = (list, value, max) => {
  list.push(value)
  if (list.length > max) list.shift()
}


export const streamKey = scenes => {
  const agents = [...new Set(scenes.map(s => s.agent))].sort()
  const radius = Math.max(0, ...scenes.map(s => s.radius))
  return { key: `${agents.join(',')}|${radius}`, agents, radius }
}
