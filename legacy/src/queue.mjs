// ./mc <action> queue=true: a chore that waits for the current task instead of superseding it. A driver on 09-26 issued
// a side `craft item=stone_hoe` while a routine ran, and the routine was cancelled for it (card 6cf481c0). Pure order;
// src/body/jobs.mjs holds the queue and starts the next chore when a task ends, so a routine keeps its body until it is done.
export const withoutQueue = ({ queue, ...args }) => args

export const enqueue = (queue, { name, args }, id) => [...queue, { id, name, args: withoutQueue(args ?? {}) }]

export const dequeue = queue => (queue.length ? { next: queue[0], rest: queue.slice(1) } : { next: null, rest: [] })

// the reply the caller gets at once: which task it waits for and how many chores stand before it
export const queuedReply = (id, after, position) => ({
  ok: true,
  queued: id,
  after: `${after.name} (task ${after.id})`,
  position,
  note: `runs when task ${after.id} ends${position > 1 ? ` and ${position - 1} queued chore${position > 2 ? 's are' : ' is'} done` : ''}; its result comes as a task_done event in ./mc wait`
})

// ./mc stop takes the body back: the chores waiting behind the task go too, and the stop says which
export const droppedLine = queue => (queue.length ? queue.map(c => `${c.name} (queued ${c.id})`).join(', ') : null)
