// The event tail: watching for a chat/state pattern, and how repeated errors within a window are folded together.

// One check of a watch against how many matching things are there now. Fires on the edge (condition just became
// true), so a standing condition doesn't repeat itself. {count=1, atMost=false, met} -> {fire, met}
export function checkWatch (watch, seen) {
  const wanted = watch.count ?? 1
  const met = watch.atMost ? seen <= wanted : seen >= wanted
  return { fire: met && !watch.met, met }
}
// what chat and whisper say: {text} | {error}. String(undefined) went out to the whole server as "undefined"
export function chatText (args, max) {
  const text = ['string', 'number'].includes(typeof args.message) ? String(args.message).trim() : ''
  if (text) return { text: text.slice(0, max) }
  const given = Object.keys(args).filter(k => k !== 'player')
  return { error: `nothing said: the text goes in message=${given.length ? ` (you gave ${given.map(k => `${k}=`).join(' ')})` : ''}. Quote it: ./mc chat message="hello all"` }
}

// `events` after a restart showed "nothing yet" although events.jsonl was full, and sent agents to the raw log: a body starts from the
// tail of its own file. The read may cut the first line in half, and the last may be half written
export function parseEventTail (text, limit) {
  const parse = line => { try { return JSON.parse(line) } catch { return null } }
  return text.split('\n').map(parse).filter(e => e && typeof e === 'object').slice(-limit)
}

// One fact, said once. After the 09-22 20:53 server restart Perrin's and Mariel's bodies wrote the same uncaught error
// into their events files every few seconds until the file was unreadable, and the one line that mattered (the restart)
// was buried under thousands of copies of itself. So the first of a message is said, the repeats are counted silently,
// and the count is said when the message changes or the window runs out. `seen` is opaque state: keep it, pass it back.
export function errorRepeat (seen, message, now, window = 60000) {
  if (seen?.message !== message) return { say: message, seen: { message, said: now, suppressed: 0 } }
  const suppressed = seen.suppressed + 1
  if (now - seen.said < window) return { say: null, seen: { ...seen, suppressed } }
  return { say: `${message} (${suppressed} more in the last ${Math.round((now - seen.said) / 1000)}s)`, seen: { message, said: now, suppressed: 0 } }
}
// One repeat window per KIND of message, not one for the whole process. A body refused at login is kicked again on
// every retry, every ten seconds, for ever (#119), and the single shared slot made it worse than it looks: two faults
// taking turns each reset the other's window, so neither was ever suppressed. A kick loop gets a long window because
// it will never stop on its own and the first line already said everything; a changed reason is still said at once,
// which a plain gate would have thrown away.
export const REPEAT_DEFAULT = 60000
export const REPEAT_WINDOW = { kicked: 600000 }
export function repeatByType (seen, type, message, now) {
  const { say, seen: next } = errorRepeat(seen?.[type] ?? null, message, now, REPEAT_WINDOW[type] ?? REPEAT_DEFAULT)
  return { say, seen: { ...seen, [type]: next } }
}
