// What a lead says when it gives up fetching an animal. The old answer blamed the animal every time ("will not
// follow: is there a fence or water between you?"), and three times in one afternoon the evidence said the BODY had
// never moved: flock.lead beside a wheat field's fence stalled pressing forward at the same cell with a found path,
// then fetched three times and gave up (card fc47bf28). The body's frozen_walk event (src/stall.mjs) already names
// what froze it; a stall seen since the fetches began makes the failure the walk's own, and the answer says so.

// the frozen walk that belongs to these fetches: one seen before they began is some earlier walk's
export const stalledSince = (frozen, sinceMs) => frozen && frozen.at >= sinceMs ? frozen : null

export const fetchFailure = ({ mob, frozen }) => {
  if (!frozen) return `the ${mob} will not follow (fetched it 3 times, got no nearer): is there a fence or water between you? Get them out in the open first, or lead fewer`
  const where = `${frozen.pos.x},${frozen.pos.y},${frozen.pos.z}`
  return `I could not walk to the ${mob} (stalled at ${where} pressing forward)${frozen.advice ? `: ${frozen.advice}` : ''}`
}
