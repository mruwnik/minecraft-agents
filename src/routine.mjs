// A routine over several places. A farmstead is several fields 40 blocks apart (a crop field, a melon patch, a cane
// stand), and `routine name=farmer/homestead place=a,b,c` is the whole day: the role's routine expanded once per
// place, in the order given. The one-place call is the routine exactly as it was.
import { routineSteps, placeRefusal } from './lib.mjs'

// place=a,b,c as the CLI hands it over (one string), or a list already
export const placeList = place => (Array.isArray(place) ? place : String(place ?? '').split(','))
  .map(name => String(name).trim())
  .filter(Boolean)

// the day's steps: routineSteps once per place, in order. A routine with no $place in it run over several places would
// do the same chores N times, which is never what was meant, so that call is refused rather than repeated.
export function routinePlan (a, readRole) {
  const places = placeList(a.place)
  if (places.length === 0) {
    const { steps, error } = routineSteps({ ...a, place: undefined }, readRole)
    return error ? { error } : { places, steps }
  }
  const rounds = []
  for (const place of places) {
    const { steps, error } = routineSteps({ ...a, place }, readRole)
    if (error) return { error }
    rounds.push(steps)
  }
  const same = places.length > 1 && JSON.stringify(rounds[0]) === JSON.stringify(rounds[1])
  if (same) return { error: `${a.name ? `roles/${a.name}.json` : 'steps='} has no $place to fill: place=${places.join(',')} would run the same steps ${places.length} times` }
  return { places, steps: rounds.flat() }
}

// A routine is days long, so it refuses before day one rather than failing the same way once a round (#144): a place
// nobody marked is named (every place asked for, so one call fixes them all), and somebody else's ground answers as it
// always does, wherever in the list it sits.
export function unmarkedPlaces (places, names) {
  const unknown = names.filter(name => !(places ?? []).some(p => p.name === name))
  return unknown.length ? `no place called ${unknown.join(', ')} on the shared map (places lists what is marked): a routine over places you name stops here rather than failing the same way once a round` : null
}

export function placesRefusal (places, names, me) {
  const unknown = unmarkedPlaces(places, names)
  if (unknown) return unknown
  for (const name of names) {
    const refusal = placeRefusal(places, name, me)
    if (refusal) return refusal
  }
  return null
}
