// Talking to one person. Chat goes to everyone; a whisper goes to one player, only they see it, and it ends their
// ./mc wait at once. Agents kept chatting "Chani: ..." to the whole server (38 chats against 5 whispers, 720224ff),
// so `chat` reads its own text: a message that opens with an online player's name gets a hint back, and a whisper
// to someone who is not online is refused (the server's own "No player was found" never reaches the driver).
const CROWD = new Set(['all', 'everyone', 'everybody', 'anyone', 'anybody'])
// "@Chani ...", "Chani: ...", "Chani, ..." - a bare name with no mark after it is just a word ("Chani has the carrots")
const OPENING = /^\s*(?:@([A-Za-z0-9_]{1,16})(?=\s|$)|([A-Za-z0-9_]{1,16})\s*[:,])/
// "morning, Chani", "the bed is yours, Chani!" - a name after a comma at the very end is a vocative; without the comma it is a word
const CLOSING = /,\s*@?([A-Za-z0-9_]{1,16})\s*[!?.]*\s*$/

const onlineSpelling = (name, players) => players.find(p => p.toLowerCase() === String(name).toLowerCase()) ?? null

// the online player the message opens or closes with, in the spelling that is online, or null
export const addressedTo = (text, players) => {
  const named = m => {
    if (!m) return null
    const name = m[1] ?? m[2]
    return CROWD.has(name.toLowerCase()) ? null : onlineSpelling(name, players)
  }
  const line = String(text ?? '')
  return named(OPENING.exec(line)) ?? named(CLOSING.exec(line))
}

export const whisperHint = name => `this read as a message to ${name}: whisper player=${name} next time, only they see it and it wakes their wait`

// null when the player is online; otherwise why the whisper is refused, naming who could be whispered to instead
export const offlineWhisper = (name, players) => {
  if (onlineSpelling(name, players)) return null
  const others = players.length ? `Online now: ${players.join(', ')}` : 'Nobody else is online'
  return `${name} is not online, so a whisper would go nowhere. ${others}`
}

// The server takes 256 characters per chat line, and a whisper spends some of them on its "/tell <name> " header.
// mineflayer cuts a longer text into 256-character lines mid-word; before that our own primitives cut it off silently
// at a fixed length (the human, 17:50Z: "whisper seems to have a length limit"). A long text now goes out in numbered
// pieces, each cut at the last sentence end that fits, else the last word end, else hard.
export const CHAT_MAX = 256
export const sayLimit = player => CHAT_MAX - (player ? `/tell ${player} `.length : 0)

const SENTENCE_ENDS = ['. ', '! ', '? ', '; ']
// how many characters of `text` make the next piece under `budget`
const cutAt = (text, budget) => {
  if (text.length <= budget) return text.length
  const head = text.slice(0, budget + 1)
  const sentence = Math.max(...SENTENCE_ENDS.map(m => head.lastIndexOf(m)))
  if (sentence > 0) return sentence + 1
  const space = head.lastIndexOf(' ')
  return space > 0 ? space : budget
}
const pieces = (text, budget) => {
  const out = []
  for (let rest = text; rest.length; rest = rest.slice(cutAt(rest, budget)).trim()) out.push(rest.slice(0, cutAt(rest, budget)).trim())
  return out
}
const numbered = parts => parts.map((p, i) => `(${i + 1}/${parts.length}) ${p}`)

// the pieces to send for `text`, each within `max` characters including its "(i/n) " number; one untouched piece when it fits
export const splitSay = (text, max) => {
  const whole = String(text ?? '').trim()
  if (!whole) return []
  if (whole.length <= max) return [whole]
  const few = pieces(whole, max - '(9/9) '.length)
  return numbered(few.length < 10 ? few : pieces(whole, max - '(99/99) '.length))
}
