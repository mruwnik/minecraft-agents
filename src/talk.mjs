// Talking to one person. Chat goes to everyone; a whisper goes to one player, only they see it, and it ends their
// ./mc wait at once. Agents kept chatting "Chani: ..." to the whole server (38 chats against 5 whispers, 720224ff),
// so `chat` reads its own text: a message that opens with an online player's name gets a hint back, and a whisper
// to someone who is not online is refused (the server's own "No player was found" never reaches the driver).
const CROWD = new Set(['all', 'everyone', 'everybody', 'anyone', 'anybody'])
// "@Chani ...", "Chani: ...", "Chani, ..." - a bare name with no mark after it is just a word ("Chani has the carrots")
const OPENING = /^\s*(?:@([A-Za-z0-9_]{1,16})(?=\s|$)|([A-Za-z0-9_]{1,16})\s*[:,])/

const onlineSpelling = (name, players) => players.find(p => p.toLowerCase() === String(name).toLowerCase()) ?? null

// the online player the message opens with, in the spelling that is online, or null
export const addressedTo = (text, players) => {
  const m = OPENING.exec(String(text ?? ''))
  if (!m) return null
  const name = m[1] ?? m[2]
  if (CROWD.has(name.toLowerCase())) return null
  return onlineSpelling(name, players)
}

export const whisperHint = name => `this read as a message to ${name}: whisper player=${name} next time, only they see it and it wakes their wait`

// null when the player is online; otherwise why the whisper is refused, naming who could be whispered to instead
export const offlineWhisper = (name, players) => {
  if (onlineSpelling(name, players)) return null
  const others = players.length ? `Online now: ${players.join(', ')}` : 'Nobody else is online'
  return `${name} is not online, so a whisper would go nowhere. ${others}`
}
