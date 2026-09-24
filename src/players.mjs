// Telling agent bodies from human players, by name, with no name hard-coded either way.
import fs from 'node:fs'
import path from 'node:path'

// The agent bodies playing in this world: every folder under state/agents that holds a config.json
export const agentNames = stateDir =>
  fs.readdirSync(path.join(stateDir, 'agents'))
    .filter(name => fs.existsSync(path.join(stateDir, 'agents', name, 'config.json')))

// Splits a `players` map ({name: pos|'out of sight'}) into the agent bodies and the human players it names
export const splitPlayers = (players, agents) => {
  const names = Object.keys(players)
  return {
    agents: names.filter(name => agents.includes(name)),
    humans: names.filter(name => !agents.includes(name))
  }
}
