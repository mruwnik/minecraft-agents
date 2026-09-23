# Claude's Minecraft body

`src/bot.mjs` joins the server in `..` (offline mode, port 25565) as the player **Claude**.
Mineflayer speaks protocol 26.1; ViaVersion + ViaBackwards in `../plugins` bridge it to the 26.2 server.

- Start: `node src/bot.mjs` (reconnects every 10s if the server is down). Override defaults in `config.json`.
- Reflexes handled in-process: eating, armour, fighting nearby hostiles, running from creepers.
- Control API: `http://127.0.0.1:3777/<action>` with a JSON body; `./mc <action> key=value ...` wraps it.
  `./mc help` lists actions. Long actions (goto, mine, craft, place, ...) take over the body, return after
  `timeout` seconds (default 60) with `status: running`, and then report via a `task_done` event.
- Everything notable (chat, damage, death, night, task results) is appended to `events.jsonl`.

## Where things live

Never move or rename a folder under `state/` (or `state/` itself) while a body runs from it: the body appends
events by an absolute path fixed at start, so the first write after the move kills it. `./mc quit` every body
first (moving `agents/` to `state/agents/` on 2026-09-22 took two bodies down this way).

    src/        the body: bot.mjs (reflexes, primitives, the composite runner, the HTTP API), lib.mjs (pure helpers,
                tested), eyes.mjs + vision.mjs (what it sees), builder.mjs (plans -> jobs), pens.mjs
    library/    one composite action per file: library/<folder>/<file>.mjs is `./mc <folder>.<file>`
    tools/      mc.mjs (the CLI behind ./mc), start-body (behind an agent's ./start), new-agent.mjs,
                patch-deps.mjs, textures.mjs (both run at every body start), rcon.mjs
    test/       every *.test.mjs; `npm test` runs them all (`node --test test/*.test.mjs`)
    state/      everything this world made, and the only folder besides node_modules/ and textures/ that git ignores:
                agents/<Name>/ (one folder per agent: config.json, BRIEFING.md, journal.md, events.jsonl,
                snapshots/, its own ./mc and ./start), places.json (the shared map), zones.json (protected
                boxes), clock.json (the world's time), gates.log (who opened which gate), WORLD.md and BUGS.md
    roles/      knowledge and routines an agent can read on demand; harness/ notes per program that runs an agent
    textures/   block textures for `./mc look` (not checked in; see Vision below)

`./mc`, `./play` and `AGENT_GUIDE.md` stay at the top, with `harness/`: they are true on any server. What belongs to
THIS world is under `state/`, `WORLD.md` and `BUGS.md` included, so an agent still reads `../../WORLD.md` from its folder.

## Keeping the driver's context small

The planning side is an LLM that pays for every token it reads, so:

- `./mc` prints one terse line per result (`ok goto 14s +mutton:1 @61,69,-107`); add `-v` for the full JSON.
- `./mc scan x1= y1= z1= x2= y2= z2=` draws ASCII slices of a box in one call (all-air layers are collapsed). Use it instead of looping `block_at`.
- `./mc run steps='[{"action":"dig",...},{"action":"place",...}]'` runs several actions in one call and stops at the first failure.
- Pure helpers live in `src/lib.mjs`; run every test with `npm test` (`node --test test/*.test.mjs`).

## Protected zones and doors

Walks are walk-only by default (`makeMoves(false)` in `src/bot.mjs`). `mine` and `goto dig=true` use the digging movements, which
dig through or scaffold over anything in their way, including our own walls and roofs.
`./mc protect name=<n> x1= y1= z1= x2= y2= z2=` (saved in `state/zones.json`) forbids path-digging and scaffolding inside a box;
`./mc zones` lists them and `./mc unprotect name=<n>` removes one. Explicit `dig`, `mine` and `place` still work there.
Protect every build, including a layer or two of ground beneath it, or it will tunnel under.

mineflayer-pathfinder only understands fence gates, so `src/bot.mjs` marks wooden doors as walkable and `doorTick`
opens a closed door as the bot reaches it, then shuts it again once through (only doors it opened itself).
A door is oriented by where the bot stands when placing it: stand in front of the doorway, outside.

## Vision

`./mc look` renders what the bot sees to `snapshots/look-NNN.png` and returns the file path plus `seen`: the entities
actually visible (not hidden behind blocks) with the pixel they are centred on and their distance, so names need no OCR.

    ./mc look                      # where the bot is facing (640x360, fov 100)
    ./mc look dir=east             # or yaw=<deg> pitch=<deg>
    ./mc look x=116 y=70 z=-141    # towards a block
    ./mc look pano=true            # 360 degrees, 1024x256: north in the middle, west to its left, south at the edges
    options: width= height= fov= dist=<blocks, default 64, max 96> file=<name.png>

It is a software raycaster (`src/vision.mjs`, pure and tested: `node --test test/vision.test.mjs`) over the chunk data mineflayer
already holds, glued to the bot by `src/eyes.mjs`. No GPU, browser or extra dependency; a frame takes about a second.
An image costs the driving LLM roughly width*height/750 tokens (~300 for a PoV shot, ~350 for a panorama), which is
less than most text descriptions of the same scene. Entities are flat-coloured boxes (players magenta, hostiles red).
Not drawn: block light (caves render fully lit, which is handy), translucent water, item/entity models, the sun.

Block textures are Mojang's art, so they are not checked in: `textures/` is gitignored and `tools/textures.mjs` fills it.
Every body start runs it beside `patch-deps.mjs`. With pictures already there it prints `[textures] already 1083` and
stops; with none it extracts `assets/minecraft/textures/block/*.png` from a client jar and prints
`[textures] extracted 1083 from <jar>`. The jar it reads is `$MC_CLIENT_JAR` when that is set, otherwise the newest
plain release under `~/.minecraft/versions/<version>/<version>.jar` (OptiFine, snapshots, pre-releases and mod-loader
folders are skipped). Finding no jar is a warning, never a failure: the body still starts, and the pictures still draw.
To fill `textures/` by hand, or from a jar kept somewhere else:

    node tools/textures.mjs
    MC_CLIENT_JAR=/path/to/1.21.8.jar node tools/textures.mjs

Blocks without a texture (newer than the jar, or entity-rendered like signs) get a colour hashed from their name.

## Watching from outside: the dashboard

    node tools/dashboard.mjs          # http://127.0.0.1:3700 (PORT= to move it)

A browser page that shows where every body is and what it is doing, for whoever is watching rather than playing.
It reads `state/agents/*/config.json`, polls each body's `state` every 2 seconds and draws a top-down map (x east,
z south): a dot per body with its name, health, food and current task, Dan as a diamond wherever a body can see him,
protected zones as boxes and marked places as crosses. Click a body and its view appears beside the map, rendered
through its own eyes. Drag to pan, wheel to zoom; the map fits itself around the bodies, and "fit everything" widens
it to the whole map.

It only reads. `state` and `look` are both **quick** actions in `src/bot.mjs`: they answer without taking the task
slot and without turning the body, so watching a body cannot cancel or disturb the work it is doing, and it costs
that agent's driver nothing - no tokens are spent by looking. A port that does not answer is simply a body that is
down. Its own API, for scripts: `/api/state` (every body, plus places and zones) and `/api/look/<Name>` (a PNG, with
`?pano=1`; what the body saw comes back in the `x-look-view`, `x-look-seen` and `x-look-blocked` headers).

The map arithmetic is in `tools/dashboard/map.mjs`, which has no node imports so the page and `npm test` use the
same code; `tools/dashboard/lib.mjs` reads the folders and routes.

## Agents: one folder each

    node tools/new-agent.mjs             # draws a name from ~/.claude/hooks/choose_name.py (redraws until it is a valid, unused
    node tools/new-agent.mjs Lightsong   # Minecraft username), or takes the one given
    node tools/new-agent.mjs Nona --harness codex   # the program that will run it: a notes file in harness/ (default claude-code)

creates `state/agents/<Name>/` with everything that agent owns:

    config.json    username, its own apiPort, its harness, and the character the name comes from
    BRIEFING.md    who the agent is, how its folder works and what to read next; hand this to a new agent as its first read
    journal.md     the agent's own memory between sessions
    start, mc      start its body / drive it (`./mc look pano=true`), no ports or paths to remember
    events.jsonl   what happens to it; snapshots/ what it sees; bot.log the body's console output

Shared by everyone, in this directory: the code, `AGENT_GUIDE.md` (toolset, house rules, token habits; true on any
server and harness), `harness/<name>.md` (what is specific to Claude Code, Codex, ...: waiting, timeouts, delegation),
`state/WORLD.md` (this server: who plays, shared places, customs; changes often), `state/places.json`
(the common map of points of interest: `./mc mark`, `./mc places`, `goto place=<name>`), `state/zones.json`
(protected builds; a change by one bot reaches the others within seconds) and `textures/`.
Each name must be whitelisted once, on the server console: `whitelist add <Name>`.

## Starting an agent with one command

- `./play [Name] [claude options]` in a terminal: creates the agent if needed (whitelists it through `tools/rcon.mjs`; if that fails, prints the `whitelist add` line and waits),
  then starts a Claude Code session in the agent's folder with its opening instructions. One long-running session per agent.
- Inside any Claude Code session under `/home/dan/minecraft/claude`: the `/minecraft-agent [Name]` skill does the same
  from within (`.claude/skills/minecraft-agent/SKILL.md`).
- From another agent's session: spawn a named sonnet subagent (see the skill's last paragraph). Dan prefers this over headless,
  because the subagent shows up in his session.
- Without a terminal (from a script): `./play Aviendha -p --model sonnet --permission-mode auto --max-budget-usd 3`
  runs the session headless until it stops or the budget is spent. Tried 2026-09-19: works; the agent found its body already
  running, greeted in chat and went on building. Only ever one driver per body.

## Patched dependencies

`tools/patch-deps.mjs` fixes `node_modules/mineflayer-pathfinder/index.js` (a crash on every tick after a walk opens a fence gate while the
body carries a scaffolding block). `start-body` runs it at every body start, so an `npm install` cannot undo it; the first line of each
`bot.log` says `patched`, `already` or `anchor missing` (= a new pathfinder version: read `patchPathfinder` in `src/lib.mjs`).

