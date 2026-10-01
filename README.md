# The agents' Minecraft bodies

`src/bot.mjs` joins the server named in the `config.json` of the agent folder it runs from (`state/agents/<Name>/`,
made by `node tools/new-agent.mjs <Name>`): `host`, `port` (default localhost:25565) and `auth`. `auth: "offline"`
(the default) is for an offline-mode server, where whitelisting the name is all it takes. `auth: "microsoft"` is for
an online-mode server: the name is a real account's profile name, and a human signs in once with
`node tools/login.mjs <Name>` (a device code in the browser); the body then refreshes its own tokens, and reports
`login_needed` if that ever stops working. Without a config.json the body refuses to start, so a stray run can never
log in under another agent's name.
Mineflayer speaks protocol 26.1; a server newer than that needs ViaVersion + ViaBackwards to bridge it, either as
plugins on a Paper server or, for a vanilla server, as [ViaProxy](https://github.com/ViaVersion/ViaProxy) running
beside the body: point its `viaproxy.yml` at the server (`target-version` the server's, `auth-method: ACCOUNT`, the
Microsoft account added in its window) and give the body `host: 127.0.0.1`, `port: 25568`, `auth: "offline"`.

- Start: `cd state/agents/<Name> && ./start` in the background (reconnects every 10s if the server is down).
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

For gathering errands, have the driver read [the forager role](roles/forager/ROLE.md), then ask naturally:
"Please go and find me 12 roses", "seeds of all crops", "a pair of sheep", "bamboo shoots", or "15 iron".
The role selects existing gathering and delivery commands and checks the result. Its `forage.search` command
adds outward expeditions plus spiral and sweep searches for blocks or farm animals, for example
`./mc forage.search block=bamboo heading=east radius=512 steps=64` or `./mc forage.search mob=sheep count=2`.
Outward search advances in short legs, tries alternative routes after ordinary path failures, and reports how
to resume. The role continues into fresh country after nearby misses and retains errands across rest and resupply.
For known destinations, `./mc travel x= y= z= plan=true` compares complete travel itineraries. It supports walking
and validated powered-rail trips when an authorized cart, straight track corridor and safe exit are supplied.
Prepared horses can also be compared with `horse=<id>` on clear, flat, dry routes up to 128 blocks.
Use `horse_state`, `tame id=<id>`, `horse_saddle id=<id>` and `ride id=<id> x= y= z=` to prepare and ride
an authorized horse, donkey or mule; `horse_dismount` confirms a safe server-reported landing.
For a checked water itinerary, add `boat=<id> shore=<dry feet x:y:z>`; the planner derives the boarding and landing approach.
Ordinary wooden boats require a loaded, level source-water corridor. See [checked travel itineraries](docs/travel.md) for supported routes and arrival safeguards.
Natural-language interpretation belongs to the driver; the CLI takes these structured commands.
New composites load when a body starts, so an already-running body needs a normal restart before using this command.

The planning side is an LLM that pays for every token it reads, so:

- `./mc` prints one terse line per result (`ok goto 14s +mutton:1 @61,69,-107`); add `-v` for the full JSON.
- `./mc scan x1= y1= z1= x2= y2= z2=` draws ASCII slices of a box in one call (all-air layers are collapsed). Use it instead of looping `block_at`.
- `./mc run steps='[{"action":"dig",...},{"action":"place",...}]'` runs several actions in one call and stops at the first failure.
- Pure helpers live in `src/lib.mjs`; run every test with `npm test` (`node --test test/*.test.mjs`).

`node tools/profile-pathfinding.mjs` compares the original and cached pathfinder on open ground, a fence detour
and an unreachable goal, reporting search time, world reads and path equivalence. The cache lasts for one
node expansion; the next expansion reads the world again, including changed blocks and gates.
One second is a scan performance target, not a cutoff. Slow resource scans, frontier scans and route searches
record rate-limited `performance_bug` events with timing and workload details while keeping their results and
continuing normal work. Inspect them with `./mc events type=performance_bug last=10`.

Terrain routing uses block-state collision geometry for body clearance and footing, plus explicit rules for
hazards, fluids, crops, doors and climbing. Harmless decorations do not need a plant whitelist. The local
foraging scan selects connected dry routes; ordinary walking can also use its swimming routes. A route through
partial blocks must fit the body along the transition, not just at its destination. Scaffolding requires the
physics and movement patches installed by `tools/patch-deps.mjs`; its decks behave differently when entering,
climbing, standing above them or sneaking down. This does not make hazardous terrain safe or authorize digging.

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

It is a software raycaster (`src/vision/renderer.mjs`, pure and tested: `node --test test/vision.test.mjs`) over the chunk data
mineflayer already holds, glued to the bot by `src/vision/eyes.mjs`, which draws on a worker thread
(`src/vision/render-worker.mjs`) so the body keeps ticking. No GPU, browser or extra dependency; a 480x270 frame takes
about a tenth of a second, so the dashboard's look popup streams ~10 frames a second.
An image costs the driving LLM roughly width*height/750 tokens (~300 for a PoV shot, ~350 for a panorama), which is
less than most text descriptions of the same scene. Entities are flat-coloured boxes (players magenta, hostiles red).
Not drawn: block light (caves render fully lit, which is handy), translucent water, item/entity models, the sun.

Block textures are Mojang's art, so they are not checked in: `textures/` is gitignored and `tools/textures.mjs` fills it.
Every body start runs it beside `patch-deps.mjs`. With pictures already there it prints `[textures] already 1083` and
stops; with none it extracts `assets/minecraft/textures/block/*.png` from a client jar and prints
`[textures] extracted 1083 from <jar>`. The jar it reads is `$MC_CLIENT_JAR` when that is set, otherwise the newest
plain release the launcher installed under `~/.minecraft/versions/<version>/<version>.jar` (on a Mac,
`~/Library/Application Support/minecraft/versions`); OptiFine, snapshots, pre-releases and mod-loader folders are
skipped. Finding no jar is a warning, never a failure: the body still starts, and the pictures still draw.
To fill `textures/` by hand, or from a jar kept somewhere else:

    node tools/textures.mjs
    MC_CLIENT_JAR=/path/to/1.21.8.jar node tools/textures.mjs

Blocks without a texture (newer than the jar, or entity-rendered like signs) get a colour hashed from their name.

## Watching from outside: the dashboard

    node tools/dashboard.mjs          # http://127.0.0.1:3700 (PORT= to move it)

A browser page that shows where every body is and what it is doing, for whoever is watching rather than playing.
It reads `state/agents/*/config.json`, polls each body's `state` every 2 seconds and draws a top-down map (x east,
z south): a dot per body with its name, health, food and current task, each human as a diamond wherever a body can see them,
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

`/blueprints` (the link in the header) is the blueprint library, `blueprints/*.md` as `src/blueprint.mjs` reads them:
a list (name, tags, footprint, layers, items to fetch, what lint says) and, per blueprint, every layer drawn from the
ground up - one coloured cell per block, the roles of one wood or stone as shades of one hue, hover for the block and
its offset from the anchor - with the legend, the bill of materials and lint's lines, plus the marked places whose note
says they were built from it. `/api/blueprints` and `/api/blueprint/<name>` hand out the same data as JSON; the drawing
is `tools/dashboard/blueprint.mjs`, pure like the map module.

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
- Inside any Claude Code session under the bot/ folder of this repo: the `/minecraft-agent [Name]` skill does the same
  from within (`.claude/skills/minecraft-agent/SKILL.md`).
- From another agent's session: spawn a named sonnet subagent (see the skill's last paragraph). The humans prefer this over headless,
  because the subagent shows up in their session.
- Without a terminal (from a script): `./play Aviendha -p --model sonnet --permission-mode auto --max-budget-usd 3`
  runs the session headless until it stops or the budget is spent. Tried 2026-09-19: works; the agent found its body already
  running, greeted in chat and went on building. Only ever one driver per body.

## Patched dependencies

`tools/patch-deps.mjs` fixes `node_modules/mineflayer-pathfinder/index.js` (a crash on every tick after a walk opens a fence gate while the
body carries a scaffolding block). `start-body` runs it at every body start, so an `npm install` cannot undo it; the first line of each
`bot.log` says `patched`, `already` or `anchor missing` (= a new pathfinder version: read `patchPathfinder` in `src/lib.mjs`).
It also fixes `node_modules/mineflayer/lib/plugins/entities.js`: mineflayer 4.39.0 writes the air_supply of EVERY entity's
metadata packet into `bot.oxygenLevel`, so a body beside a pond read a squid's air and a swimmer's by turns (`oxygen` fell
20 -> 8 -> 7 on dry land, card 962beec2). The patch keeps only the body's own (`patchOwnBreath` in `src/airlog.mjs`).
