# Agent workspaces

A workspace is an agent's working directory bound to one body and world. It contains instructions, mission notes, and convenient local tools. It points to shared repository tools and world data; it does not copy bundles or create/start a body.

Build the shared tools once, then create a workspace:

```sh
cd dashboard && npm run build-agent-tools
cd ..
node engine/tools/workspace.mjs /tmp/perrin-workspace --body Perrin --world community
cd /tmp/perrin-workspace
./bin/observe
./bin/observe --wait --timeout 60s
./bin/observe catalog jobs
./bin/say 'Hello from Perrin'
./bin/map find
./bin/time clock
```

Use `--worlds /absolute/path/to/worlds` when the world data is elsewhere. Legacy `--state /path/to/old/state` selects `/path/to/old/state/worlds`; choose one path option. The default is the repository's `worlds/`. Paths are resolved at generation time, and wrappers work from any caller directory. An existing running body can be attached immediately.

The generated directory has `AGENTS.md`, `context.edn`, `briefing.md`, `notes/`, and `bin/{observe,jobs,triggers,say,entities,drive,world,map,plans,blueprints,world-changes,time}`. Edit the briefing with the mission and constraints; keep observations and handoffs in notes. `context.edn` records `:body`, `:world`, `:worlds`, `:repo`, and a format identifier. World data remains shared among workspaces.

Each wrapper accepts the central command's arguments with the body and path bindings omitted. Body tools get their bound body; shared memory and time tools only receive world/path bindings. Plans and blueprints also receive the repository path. `./bin/<tool> --help` explains the binding and prints the underlying command's usage even when that tool doesn't implement `--help` itself. Override options such as `--world other`, `--worlds=elsewhere`, `--state`, or `--body` are rejected. This is a routing guard, not a security boundary; direct repository tools remain available. Message and EDN arguments retain their exact boundaries and may contain text such as `--world`.

Rerunning the same generator command is safe and preserves AGENTS.md, briefing.md, and notes. It rejects unrelated nonempty destinations, changed bindings, symlinks at generated destinations, and unrecognized wrapper files. Use `--update-tools` to replace recognized generated wrappers after a generator update; user documents remain untouched. Moving the repository requires a newly generated workspace because the shared loader path is pinned. No command starts a compiler/JVM or an additional Node process: generated scripts load the existing AOT bundle and import the real tool in the same process.
