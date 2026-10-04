# Minecraft agents

The current implementation uses a ClojureScript body engine, a ClojureScript
dashboard, and compiled ClojureScript agent tools. The previous HTTP-action body
and JavaScript dashboard are archived in [legacy/](legacy/README.md).

## Current code

- [engine/](engine/README.md): bodies, jobs, triggers, events, and Mineflayer integration.
- [dashboard/](dashboard/README.md): the current dashboard and compiled shared-world tools.
- `engine/tools/`: stable command launchers for observation, control, jobs,
  triggers, and shared-world tools. Shared-world command logic is in
  `dashboard/src/agent_tools/`; their tests are in `engine/test/tools/`.
- `tools/view/` and `tools/view-*.mjs`: the renderer used by the current dashboard.
- `tools/view/renderer.mjs` and `tools/jar.mjs`: current rendering and asset helpers.
- `tools/dependency-patches/`: source patch adapters for JavaScript dependencies.
- `tools/patch-deps.mjs`, `tools/textures.mjs`, `tools/rcon*.mjs`, and `patches/`:
  current dependency, texture, and test-server infrastructure.
- `blueprints/`: shared blueprint data. `worlds/` holds local world and body data;
  `textures/` holds local rendering assets. Both stay out of git.
- Frozen benchmark captures live under `engine/test/fixtures/pathfinding/` (local, ignored).

Agent tools default to this repository’s `worlds/`, regardless of the shell’s
working directory. Use `--worlds DIR` to select another worlds directory. The
older `--state DIR` still means a parent containing `worlds/`; body and data
migration launchers retain `--state-dir DIR` with the same legacy meaning.
Choose one selector. Account caches are shared under `worlds/.accounts/`, and
job request records are scoped by world and body under
`worlds/<world>/agents/<body>/.commands/jobs/`.

During the live migration, `state/worlds` and the existing `state/commands/<body>`
paths remain compatibility links to the moved data. Old running builds can
continue writing through those links. When a canonical request directory already
has concurrent writes, its records stay in place and the original directory is
retained under the body's `.commands/.legacy-layout-backup/` (currently ProbeWalk).
The remaining `state/` files (logs,
screenshots, inspection reports, manifests, backups and manual notes) are
retained for separate review; they are not required by current body storage.

The current engine, dashboard, tools and tests do not import the archive.
Rendering, asset and dependency patch helpers have current copies; the archive
retains its original helpers for historical tests and launchers. These existing
JavaScript library boundaries retain their behavior; application logic lives in
ClojureScript.

## Build and test

Install the root dependencies with `npm install`, then follow the engine and
dashboard READMEs for their dependencies and launch commands. To use the archived
JavaScript implementation, install its additional plugins with
`npm install --prefix legacy`.

## Local ViaProxy

The local ViaProxy installation lives in `viaproxy/`; its jar, configuration,
saved servers, plugin data, and logs stay local. Start it from the repository
root with `./viaproxy/start`. The launcher sets `viaproxy/` as the working
directory so ViaProxy can find its relative files.

```sh
npm test                             # current renderer and shared infrastructure
npm --prefix engine test             # engine tests
npm --prefix dashboard test          # dashboard tests
npm --prefix engine run test:agent-tools  # build compiled tools and test their CLIs
npm run test:legacy                  # archived implementation tests
```

Build the shared-world commands once with
`npm --prefix dashboard run build-agent-tools`. Their existing command paths
continue to work, for example:

```sh
node engine/tools/map.mjs --world claude find --limit 10
node engine/tools/plans.mjs --world claude list
```

The archive contains the previous source, action library, dashboard, agent
launchers/guides, and tests. Its documentation describes that old implementation,
not the current engine. See [AGENTS.md](AGENTS.md) for development conventions.
