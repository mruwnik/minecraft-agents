Village intentions belong in `worlds/<world>/plans/<id>.edn`. The village
page reads plans whose `:kind` is `:village`. `:at` records a map anchor; it does
not claim an occupied region. Geometry remains in ordinary plan `:parts`.

Build the migration tool once, then run its compiled JavaScript directly:

```sh
cd dashboard
npm run build-migrate-data
cd ..
node dashboard/out/migrate-data.cjs --root . --world claude --dry-run
node dashboard/out/migrate-data.cjs --root . --world claude --apply
```

`--worlds /path/to/worlds` selects another worlds directory; legacy
`--state-dir /path/to/state` selects a parent containing `worlds/`. `--world` and
exactly one of `--dry-run` or `--apply` are required. This tool does not invoke a
compiler or JVM at runtime.

The migration reads the selected world's village markers from `places.json`
and existing global `village-inspections/*.json`. The explicit world argument
assigns those historic inspection intentions to that world; contradictory
marker/inspection anchors and duplicate inspection identities are refused.
The legacy JSON files remain untouched.

For an existing native plan, the migration preserves its geometry,
assignments, note, and other native metadata. It adds `:kind :village`, the
recorded `:at`, and nonconflicting provenance metadata. Existing values that
conflict with the intended metadata cause refusal before any writes. A missing
plan is created with `:parts []` and
`:metadata {:geometry :incomplete ...}`. Marker descriptions do not become
guessed building geometry or automatic active protections.

The metadata retains full `:legacy-place` and `:legacy-inspection` values,
recorded `:population` intentions, and source file names and SHA-256 hashes.
Unknown fields remain available, including arbitrary string keys. Historic
inspection evidence never proves current population, shelter, or trade stock;
the village display labels it stale. Villagers are independent engine
observations and are never enrolled as plan members by this migration.

Every source and destination is checked before applying. Existing plan updates
take the native writer's per-file lock and compare the original text before an
atomic replacement. Their exact original text is first backed up to
`worlds/<world>/.migration-backups/village-plans/<id>.<sha256>.edn`.
New files and backups use atomic create-only links; nothing already present is
overwritten by those operations. Repeating an unchanged migration produces
`:unchanged` results. A concurrent edit causes refusal; if an earlier file was
already completed, a rerun safely resumes. Backup paths are included in the
apply report so the original text can be inspected or restored deliberately.

The migration neither reads nor writes a villager roster or entity notes.
Entity observation updates are handled by the engine's transient cache.
