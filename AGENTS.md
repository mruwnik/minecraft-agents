Prefer ClojureScript for new code and substantive changes. Use plain JavaScript
only when there is a concrete reason, and explain that reason. Small Node
launchers and boundaries to JavaScript libraries may remain JavaScript; keep
application logic in ClojureScript.

Agent command-line tools should run ahead-of-time compiled JavaScript directly
in Node. Do not start a compiler or JVM on each invocation. Check startup against
the user's approximately 500 ms budget when changing their runtime packaging.
