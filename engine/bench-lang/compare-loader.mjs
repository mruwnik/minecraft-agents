// the resolve hook of compare-hook.mjs: a planner test's import of ./planner.mjs becomes the comparing wrapper
export async function resolve (specifier, context, next) {
  const fromPlannerTest = /\/js\/path\/planner[^/]*\.test\.mjs$/.test(context.parentURL ?? '')
  if (fromPlannerTest && /(^|\/)planner\.mjs$/.test(specifier)) return { url: new URL('./compared-planner.mjs', import.meta.url).href, shortCircuit: true }
  return next(specifier, context)
}
