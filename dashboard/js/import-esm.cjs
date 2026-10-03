// import() of an ESM file for the CJS dashboard server. A literal import() inside the shadow-cljs bundle does not
// survive (Closure, and the bundle is not a native module), so the call lives in this plain CommonJS file.
module.exports = href => import(href)
