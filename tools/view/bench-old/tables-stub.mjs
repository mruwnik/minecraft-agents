// Stands in for tools/view/web/tables.mjs in the bench: the block table is fetched once per scene and is not what is measured.
export const tablesFor = () => ({ ensure: () => Promise.resolve({}) })
