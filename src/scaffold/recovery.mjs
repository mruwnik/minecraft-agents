const CENTER_REFUSALS = [
  'center_work_stand requires grounded feet and no vehicle',
  'center_work_stand needs clear scaffold feet and head cells',
  'center_work_stand scaffold deck is stable but off-center; retain it and choose another platform',
  'center_work_stand scaffold deck did not settle; retain the current platform',
  'center_work_stand is not settled on the verified scaffold deck',
  'center_work_stand cannot stand on an airborne or off-cell scaffold position',
  'center_work_stand cannot stand on a scaffold climb cell as a work deck; feet or head cell is occupied',
  'center_work_stand cannot center this scaffold deck safely; choose another platform',
  'center_work_stand scaffold support changed; retain the current platform'
]

const knownCenterRefusal = error => {
  const message = String(error?.message ?? error)
  return CENTER_REFUSALS.find(refusal => message === refusal || message.endsWith(`: ${refusal}`))
}

export function handleCenterWorkRefusal (api, error, report, context = 'scaffold work platform is still settling') {
  const refusal = knownCenterRefusal(error)
  if (!refusal) return false
  api.acknowledgeFailure?.('center_work_stand')
  report.attention ??= []
  report.attention.push(`${context} (${refusal}); verified supports retained`)
  return true
}

export async function cleanupScaffoldRecoverably (api, cleanupScaffold, id, report) {
  try {
    return await cleanupScaffold(api, id, report)
  } catch (error) {
    if (!handleCenterWorkRefusal(api, error, report, 'scaffold cleanup could not confirm a settled work platform')) throw error
    const record = api.scaffolds?.(id)
    const material = record?.kind === 'pillar' ? null : 'scaffolding'
    report.cleanup_left = (record?.cells ?? []).filter(p => {
      const block = api.block(p.x, p.y, p.z)
      return !block || block.name === (material ?? p.item)
    }).map(p => ({ ...p }))
    api.report?.(report)
    return { cleanup_left: report.cleanup_left, attention: report.attention }
  }
}
