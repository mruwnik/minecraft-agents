// Why JavaScript: patches prismarine-physics source in node_modules (Mineflayer boundary).
// prismarine-physics 1.11.1 index.js reads Depth Strider from the boots' nbt, which 1.20.5+ items (26.1 too) do not
// have (their enchantments are components), so the client never swam faster; and it moved the water acceleration
// toward 0.7, where vanilla (LivingEntity water travel) moves it toward the walking speed 0.1.
export const BOOTS_NBT = `    if (boots && boots.nbt) {
      const simplifiedNbt = nbt.simplify(boots.nbt)
      const enchantments = simplifiedNbt.Enchantments ?? simplifiedNbt.ench ?? []
      this.depthStrider = getEnchantmentLevel(mcData, 'depth_strider', enchantments)
    } else {
      this.depthStrider = 0
    }
`
export const BOOTS_ENCHANTS = `    // patched by bot/patch-deps.mjs: depth strider from the item's enchants (components on 1.20.5+)
    this.depthStrider = boots?.enchants?.find(e => e.name === 'depth_strider')?.lvl ?? 0
`
export const STRIDER_ACCEL = '          acceleration += (0.7 - acceleration) * strider / 3\n'
export const STRIDER_ACCEL_WALK = '          acceleration += (physics.playerSpeed - acceleration) * strider / 3 // patched by bot/patch-deps.mjs: vanilla walking speed\n'
export const patchDepthStrider = source => source.includes(BOOTS_ENCHANTS) && source.includes(STRIDER_ACCEL_WALK)
  ? { status: 'already', source }
  : source.includes(BOOTS_NBT) && source.includes(STRIDER_ACCEL)
    ? { status: 'patched', source: source.replace(BOOTS_NBT, BOOTS_ENCHANTS).replace(STRIDER_ACCEL, STRIDER_ACCEL_WALK) }
    : { status: 'anchor missing', source }
