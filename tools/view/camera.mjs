// Why JavaScript: graphics; converts pose files into the renderer's camera numbers.
// A pose file's numbers as the renderer wants them. The renderer takes mineflayer's own yaw and pitch in radians
// (directionFor: yaw 0 faces north, 90 degrees west, 180 south, 270 east; pitch > 0 looks up), which is what the old
// eyes.mjs passed it as bot.entity.yaw/pitch, so the conversion is the identity plus the eye.
export const cameraFromPose = pose => ({
  eye: { x: pose.eye.x, y: pose.eye.y, z: pose.eye.z },
  yaw: pose.yaw ?? 0,
  pitch: pose.pitch ?? 0
})

// a dropped item is a quarter of a block across, as the game's item entity is
const ITEM_SIZE = 0.25

// eyes.mjs's visibleEntities, from the pose's entity list
export const entitiesFromPose = pose => (pose.entities ?? []).filter(e => e.pos).map(e => ({
  name: e.name ?? e.type,
  label: e.username ?? e.name,
  kind: e.type === 'player' ? 'player' : e.type === 'hostile' || e.kind === 'Hostile mobs' ? 'hostile' : e.type,
  x: e.pos.x,
  y: e.pos.y,
  z: e.pos.z,
  ...(e.item ? { item: e.item } : {}),
  width: e.name === 'item' ? e.width || ITEM_SIZE : e.width || 0.6,
  height: e.name === 'item' ? e.height || ITEM_SIZE : e.height || 1.8,
  yaw: e.yaw ?? 0
}))
