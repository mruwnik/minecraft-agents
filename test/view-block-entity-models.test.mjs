// The block entities the game draws with Java model code (chests, beds, signs, banners, heads, shulker boxes, pots, the bell) as baked
// elements in a static pose: shapes, facing and rotation, sheet-region textures.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { blockEntityElements, DYE_COLORS } from '../tools/view/block-entity-models.mjs'
import { decodePng } from '../tools/view/renderer.mjs'
import { openJar, findClientJar } from '../tools/view/jar-read.mjs'

const SOUTH = { north: 'south', south: 'north', east: 'west', west: 'east' }
const FACINGS = ['north', 'east', 'south', 'west']
const get = (name, props = {}) => blockEntityElements(name, props)
const sheetOf = texture => texture.split('#')[0]
const regionOf = texture => texture.split('#')[1].split('@')[0].split(',').map(Number)
const size = element => element.to.map((v, i) => v - element.from[i])
const faceTextures = elements => elements.flatMap(e => Object.values(e.faces).map(f => f.texture))
const faceOf = (element, texture) => Object.entries(element.faces).find(([, f]) => f.texture.startsWith(texture))?.[0]

test('names out of scope are null', () => {
  for (const name of ['stone', 'piston_head', 'oak_planks', 'oak_fence', 'barrel', 'lectern', 'player_wall_head_x', 'chest_minecart'])
    assert.equal(get(name), null, name)
})

test('every element is in the baked format, inside the voxel, with full-layer uvs', () => {
  const cases = [
    ['chest', { facing: 'east', type: 'left' }], ['oak_sign', { rotation: 3 }], ['oak_wall_hanging_sign', { facing: 'west' }],
    ['red_bed', { part: 'head', facing: 'south' }], ['red_banner', { rotation: 5 }], ['skeleton_skull', { rotation: 9 }],
    ['purple_shulker_box', { facing: 'down' }], ['decorated_pot', { facing: 'north' }], ['oak_hanging_sign', { attached: false, rotation: 2 }]
  ]
  for (const [name, props] of cases) {
    const elements = get(name, props)
    assert.ok(elements.length > 0, name)
    for (const e of elements) {
      assert.deepEqual(Object.keys(e).sort(), ['faces', 'from', 'rotation', 'shade', 'to'], name)
      assert.ok(e.from.every((v, i) => v >= 0 && e.to[i] <= 16 && v <= e.to[i]), `${name} ${e.from} ${e.to}`)
      for (const f of Object.values(e.faces)) {
        assert.equal(f.uv[1], 0, name)
        assert.ok(f.texture === 'iron_chain' || f.uv.join() === '0,0,16,16', name)
        assert.equal(f.tintindex, -1)
        assert.equal(f.cullface, null)
        assert.ok(f.texture === 'iron_chain' || /^entity\/[\w/]+#\d+,\d+,\d+,\d+(@[0-9a-f]{6})?$/.test(f.texture), f.texture)
      }
    }
  }
})

// ---- chests ----

const chestParts = elements => ({ latch: elements.find(e => size(e)[0] === 2 || size(e)[2] === 2 || size(e)[0] === 1 || size(e)[2] === 1), body: elements.find(e => size(e)[1] === 9), lid: elements.find(e => size(e)[1] === 5) })

test('a single chest: 14 wide, body at 0..9, a 5 high lid on top', () => {
  const { body, lid } = chestParts(get('chest', { facing: 'north', type: 'single' }))
  assert.deepEqual([body.from, body.to], [[1, 0, 1], [15, 9, 15]])
  assert.deepEqual([lid.from, lid.to], [[1, 9, 1], [15, 14, 15]])
})

for (const [facing, axis, at] of [['north', 2, 0], ['south', 2, 15], ['west', 0, 0], ['east', 0, 15]]) {
  test(`the latch is on the ${facing} side of a chest facing ${facing}`, () => {
    const elements = get('chest', { facing, type: 'single' })
    const latch = elements.find(e => e.faces && Object.values(e.faces).some(f => regionOf(f.texture).join() === '1,1,2,4'))
    assert.equal(latch.from[axis], at)
    assert.equal(latch.to[axis], at + 1)
    assert.ok(faceOf(latch, 'entity/chest/normal#1,1,2,4') === facing)
  })
}

for (const [name, sheet] of [['chest', 'normal'], ['trapped_chest', 'trapped'], ['ender_chest', 'ender'], ['copper_chest', 'copper'], ['waxed_oxidized_copper_chest', 'copper_oxidized'], ['exposed_copper_chest', 'copper_exposed'], ['weathered_copper_chest', 'copper_weathered']]) {
  test(`${name} reads the ${sheet} sheet`, () => {
    const sheets = new Set(faceTextures(get(name, { facing: 'north', type: 'single' })).map(sheetOf))
    assert.deepEqual([...sheets], [`entity/chest/${sheet}`])
  })
}

for (const [facing, axis] of [['north', 0], ['east', 2], ['south', 0], ['west', 2]]) {
  test(`double chest halves facing ${facing} join: the left half reaches the clockwise side, the right half the other`, () => {
    const extent = type => {
      const lid = get('chest', { facing, type }).find(e => size(e)[1] === 5)
      return [lid.from[axis], lid.to[axis]]
    }
    const [left, right] = [extent('left'), extent('right')]
    const flipped = facing === 'south' || facing === 'west'
    assert.deepEqual(left.map(v => flipped ? 16 - v : v).sort((a, b) => a - b), [1, 16])
    assert.deepEqual(right.map(v => flipped ? 16 - v : v).sort((a, b) => a - b), [0, 15])
  })
}

test('double halves read the left and right sheets', () => {
  assert.deepEqual([...new Set(faceTextures(get('chest', { facing: 'north', type: 'left' })).map(sheetOf))], ['entity/chest/normal_left'])
  assert.deepEqual([...new Set(faceTextures(get('trapped_chest', { facing: 'north', type: 'right' })).map(sheetOf))], ['entity/chest/trapped_right'])
})

// ---- beds ----

test('a bed is a 16x16 slab at 3..9 with a pillow on the head and legs at the outer corners', () => {
  const head = get('red_bed', { part: 'head', facing: 'north' })
  const foot = get('red_bed', { part: 'foot', facing: 'north' })
  const slab = elements => elements.find(e => size(e)[0] === 16)
  assert.deepEqual([slab(head).from, slab(head).to], [[0, 3, 0], [16, 9, 16]])
  assert.deepEqual(slab(head).faces.up.texture, 'entity/bed/red#6,6,16,16')
  assert.deepEqual(slab(foot).faces.up.texture, 'entity/bed/red#6,28,16,16')
  const legs = elements => elements.filter(e => size(e)[0] === 3)
  assert.equal(legs(head).length, 2)
  assert.ok(legs(head).every(e => e.from[1] === 0 && e.to[1] === 3))
  assert.ok(legs(head).every(e => e.from[2] === 0), 'head legs at the head (north) end')
  assert.ok(legs(foot).every(e => e.to[2] === 16), 'foot legs at the south end')
})

for (const [facing, dir] of [['north', 'north'], ['east', 'east'], ['south', 'south'], ['west', 'west']]) {
  test(`the head end of a bed facing ${facing} has its board on the ${dir} side`, () => {
    const slab = get('blue_bed', { part: 'head', facing }).find(e => size(e)[0] === 16)
    assert.equal(faceOf(slab, 'entity/bed/blue#6,0,16,6'), dir)
  })
}

// ---- signs ----

for (const [rotation, front] of [[0, 'south'], [4, 'west'], [8, 'north'], [12, 'east']]) {
  test(`a standing sign at rotation ${rotation} faces ${front}`, () => {
    const board = get('oak_sign', { rotation }).find(e => size(e)[0] > 4 || size(e)[2] > 4)
    assert.equal(faceOf(board, 'entity/signs/oak#2,2,24,12'), front)
    assert.equal(board.rotation, null)
  })
}

test('a standing sign: the board is above the post, 16 wide and 8 high', () => {
  const [post, board] = get('oak_sign', { rotation: 0 }).sort((a, b) => a.from[1] - b.from[1])
  assert.ok(post.to[1] <= board.from[1])
  assert.ok(size(post)[0] < 3 && size(post)[2] < 3)
  assert.deepEqual([size(board)[0], size(board)[1]], [16, 8])
  assert.ok(post.from[0] > board.from[0] && post.to[0] < board.to[0], 'background shows between the post and the board edge')
})

for (const [rotation, front, angle] of [[1, 'south', -22.5], [2, 'west', 45], [3, 'west', 22.5], [5, 'west', -22.5]]) {
  test(`a standing sign at rotation ${rotation} faces ${front} turned ${angle} degrees about y`, () => {
    const board = get('oak_sign', { rotation }).find(e => size(e)[0] > 4 || size(e)[2] > 4)
    assert.deepEqual([board.rotation.axis, board.rotation.angle, board.rotation.origin], ['y', angle, [8, 8, 8]])
    assert.equal(faceOf(board, 'entity/signs/oak#2,2,24,12'), front)
  })
}

for (const facing of FACINGS) {
  test(`a wall sign facing ${facing} has its board on the ${SOUTH[facing]} wall side, facing ${facing}`, () => {
    const [board] = get('birch_wall_sign', { facing })
    assert.equal(faceOf(board, 'entity/signs/birch#2,2,24,12'), facing)
    const axis = facing === 'north' || facing === 'south' ? 2 : 0
    const wallSide = facing === 'north' || facing === 'west' ? 16 : 0
    assert.ok(Math.abs((board.from[axis] + board.to[axis]) / 2 - wallSide) < 1.5)
  })
}

test('hanging signs: the board on chains, the wall one under a plank', () => {
  const ceiling = get('oak_hanging_sign', { attached: true, rotation: 0 })
  const wall = get('oak_wall_hanging_sign', { facing: 'north' })
  for (const elements of [ceiling, wall]) assert.ok(elements.some(e => Object.values(e.faces).some(f => f.texture.startsWith('entity/signs/hanging/oak#2,14,14,10'))))
  assert.ok(wall.some(e => size(e)[0] === 16 && size(e)[1] === 2 && size(e)[2] === 4))
  assert.ok(faceTextures(ceiling).includes('iron_chain'))
})

test('a hanging sign with and without an attached chain pair differs', () => {
  assert.notDeepEqual(get('oak_hanging_sign', { attached: true, rotation: 0 }), get('oak_hanging_sign', { attached: false, rotation: 0 }))
})

// ---- banners ----

test('dye colours: sixteen, as colours', () => {
  assert.equal(Object.keys(DYE_COLORS).length, 16)
  assert.ok(Object.values(DYE_COLORS).every(c => /^[0-9a-f]{6}$/.test(c)))
})

test('a banner: a pole, a bar and a cloth tinted with the dye colour', () => {
  for (const [name, hex] of [['red_banner', 'b02e26'], ['white_banner', 'f9fffe'], ['lime_wall_banner', '80c71f']]) {
    const elements = get(name, { rotation: 0, facing: 'north' })
    const cloth = elements.filter(e => Object.values(e.faces).some(f => f.texture.includes(`@${hex}`)))
    assert.equal(cloth.length, 1, name)
    assert.ok(Object.values(cloth[0].faces).every(f => f.texture.startsWith('entity/banner/base#')))
    assert.ok(faceTextures(elements).some(t => t.startsWith('entity/banner/banner_base#')), 'pole and bar are not tinted')
  }
})

test('a standing banner has a pole under the bar; a wall banner has none', () => {
  assert.ok(get('red_banner', { rotation: 0 }).some(e => size(e)[1] > 14 && size(e)[0] < 3))
  assert.ok(!get('red_wall_banner', { facing: 'north' }).some(e => size(e)[1] > 14))
})

// ---- heads ----

const HEADS = [['skeleton_skull', 'entity/skeleton/skeleton'], ['wither_skeleton_skull', 'entity/skeleton/wither_skeleton'], ['zombie_head', 'entity/zombie/zombie'], ['player_head', 'entity/player/wide/steve'], ['creeper_head', 'entity/creeper/creeper'], ['piglin_head', 'entity/piglin/piglin']]
for (const [name, sheet] of HEADS) {
  test(`${name} is an 8 high head cube on the ground, textured from ${sheet}`, () => {
    const [head, ...rest] = get(name, { rotation: 0 })
    assert.equal(rest.length, 0)
    assert.equal(size(head)[1], 8)
    assert.equal(size(head)[2], 8)
    assert.ok(size(head)[0] === 8 || size(head)[0] === 10)
    assert.equal(head.from[1], 0)
    assert.deepEqual([...new Set(faceTextures([head]).map(sheetOf))], [sheet])
  })
  test(`${name}: the wall variant sits against the wall it is on`, () => {
    const wall = name.replace(/_(skull|head)$/, '_wall_$1')
    const [head] = get(wall.replace('skeleton_wall_skull', 'skeleton_wall_skull'), { facing: 'north' })
    assert.equal(head.to[2], 16)
    assert.equal(head.from[1], 4)
  })
}

test('a skull face is on the side the head looks at', () => {
  for (const [rotation, front] of [[0, 'south'], [4, 'west'], [8, 'north'], [12, 'east']]) {
    const [head] = get('zombie_head', { rotation })
    assert.equal(faceOf(head, 'entity/zombie/zombie#8,8,8,8'), front)
  }
})

test('the dragon head is two boxes (upper head and jaw)', () => {
  const elements = get('dragon_head', { rotation: 0 })
  assert.equal(elements.length, 2)
  assert.ok(faceTextures(elements).every(t => t.startsWith('entity/enderdragon/dragon#')))
  assert.equal(get('dragon_wall_head', { facing: 'east' }).length, 2)
})

// ---- shulker boxes, pots, bell ----

test('a shulker box: base and a lid over it, on the facing side', () => {
  const [base, lid] = get('shulker_box', { facing: 'up' }).sort((a, b) => a.from[1] - b.from[1])
  assert.equal(base.from[1], 0)
  assert.equal(lid.to[1], 16)
  assert.ok(lid.from[1] >= base.to[1] - 0.001 || lid.from[1] === 4)
  const sheet = new Set(faceTextures([base, lid]).map(sheetOf))
  assert.deepEqual([...sheet], ['entity/shulker/shulker'])
})

for (const [facing, axis, high] of [['down', 1, false], ['north', 2, false], ['south', 2, true], ['east', 0, true], ['west', 0, false]]) {
  test(`a shulker box facing ${facing} has its lid on that side`, () => {
    const elements = get('green_shulker_box', { facing })
    const lid = elements.find(e => faceTextures([e]).some(t => t.includes('#16,0,16,16')))
    assert.equal(high ? lid.to[axis] : lid.from[axis], high ? 16 : 0)
    assert.ok(faceTextures(elements).every(t => t.startsWith('entity/shulker/shulker_green#')))
  })
}

test('a decorated pot has a body and a narrower neck', () => {
  const [body, neck] = get('decorated_pot', { facing: 'north' }).sort((a, b) => a.from[1] - b.from[1])
  assert.ok(size(neck)[0] < size(body)[0])
  assert.equal(neck.to[1], 16)
  assert.equal(body.from[1], 0)
})

test('the bell adds its body, centred, to the jar model', () => {
  const elements = get('bell', { attachment: 'floor', facing: 'north' })
  const [lip, body] = elements.sort((a, b) => a.from[1] - b.from[1])
  assert.deepEqual(size(body), [6, 7, 6])
  assert.equal(body.from[0], 5)
  assert.ok(size(lip)[0] > size(body)[0])
})

// ---- books: the lectern's (has_book) and the enchanting table's, added to the jar models ----

const BOOK_SHEET = 'entity/enchantment/enchanting_table_book'
const bookOf = (name, props) => get(name, props)
const COVER_REGIONS = ['0,0,6,10', '6,0,6,10', '16,0,6,10', '22,0,6,10']

test('a lectern without a book and other lecterns state-free get nothing; with a book, an open book', () => {
  assert.equal(get('lectern', { facing: 'north', has_book: false }), null)
  assert.ok(get('lectern', { facing: 'north', has_book: true }).length >= 4)
})

test('the enchanting table gets an open book floating above its 12 high base', () => {
  const elements = bookOf('enchanting_table', {})
  assert.ok(elements.length >= 4)
  for (const e of elements) {
    assert.ok(e.from[1] >= 12.5, `${e.from}`)
    assert.equal(e.rotation, null)
  }
  assert.ok(faceTextures(elements).every(t => t.startsWith(`${BOOK_SHEET}#`)))
})

for (const [name, props] of [['enchanting_table', {}], ['lectern', { facing: 'north', has_book: true }]]) {
  test(`${name}: the book's cover faces show the cover regions of the sheet, its pages the page regions`, () => {
    const regions = new Set(faceTextures(bookOf(name, props)).map(t => regionOf(t).join()))
    assert.ok(COVER_REGIONS.some(r => regions.has(r)), [...regions].join(' '))
    const pageRegions = [...regions].filter(r => r.split(',')[1] >= 10)
    assert.ok(pageRegions.length > 0, 'a page region (the sheet rows from 10 down)')
    assert.ok([...regions].every(r => r.split(',').map(Number)[0] + r.split(',').map(Number)[2] <= 64))
  })
}

test('the lectern book rests on the jar top: its underside centre, tilted, lands on the top surface centre (y 16 tilted about 8,8)', () => {
  const tilt = (y, z, [, oy, oz], angle) => {
    const [c, s] = [Math.cos(angle * Math.PI / 180), Math.sin(angle * Math.PI / 180)]
    return [(y - oy) * c - (z - oz) * s + oy, (y - oy) * s + (z - oz) * c + oz]
  }
  const cover = bookOf('lectern', { facing: 'north', has_book: true })[0]
  const [y, z] = tilt(cover.from[1], (cover.from[2] + cover.to[2]) / 2, cover.rotation.origin, cover.rotation.angle)
  const [topY, topZ] = tilt(16, 9.5, [8, 8, 8], -22.5)
  assert.ok(Math.abs(y - topY) < 1e-3 && Math.abs(z - topZ) < 1e-3, `${y},${z} vs ${topY},${topZ}`)
})

for (const facing of FACINGS) {
  test(`lectern book facing ${facing} lies on the top's 22.5 degree slope, inside the voxel`, () => {
    const elements = bookOf('lectern', { facing, has_book: true })
    for (const e of elements) {
      assert.equal(e.rotation.angle ** 2, 22.5 ** 2)
      assert.ok(e.from.every((v, i) => v >= 0 && e.to[i] <= 16), `${e.from} ${e.to}`)
    }
  })
}

test('the same state gives the same elements', () => {
  assert.deepEqual(get('chest', { facing: 'east', type: 'single', waterlogged: true }), get('chest', { facing: 'east', type: 'single', waterlogged: false }))
})


// ---- the right sheet region on the right face, judged from the sheet's pixels alone ----
// Entity sheets keep the model's inside regions too (black or a dark interior); a face must not show those.

const jarPath = findClientJar()
const jar = jarPath ? openJar(jarPath) : null
const pixelsOf = texture => {
  const [x, y, w, h] = regionOf(texture)
  const sheet = decodePng(jar.read(`assets/minecraft/textures/${sheetOf(texture)}.png`))
  const pixels = []
  for (let j = y; j < y + h; j++) for (let i = x; i < x + w; i++) pixels.push([...sheet.rgba.subarray((j * sheet.width + i) * 4, (j * sheet.width + i) * 4 + 3)])
  return pixels
}
const lumOf = texture => pixelsOf(texture).reduce((n, [r, g, b]) => n + 0.2126 * r + 0.7152 * g + 0.0722 * b, 0) / pixelsOf(texture).length
const faceTexture = (name, props, dir, pick = e => e) => pick(get(name, props)).faces[dir].texture
const skip = jar === null

test('chest: the lid top is not darker than the lid sides (the inside region is the dark one)', { skip }, () => {
  const elements = get('chest', { facing: 'north', type: 'single' })
  const lid = elements.find(e => size(e)[1] === 5)
  assert.ok(lumOf(lid.faces.up.texture) >= lumOf(lid.faces.north.texture) * 0.95, `${lid.faces.up.texture}`)
  assert.ok(lumOf(lid.faces.up.texture) > lumOf('entity/chest/normal#14,0,14,14') + 20, 'not the inside region')
})
test('double chest: the lid tops are not the dark inside region', { skip }, () => {
  for (const type of ['left', 'right']) {
    const lid = get('chest', { facing: 'north', type }).find(e => size(e)[1] === 5)
    assert.ok(lumOf(lid.faces.up.texture) > lumOf(lid.faces.north.texture) * 0.95, type)
  }
})
test('shulker box: the lid top shows the shell, not the black inside', { skip }, () => {
  const lid = get('red_shulker_box', { facing: 'up' }).find(e => e.from[1] === 4)
  assert.ok(lumOf(lid.faces.up.texture) > 40)
})
test('bed: the top shows the pillow and blanket, not the underside planks', { skip }, () => {
  const slab = get('red_bed', { part: 'head', facing: 'north' }).find(e => size(e)[0] === 16)
  const pixels = pixelsOf(slab.faces.up.texture)
  assert.ok(pixels.some(([r, g, b]) => r > 200 && g > 200 && b > 200), 'pillow white')
  assert.ok(pixels.some(([r, g, b]) => r > 150 && g < 60 && b < 60), 'blanket red')
})
test('skulls: the top of the head is not the black inside', { skip }, () => {
  for (const name of ['skeleton_skull', 'zombie_head', 'player_head', 'creeper_head', 'wither_skeleton_skull']) assert.ok(lumOf(faceTexture(name, { rotation: 0 }, 'up', e => e[0])) > 30, name)
})
test('bell: the body top and rim top are lit metal', { skip }, () => {
  const [lip, body] = get('bell', { attachment: 'floor', facing: 'north' }).sort((a, b) => a.from[1] - b.from[1])
  assert.ok(lumOf(body.faces.up.texture) > 100)
  assert.ok(lumOf(lip.faces.up.texture) > 60)
})
test('book: the covers are brown leather and the pages cream, never the sheet\'s black', { skip }, () => {
  const book = get('enchanting_table', {})
  const [left, right] = book.slice(0, 2)
  const brown = ([r, g, b]) => r > g && g > b && r > 100 && r < 270 && b < 80
  const cream = ([r, g, b]) => r > 180 && g > 170 && b > 120
  for (const cover of [left, right]) for (const dir of ['up', 'down']) assert.ok(pixelsOf(cover.faces[dir].texture).filter(brown).length >= 30, `${dir} ${cover.faces[dir].texture}`)
  for (const pages of book.slice(3)) assert.ok(pixelsOf(pages.faces.up.texture).filter(cream).length >= 36, pages.faces.up.texture)
})
test('pot: the neck band and the side tile have colour, not a transparent or black region', { skip }, () => {
  const [body, neck] = get('decorated_pot', { facing: 'north' }).sort((a, b) => a.from[1] - b.from[1])
  assert.ok(lumOf(neck.faces.north.texture) > 40)
  assert.ok(lumOf(body.faces.north.texture) > 40)
})
test('banner: the cloth is the near-white base sheet, the pole is wood', { skip }, () => {
  const elements = get('red_banner', { rotation: 0 })
  const cloth = elements.find(e => Object.values(e.faces).some(f => f.texture.includes('@')))
  assert.ok(lumOf(cloth.faces.north.texture) > 200)
  assert.ok(lumOf(elements.find(e => size(e)[1] > 14).faces.north.texture) < 120)
})
test('sign: the board front is the wood face, not the black inside', { skip }, () => {
  assert.ok(lumOf(faceTexture('oak_sign', { rotation: 0 }, 'north', e => e.find(b => size(b)[0] === 16))) > 60)
})
