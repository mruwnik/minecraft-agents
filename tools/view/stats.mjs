// Pixel statistics over a rectangle {x0, y0, x1, y1} (inclusive) of a decoded image {width, height, rgba}.
export const luminance = ([r, g, b]) => 0.2126 * r + 0.7152 * g + 0.0722 * b

const pixelsOf = ({ width, rgba }, { x0, y0, x1, y1 }) => {
  const pixels = []
  for (let y = y0; y <= y1; y++) {
    for (let x = x0; x <= x1; x++) pixels.push([rgba[(y * width + x) * 4], rgba[(y * width + x) * 4 + 1], rgba[(y * width + x) * 4 + 2]])
  }
  return pixels
}

// mean per channel, std (the mean of the per-channel standard deviations), fraction(predicate over [r, g, b])
export const regionStats = (image, region) => {
  const pixels = pixelsOf(image, region)
  const n = pixels.length
  const mean = [0, 1, 2].map(c => pixels.reduce((sum, p) => sum + p[c], 0) / n)
  const std = [0, 1, 2].map(c => Math.sqrt(pixels.reduce((sum, p) => sum + (p[c] - mean[c]) ** 2, 0) / n)).reduce((a, b) => a + b, 0) / 3
  return { n, mean, std, fraction: predicate => pixels.filter(predicate).length / n }
}
