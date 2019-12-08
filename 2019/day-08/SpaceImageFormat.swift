// https://adventofcode.com/2019/day/8

import Foundation

let width = 25
let height = 6

func loadImage() -> [[Int]] {
  let filePath = Bundle.main.path(forResource: "input", ofType: "")!
  let input = try! String(contentsOfFile: filePath)
  let numbers = input.map(String.init).compactMap(Int.init)
  let chunkSize = width * height
  let chunks: [[Int]] = stride(from: 0, to: numbers.count, by: chunkSize).map {
    Array(numbers[$0 ..< Swift.min($0 + chunkSize, numbers.count)])
  }
  return chunks
}

// Part 1
func occurences(of digit: Int, in layer: [Int]) -> Int {
  return layer.filter { $0 == digit }.count
}

func imageChecksum(image: [[Int]]) -> Int {
  let layerWithFewestZeros = image
    .sorted(by: { occurences(of: 0, in: $0) > occurences(of: 0, in: $1) })
    .last!
  let numberOfOneDigits = occurences(of: 1, in: layerWithFewestZeros)
  let numberOfOwoDigits = occurences(of: 2, in: layerWithFewestZeros)

  return numberOfOneDigits * numberOfOwoDigits
}

print(imageChecksum(image: loadImage()))

// Part 2
enum Pixel: Int, CustomStringConvertible {
  case black, white, transparent

  func stack(with pixel: Pixel) -> Pixel {
    switch self {
    case .black:
      return self
    case .white:
      return self
    case .transparent:
      return pixel
    }
  }

  func render() -> String {
    return description
  }

  var description: String {
    get {
      switch self {
      case .black:
        return "⬛️"
      case .white:
        return "⬜️"
      case .transparent:
        return "🀆"
      }
    }
  }
}


func pixelsTransposition(of image: [[Int]]) -> [[Pixel]] {
  return image.first!.indices
    .map { layer in image.map { $0[layer] }.map { Pixel(rawValue: $0)! } }
}

func renderImage(of pixels: [[Pixel]]) -> [[String]] {
  let image = pixels.map { $0.reduce(Pixel.transparent, { $0.stack(with: $1) }) }
  let rows: [[String]] = stride(from: 0, to: image.count, by: width).map {
    Array(image[$0 ..< Swift.min($0 + width, image.count)]).map { $0.render() }
  }

  return rows
}

for row in renderImage(of: pixelsTransposition(of: loadImage())) {
  print(row.joined())
}
