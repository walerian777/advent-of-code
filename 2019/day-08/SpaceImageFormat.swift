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
