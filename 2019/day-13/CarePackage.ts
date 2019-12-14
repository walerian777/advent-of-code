const fs = require('fs');

function Tile(x, y, id) {
  this.id = id
  this.x = x
  this.y = y

  this.move = (x, y) => {
    this.x = x
    this.y = y
  }
}

type Tile = {
  id: number,
  x: number,
  y: number,
  move: (x: number, y: number) => void
}

type Tally = {
  blocks: number,
  score: number
}

const execute = (input: number[], quarters?: number): Tally => {
  const memory: number[] = [...input]
  const output: number[] = []
  const tiles: Tile[] = []

  let instructionPointer = 0
  let relativeBase = 0
  let score = 0

  const fetchValue = (pointer: number): number => memory[pointer] || 0

  const fetchParam = (pointer: number, mode: number): number => {
    switch(mode) {
      case 0:
        return memory[pointer]
      case 1:
        return pointer
      case 2:
        return relativeBase + memory[pointer]
      default:
        throw new Error('Unknown mode!')
    }
  }

  // Memory address 0 represents the number of quarters that have been inserted;
  // set it to 2 to play for free
  if (quarters) {
    memory[0] = quarters
  }

  while (instructionPointer < memory.length) {
    const instruction = memory[instructionPointer]

    if (output.length == 3) {
      if (output[0] == -1 && output[1] == 0) {
        score = output[2]
      } else {
        const tile = tiles.find((tile) => tile.x == output[0] && tile.y == output[1])
        if (tile == undefined) {
          const newTile = new Tile(output[0], output[1], output[2])
          tiles.push(newTile)
        } else {
          tile.id = output[2]
        }

        if (output[2] == 4) {
          const ball = tiles.find((tile) => tile.id == 4)
          ball.move(output[0], output[1])
        } else if (output[2] == 3) {
          const pad = tiles.find((tile) => tile.id == 3)
          pad.move(output[0], output[1])
        }
      }
      output.splice(0, 3)
    }

    const opcode = instruction % 100;
    const firstParamMode = Math.round(instruction / 100 % 10)
    const secondParamMode = Math.round(instruction / 1000 % 10)
    const thirdParamMode = Math.round(instruction / 10000 % 10)

    const firstParam = fetchParam(instructionPointer + 1, firstParamMode)
    const secondParam = fetchParam(instructionPointer + 2, secondParamMode)
    const thirdParam = fetchParam(instructionPointer + 3, thirdParamMode)

    switch (opcode) {
      case 1:
        memory[thirdParam] = fetchValue(firstParam) + fetchValue(secondParam)
        instructionPointer += 4
        break;
      case 2:
        memory[thirdParam] = fetchValue(firstParam) * fetchValue(secondParam)
        instructionPointer += 4
        break;
      case 3:
        const ball = tiles.find((tile) => tile.id == 4)
        const pad = tiles.find((tile) => tile.id == 3)
        memory[firstParam] = ball.x > pad.x ? 1 : ball.x < pad.x ? -1 : 0
        instructionPointer += 2
        break;
      case 4:
        output.push(memory[firstParam])
        instructionPointer += 2
        break;
      case 5:
        if (fetchValue(firstParam) != 0) {
          instructionPointer = fetchValue(secondParam)
        } else {
          instructionPointer += 3
        }
        break;
      case 6:
        if (fetchValue(firstParam) == 0) {
          instructionPointer = fetchValue(secondParam)
        } else {
          instructionPointer += 3
        }
        break;
      case 7:
        memory[thirdParam] = fetchValue(firstParam) < fetchValue(secondParam) ? 1 : 0
        instructionPointer += 4
        break;
      case 8:
        memory[thirdParam] = fetchValue(firstParam) == fetchValue(secondParam) ? 1 : 0
        instructionPointer += 4
        break;
      case 9:
        relativeBase += fetchValue(firstParam)
        instructionPointer += 2
        break;
      case 99:
        instructionPointer = memory.length
        const blocks = tiles.filter((tile) => tile.id == 2).length
        return {blocks, score}
      default:
        throw new Error('Unknown opcode!')
    }
  }
}

const read = fs.readFileSync('input');
const input = read.toString().split(',').map(Number)

console.log(execute(input).blocks)
console.log(execute(input, 2).score)
