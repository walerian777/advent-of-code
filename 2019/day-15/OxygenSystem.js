const fs = require('fs');

const execute = (input) => {
  const memory = [...input]
  const output = []
  const map = {}
  const visited = {}

  let direction = 0
  let x = 0
  let y = 0

  let instructionPointer = 0
  let relativeBase = 0

  const fetchValue = (pointer) => memory[pointer] || 0

  const fetchParam = (pointer, mode) => {
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

  const generateDirection = () => Math.floor(Math.random() * 4) + 1

  const generateKey = (x, y) => String([x, y])

  const calculateVector = (direction) => {
    switch (direction) {
      case 1:
        return [0, -1]
      case 2:
        return [0, 1]
      case 3:
        return [-1, 0]
      case 4:
        return [1, 0]
      default:
        throw new Error('Unknown direction!')
    }
  }

  const calculateDistance = (x, y) => {
    visited[generateKey(x, y)] = true;

    for (let direction = 1; direction <= 4; direction++) {
      const [dx, dy] = calculateVector(direction)
      const key = generateKey(x + dx, y + dy)

      if (map[key] == 2) {
        return 1
      } else if (!visited[key] && map[key] == 1) {
        let distance = calculateDistance(x + dx, y + dy)
;
        if (distance) {
          return distance + 1
        }
      }
    }

    return null
  }

  while (instructionPointer < memory.length) {
    const instruction = memory[instructionPointer]

    if (output.length > 0) {
      const status = output.shift()
      const [dx, dy] = calculateVector(direction)

      const key = generateKey(x + dx, y + dy)
      map[key] = status

      if (status == 1) {
        y += dy
        x += dx
      } else if (status == 2) {
        y += dy
        x += dx
        return calculateDistance(0, 0)
      }

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
        direction = generateDirection()
        memory[firstParam] = direction
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
        return
      default:
        throw new Error('Unknown opcode!')
    }
  }
}

const read = fs.readFileSync('input');
const input = read.toString().split(',').map(Number)

console.log(execute(input))
