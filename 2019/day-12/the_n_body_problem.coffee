# https://adventofcode.com/2019/day/12

INPUT = [
  [17, -7, -11]
  [1, 4, -1]
  [6, -2, -6]
  [19, 11, 9]
]

STEPS = 1000

GCD = (a, b) ->
  if a == 0 then b else GCD(b % a, a)

LCM = (a, b) ->
  (a * b) / GCD(a, b)

class Moon
  @ID = 0

  constructor: (x, y, z) ->
    @id = Moon.ID += 1
    @position = [x, y, z]
    @velocity = [0, 0, 0]

  move: ->
    @position[index] += @velocity[index] for _value, index in @position

  potentialEnergy: ->
    @position.reduce(((acc, value) -> acc + Math.abs(value)), 0)

  kineticEnergy: ->
    @velocity.reduce(((acc, value) -> acc + Math.abs(value)), 0)

  energy: ->
    @kineticEnergy() * @potentialEnergy()

  equals: (otherMoon) ->
    @id == otherMoon.id

  @motion: (moons) ->
    for moon in moons
      for otherMoon in moons
        continue if moon.equals(otherMoon)

        for _value, index in moon.position
          moon.velocity[index] += 1 if moon.position[index] < otherMoon.position[index]
          moon.velocity[index] -= 1 if moon.position[index] > otherMoon.position[index]

    for moon in moons
      moon.move()

part1 = ->
  moons = INPUT.map (position) -> new Moon(...position)
  step = 0

  while step < STEPS
    Moon.motion(moons)
    step += 1

  moons.reduce(((acc, moon) -> acc + moon.energy()), 0)

part2 = ->
  moons = INPUT.map (position) -> new Moon(...position)
  step = 0
  visited = [new Map(), new Map(), new Map()]
  repeated = [0, 0, 0]
  lcm = 1

  while repeated.reduce(((acc, value) -> acc + value), 0) < 3
    Moon.motion(moons)

    key = [[], [], []]
    for moon in moons
      for _value, index in moon.position
        key[index].push(moon.position[index])
        key[index].push(moon.velocity[index])

    for _value, index in key
      hashKey = String(key[index])
      if repeated[index] == 0 && visited[index].has(hashKey)
        repeated[index] += 1
        lcm = LCM(step, lcm)

      visited[index].set(hashKey, step)

    step += 1

  lcm

console.log(part1())
console.log(part2())
