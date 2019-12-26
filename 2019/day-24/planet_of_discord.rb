# frozen_string_literal: true

require 'set'

class BugPlanet
  attr_reader :layout, :length, :width

  BUG = '#'
  SPACE = '.'
  MOVS = [
    [-1, 0],
    [0, 1],
    [1, 0],
    [0, -1]
  ].freeze

  def initialize(initial_layout:)
    @layout = initial_layout
    @length = @layout.length
    @width = @layout.first.length
  end

  def live
    next_layout = empty_layout

    length.times do |row|
      width.times do |column|
        bugs = 0

        MOVS.each do |dx, dy|
          x = row + dx
          y = column + dy
          bugs += 1 if valid_tile?(x, y) && bug_tile?(x, y)
        end

        if (bug_tile?(row, column) && bugs != 1) || (space_tile?(row, column) && ![1, 2].include?(bugs))
          next_layout[row][column] = SPACE
        else
          next_layout[row][column] = BUG
        end
      end
    end

    self.class.new(initial_layout: next_layout)
  end

  def biodiversity_rating
    rating = 0
    power = 1

    length.times do |row|
      width.times do |column|
        rating += power if bug_tile?(row, column)
        power *= 2
      end
    end

    rating
  end

  private

  def valid_tile?(x, y)
    (0...length).include?(x) && (0...width).include?(y)
  end

  def bug_tile?(x, y)
    layout[x][y] == BUG
  end

  def space_tile?(x, y)
    layout[x][y] == SPACE
  end

  def empty_layout
    Array.new(length) { Array.new(width, SPACE) }
  end
end

class PlutonianBugPlanet < BugPlanet
  LEVEL = 100

  def initialize(*args)
    super(*args)
    prepare_map
  end

  def live
    next_map = Hash.new { |hash, key| hash[key] = SPACE }

    @map.each do |position, _|
      bugs = 0

      @adjacent_tiles[position].each do |adjacent|
        bugs += 1 if @map[adjacent] == BUG
      end

      if (@map[position] == BUG && bugs != 1) || (@map[position] == SPACE && ![1, 2].include?(bugs))
        next_map[position] = SPACE
      else
        next_map[position] = BUG
      end
    end

    @map = next_map
  end

  def bugs_count
    @map.reduce(0) { |acc, (_, tile)| acc + (tile == BUG ? 1 : 0) }
  end

  private

  def prepare_map
    @map = Hash.new { |hash, key| hash[key] = SPACE }
    @adjacent_tiles = Hash.new { |hash, key| hash[key] = [] }

    length.times do |row|
      width.times do |column|
        next if center_tile?(row, column)

        (-LEVEL..LEVEL).each do |level|
          @map[[row, column, level]] = level.zero? && bug_tile?(row, column) ? BUG : SPACE

          MOVS.each do |dx, dy|
            x = row + dx
            y = column + dy
            if valid_tile?(x, y) && !center_tile?(x, y)
              @adjacent_tiles[[row, column, level]].push([x, y, level])
            end
          end

          if row.zero? && level - 1 >= -LEVEL
            @adjacent_tiles[[row, column, level]].push([1, 2, level - 1])
          end
          if column.zero? && level - 1 >= -LEVEL
            @adjacent_tiles[[row, column, level]].push([2, 1, level - 1])
          end
          if column == @width - 1 && level - 1 >= -LEVEL
            @adjacent_tiles[[row, column, level]].push([2, 3, level - 1])
          end
          if row == @length - 1 && level - 1 >= -LEVEL
            @adjacent_tiles[[row, column, level]].push([3, 2, level - 1])
          end
          if row == 1 && column == 2 && level + 1 <= LEVEL
            5.times do |y|
              @adjacent_tiles[[row, column, level]].push([0, y, level + 1])
            end
          end
          if row == 2 && column == 1 && level + 1 <= LEVEL
            5.times do |x|
              @adjacent_tiles[[row, column, level]].push([x, 0, level + 1])
            end
          end
          if row == 2 && column == 3 && level + 1 <= LEVEL
            5.times do |x|
              @adjacent_tiles[[row, column, level]].push([x, @width - 1, level + 1])
            end
          end
          if row == 3 && column == 2 && level + 1 <= LEVEL
            5.times do |y|
              @adjacent_tiles[[row, column, level]].push([@length - 1, y, level + 1])
            end
          end
        end
      end
    end
  end

  def center_tile?(x, y)
    x == 2 && y == 2
  end
end

INPUT = File.read('input').split("\n").map { |line| line.split('') }

def doubled_layout_rating(layout)
  calculated = Set.new
  planet = BugPlanet.new(initial_layout: layout)

  loop do
    rating = planet.biodiversity_rating
    return rating if calculated.include?(rating)

    calculated.add(rating)
    planet = planet.live
  end
end

puts(doubled_layout_rating(INPUT))

def bugs_after_200_minutes(layout)
  planet = PlutonianBugPlanet.new(initial_layout: layout)
  200.times { planet.live }

  planet.bugs_count
end

puts(bugs_after_200_minutes(INPUT))
