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
