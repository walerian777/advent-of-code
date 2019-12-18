# https://adventofcode.com/2019/day/18

class Tile
  ENTRANCE = '@'
  PASSAGE = '.'
  WALL = '#'

  getter :symbol

  def initialize(symbol : Char)
    @symbol = symbol
  end

  def entrance?
    @symbol == ENTRANCE
  end

  def passage?
    @symbol == PASSAGE
  end

  def wall?
    @symbol == WALL
  end

  def key?
    ('a'..'z').includes?(@symbol)
  end

  def door?
    ('A'..'Z').includes?(@symbol)
  end

  def to_s
    @symbol
  end

  def <=>(other)
    @symbol <=> other.symbol
  end
end

class Collector
  getter :row, :column, :steps, :keys

  def initialize(row : Int32, column : Int32, keys = Set(Tile).new, steps = 0)
    @row = row
    @column = column
    @steps = steps
    @keys = keys
  end

  def can_open?(tile : Tile)
    @keys.map { |key| key.symbol }.includes?(tile.symbol.downcase)
  end
end

class PathFinder
  def initialize(map : Array(Array(Tile)))
    @map = map
    @rows = Int32.new(map.size)
    @columns = Int32.new(map.first.size)
    @keys = Set(Tile).new
    @to_visit = [] of Collector
    scan_map
  end

  def shortest_path
    visited = Set(Tuple(Int32, Int32, String)).new

    while !@to_visit.empty?
      collector = @to_visit.shift()
      state = {collector.row, collector.column, collector.keys.map { |x| x.symbol }.sort.join}
      next if visited.includes?(state)

      visited.add(state)
      tile = @map[collector.row][collector.column]

      next if tile.wall?
      next if invalid_position?(collector)
      next if tile.door? && !collector.can_open?(tile)

      collected_keys = Set.new(collector.keys)

      if tile.key?
        collected_keys.add(tile)
        return collector.steps if @keys.size == collected_keys.size
      end

      [{-1, 0}, {0, 1}, {1, 0}, {0, -1}].each do |x, y|
        row = collector.row + x
        column = collector.column + y
        next_collector = Collector.new(row, column, collected_keys, collector.steps + 1)
        @to_visit.push(next_collector)
      end
    end
  end

  private def scan_map
    @rows.times do |row|
      @columns.times do |column|
        tile = @map[row][column]
        if tile.entrance?
          collector = Collector.new(row, column)
          @to_visit.push(collector)
        elsif tile.key?
          @keys.add(tile)
        end
      end
    end
  end

  private def invalid_position?(collector)
    !(0 <= collector.row < @rows && 0 <= collector.column < @columns)
  end
end

MAP = File.read("input").strip().split("\n").map do |line|
  line.split("").map { |symbol| Tile.new(symbol.chars.first) }
end

finder = PathFinder.new(MAP)
puts finder.shortest_path
