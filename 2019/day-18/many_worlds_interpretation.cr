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

class AdvancedPathFinder
  def initialize(map : Array(Array(Tile)))
    @map = map
    @rows = Int32.new(map.size)
    @columns = Int32.new(map.first.size)
    @keys = Set(Tile).new
    @doors = Set(Tile).new
    @collectors = [] of Collector
    @to_visit = [] of Tuple(String, String, Int32)
    scan_map
  end

  def shortest_path
    visited = Hash(Tuple(String, String), Int32).new

    while !@to_visit.empty?
      step = @to_visit.shift()
      state = {step[0], step[1]}
      next if visited.includes?(state)

      visited[state] = step[2]

      step[0].split(',').map { |x| x.to_i }.each_slice(2) do |slice|
        row, column = slice.pop(2)
        tile = @map[row][column]

        next if tile.wall?
        next if invalid_position?(row, column)
        next if tile.door? && step[1].includes?(tile.symbol)
        collected_keys = Set.new(step[1].split)

        if tile.key?
          collected_keys.add(tile.symbol.to_s)
          return step[2] if @keys.size == collected_keys.size
        end

        [{-1, 0}, {0, 1}, {1, 0}, {0, -1}].each do |x, y|
          new_row = row + x
          new_column = column + y
          state = {[new_row, new_column].join, collected_keys.join, step[2] + 1}
          @to_visit.push(state)
        end
      end
    end
  end

  private def collectors_position
    @collectors.flat_map { |collector| [collector.row, collector.column] }.join(',')
  end

  private def scan_map
    @rows.times do |row|
      @columns.times do |column|
        tile = @map[row][column]
        if tile.entrance?
          collector = Collector.new(row, column)
          @collectors.push(collector)
        elsif tile.key?
          @keys.add(tile)
        elsif tile.door?
          @doors.add(tile)
        end
      end
    end
    state = {collectors_position, "", 0}
    @to_visit.push(state)
  end

  private def invalid_position?(row, column)
    !(0 <= row < @rows && 0 <= column < @columns)
  end
end

MAP = File.read("input").strip().split("\n").map do |line|
  line.split("").map { |symbol| Tile.new(symbol.chars.first) }
end

finder = PathFinder.new(MAP)
puts finder.shortest_path

advanced_finder = AdvancedPathFinder.new(MAP)
puts advanced_finder.shortest_path
