defmodule CrossedWires do
  # Part 1
  def closest_distance do
    wires()
    |> Enum.map(&MapSet.new/1)
    |> Enum.reduce(&MapSet.intersection/2)
    |> Enum.map(&manhattan_distance/1)
    |> Enum.sort
    |> List.first
  end

  # Part 2
  def fewest_combined_steps do
    [first_wire, second_wire] = wires()
    [first_wire, second_wire]
    |> Enum.map(&MapSet.new/1)
    |> Enum.reduce(&MapSet.intersection/2)
    |> Enum.map(&sum_steps(&1, first_wire, second_wire))
    |> Enum.sort
    |> List.first
  end

  defp wires do
    "input"
    |> File.read!
    |> String.split("\n", trim: true)
    |> Enum.map(&to_wire/1)
    |> Enum.map(fn {a, _} -> a end)
  end

  defp to_wire(steps) do
    steps
    |> String.split(",")
    |> Enum.reduce({[], {0, 0}}, &build_path/2)
  end

  defp build_path(step, acc) do
    direction = step |> String.first
    distance = step |> String.slice(1..-1) |> String.to_integer
    path(direction, distance, acc)
  end

  defp path(_, 0, {acc, {p, q}}), do: {acc, {p, q}}
  defp path("R", i, {acc, {p, q}}), do: path("R", i - 1, {acc ++ [{p + 1, q}], {p + 1, q}})
  defp path("L", i, {acc, {p, q}}), do: path("L", i - 1, {acc ++ [{p - 1, q}], {p - 1, q}})
  defp path("U", i, {acc, {p, q}}), do: path("U", i - 1, {acc ++ [{p, q + 1}], {p, q + 1}})
  defp path("D", i, {acc, {p, q}}), do: path("D", i - 1, {acc ++ [{p, q - 1}], {p, q - 1}})

  defp manhattan_distance(point) do
    [Tuple.to_list(point), [0, 0]]
    |> Enum.zip()
    |> Enum.reduce(0, fn {a, b}, acc -> abs(a - b) + acc end)
  end

  defp sum_steps(intersection, first_wire, second_wire) do
    [first_wire, second_wire]
    |> Enum.reduce(0, fn wire, acc -> acc + count_steps(wire, intersection) end)
  end

  defp count_steps(wire, intersection) do
    Enum.find_index(wire, fn x -> x == intersection end) + 1
  end
end
