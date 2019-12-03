defmodule CrossedWires do
  def call do
    "input"
    |> File.read!
    |> String.split("\n", trim: true)
    |> Enum.map(&to_wire/1)
    |> Enum.map(fn {a, _} -> a end)
    |> Enum.map(&MapSet.new/1)
    |> Enum.reduce(&MapSet.intersection/2)
    |> Enum.map(&manhattan_distance/1)
    |> Enum.sort
    |> List.first
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
  defp path("R", i, {acc, {p, q}}), do: path("R", i - 1, {[{p + 1, q} | acc], {p + 1, q}})
  defp path("L", i, {acc, {p, q}}), do: path("L", i - 1, {[{p - 1, q} | acc], {p - 1, q}})
  defp path("U", i, {acc, {p, q}}), do: path("U", i - 1, {[{p, q + 1} | acc], {p, q + 1}})
  defp path("D", i, {acc, {p, q}}), do: path("D", i - 1, {[{p, q - 1} | acc], {p, q - 1}})

  defp manhattan_distance(point) do
    [Tuple.to_list(point), [0, 0]]
    |> Enum.zip()
    |> Enum.reduce(0, fn {a, b}, acc -> abs(a - b) + acc end)
  end
end
