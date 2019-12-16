numbers = open("input") do file
  input = split(chomp(read(file, String)), "")
  map(x -> parse(Int, x), input)
end

numbers_count = length(numbers)
mean = div(numbers_count, 2) + 1

for _ in 1:1:100
  original_numbers = deepcopy(numbers)

  for i in 1:1:mean
    j = i
    subsum = 0

    while j <= numbers_count
      upper_bound = ifelse(j + i - 1 > numbers_count, numbers_count, j + i - 1)
      subsum += sum(original_numbers[j:upper_bound])
      j += 2 * i

      upper_bound = ifelse(j + i - 1 > numbers_count, numbers_count, j + i - 1)
      subsum -= sum(original_numbers[j:upper_bound])
      j += 2 * i
    end

    numbers[i] = abs(subsum % 10)
  end

  for i in numbers_count:-1:(mean + 2)
    numbers[i - 1] += numbers[i]
    numbers[i - 1] %= 10
  end
end

println(join(numbers[1:8]))
