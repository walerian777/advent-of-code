function load_input()
  return open("input") do file
    input = split(chomp(read(file, String)), "")
    map(x -> parse(Int, x), input)
  end
end

function execute_algorithm(signal)
  signal_length = length(signal)
  mean = div(signal_length, 2) + 1

  for _ in 1:1:100
    original_signal = deepcopy(signal)

    for i in 1:1:mean
      j = i
      subsum = 0

      while j <= signal_length
        upper_bound = min(j + i - 1, signal_length)
        subsum += sum(original_signal[j:upper_bound])
        j += 2 * i

        upper_bound = min(j + i - 1, signal_length)
        subsum -= sum(original_signal[j:upper_bound])
        j += 2 * i
      end

      signal[i] = abs(subsum % 10)
    end

    for i in signal_length:-1:(mean + 2)
      signal[i - 1] += signal[i]
      signal[i - 1] %= 10
    end
  end

  return signal
end

function generate_real_signal(signal)
  signal_length = length(signal)
  real_signal = deepcopy(signal)

  while length(real_signal) < (signal_length * 10000)
    append!(real_signal, signal)
  end

  return real_signal
end

function execute_algorithm_with_offset(signal)
  offset = parse(Int, join(signal[1:7])) + 1
  signal = signal[offset:end]
  signal_length = length(signal)

  for _ in 1:1:100
    for i in signal_length:-1:2
      signal[i - 1] += signal[i]
      signal[i - 1] %= 10
    end
  end

  return signal
end

input = load_input()

digits = execute_algorithm(copy(input))
println(join(digits[1:8]))

real_signal = generate_real_signal(copy(input))
digits = execute_algorithm_with_offset(real_signal)
println(join(digits[1:8]))
