def read_input
  filename = ENV['AOC_TEST'] ? '19.txt.test' : '19.txt'
  colors = []
  designs = []

  File.open(filename, 'r') do |file|
    # Read the first section (colors)
    file.each_line do |line|
      line.strip!
      break if line.empty?  # Stop reading when an empty line is encountered
      colors.concat(line.split(',').map(&:strip))  # Split by comma and strip whitespace
    end

    # Read the second section (designs)
    file.each_line do |line|
      line.strip!
      next if line.empty?  # Skip empty lines
      designs << line  # Add the line to the designs array
    end
  end

  [colors, designs]
end

def count_ways_to_form_design(design, colors)
  # Initialize a ways array to count the number of ways to form each prefix of the design
  ways = Array.new(design.length + 1, 0)
  ways[0] = 1  # Base case: there's one way to form an empty design

  (1..design.length).each do |i|
    colors.each do |color|
      color_length = color.length
      if i >= color_length && design[i - color_length, color_length] == color
        ways[i] += ways[i - color_length]
      end
    end
  end

  ways[design.length]
end

def can_partition_with_colors?(design, colors)
  # Use dynamic programming to check if the design can be formed
  dp = Array.new(design.length + 1, false)
  dp[0] = true  # Base case: empty design can always be formed

  (1..design.length).each do |i|
    colors.each do |color|
      color_length = color.length
      if i >= color_length && dp[i - color_length] && design[i - color_length, color_length] == color
        dp[i] = true
        break
      end
    end
  end

  dp[design.length]
end

def total_ways_for_designs
  colors, designs = read_input
  total_ways = 0
  possible_count = 0

  designs.each do |design|
    ways = count_ways_to_form_design(design, colors)
    total_ways += ways

    possible_count += 1 if can_partition_with_colors?(design, colors)
  end

  [possible_count, total_ways]
end

possible_designs_count, total_designs_count = total_ways_for_designs
puts "Number of possible designs: #{possible_designs_count}"
puts "Total number of ways to create all designs: #{total_designs_count}"
