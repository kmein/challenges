local list1 = {}
local list2 = {}

local file
if os.getenv("AOC_TEST") then
  file = io.open("01.txt.test", "r")
else
  file = io.open("01.txt", "r")
end

if not file then
  print("Could not open file.")
  return
end

for line in file:lines() do
  local num1, num2 = line:match("(%S+)%s+(%S+)")
  if num1 and num2 then
    table.insert(list1, tonumber(num1))
    table.insert(list2, tonumber(num2))
  end
end

file:close()

local function list_sum_difference(a, b)
  table.sort(a)
  table.sort(b)
  local length = math.min(#a, #b)

  local sum_of_differences = 0

  for i = 1, length do
    sum_of_differences = sum_of_differences + math.abs(a[i] - b[i])
  end

  return sum_of_differences
end

print("Part 1", list_sum_difference(list1, list2))

local function count_values(list)
  local stats = {}
  for i = 1, #list do
    local list_element = list[i]
    stats[list_element] = (stats[list_element] or 0) + 1
  end
  return stats
end


local function similarity_score(a, b)
  local stats_b = count_values(b)
  local score = 0
  for i = 1, #a do
    local element = a[i]
    score = score + element * (stats_b[element] or 0)
  end
  return score
end

print("Part 2", similarity_score(list1, list2))
