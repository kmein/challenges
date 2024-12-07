# Load the gmp package for big integer support
library(gmp)

# Assuming the file reading and statements creation is already done
if (Sys.getenv("AOC_TEST") != "") {
  fileName <- "07.txt.test"
} else {
  fileName <- "07.txt"
}

data <- read.delim(fileName, header=FALSE, sep = ":", dec = ".")
second_column <- data[, 2]  # Extract the second column

# Parse the second column into a list of big integer operands
operands_list <- lapply(lapply(second_column, function(x) {
  # Split the string by spaces and convert to big integers
  as.bigz(unlist(strsplit(x, split = " ")))
}), function(x) x[!is.na(x)])

# Extract expected results from the first column and convert to big integers
expected_results <- as.bigz(data[, 1])  # Convert expected results to big integers

# Function to create operator combinations
operatorTable <- function(N, with_concat=FALSE) {
  if (with_concat) {
    operators <- c("*", "+", "||")
  } else {
    operators <- c("*", "+")
  }
  do.call(expand.grid, replicate(N, operators, simplify = FALSE))
}

# Function to evaluate expressions left-to-right using big integers
evaluateExpression <- function(operands, operators) {
  result <- operands[[1]]  # Start with the first operand
  for (i in seq_along(operators)) {
    if (operators[i] == "*") {
      result <- result * operands[[i + 1]]
    } else if (operators[i] == "+") {
      result <- result + operands[[i + 1]]
    } else if (operators[i] == "||") {
      result <- as.bigz(paste0(result, operands[[i+1]]))

    }
  }
  return(result)
}

# Evaluate each combination of operands with operators and check against expected results
results <- list()
for (i in seq_along(operands_list)) {
  operands <- operands_list[[i]]
  expected_result <- expected_results[i]

  ops_combinations <- operatorTable(length(operands) - 1, with_concat=TRUE)  # Number of operators is one less than operands
  for (j in 1:nrow(ops_combinations)) {
    operators <- (ops_combinations[j, ])
    result <- evaluateExpression(operands, operators)

    # Check if the result matches the expected result
    if (identical(result, expected_result)) {
      results[[length(results) + 1]] <- list(operands = operands, operators = operators, result = result, matches = TRUE)
    } else {
      results[[length(results) + 1]] <- list(operands = operands, operators = operators, result = result, matches = FALSE)
    }
  }
}

# Calculate the sum of unique results where matches == TRUE
matching_results <- sapply(results, function(res) {
  if (res$matches) {
    return(res$result[[1]])
  } else {
    return(NA)  # Return NA if it doesn't match
  }
})

unique_matching_results <- (unique(matching_results))

# Sum the unique matching results
total_sum <- 0
for (i in (unique_matching_results[!is.na(unique_matching_results)])) {
  total_sum <- total_sum + i
}
print(total_sum)