library(gmp)

fileName <- if (Sys.getenv("AOC_TEST") != "") "07.txt.test" else "07.txt"

cat("Reading data from:", fileName, "\n")
data <- read.delim(fileName, header=FALSE, sep = ":", dec = ".")
second_column <- data[, 2]  # Extract the second column

# Parse the second column into a list of big integer operands
cat("Parsing operands...\n")
operands_list <- lapply(second_column, function(x) {
  as.bigz(unlist(strsplit(x, split = " ")))
})

# Extract expected results from the first column and convert to big integers
cat("Extracting expected results...\n")
expected_results <- as.bigz(data[, 1])  # Convert expected results to big integers

# Function to create operator combinations
operatorTable <- function(N, with_concat=FALSE) {
  operators <- if (with_concat) c("*", "+", "||") else c("*", "+")
  do.call(expand.grid, replicate(N, operators, simplify = FALSE))
}

# Function to evaluate expressions left-to-right using big integers
evaluateExpression <- function(operands, operators, expected_result) {
  result <- operands[[1]]  # Start with the first operand
  for (i in seq_along(operators)) {
    if (result > expected_result) {
      return(FALSE)
    }
    if (operators[i] == "*") {
      result <- result * operands[[i + 1]]
    } else if (operators[i] == "+") {
      result <- result + operands[[i + 1]]
    } else if (operators[i] == "||") {
      result <- as.bigz(paste0(result, operands[[i + 1]]))
    }
  }
  return(identical(result, expected_result))
}

# Initialize a counter for matching results
match_sum <- 0

# Evaluate each combination of operands with operators and check against expected results
cat("Evaluating expressions...\n")
for (i in seq_along(operands_list)) {
  operands <- na.omit(operands_list[[i]])
  expected_result <- expected_results[i]

  cat("Processing set", i, "with operands:", toString(operands), "(expected result", as.numeric(expected_result), ")\n")

  ops_combinations <- operatorTable(length(operands) - 1, with_concat=TRUE)  # Number of operators is one less than operands

  cat("╰", nrow(ops_combinations), "combinations\n")

  for (j in 1:nrow(ops_combinations)) {
    operators <- ops_combinations[j, ]

    # Debugging statement to show the current operator combination

    matches <- evaluateExpression(operands, operators, expected_result)

    # Check if the result matches the expected result
    if (matches) {
      match_sum <- match_sum + expected_result
      cat(" ╰ Match found! ", paste(operators, collapse=" "), "\n")
      break  # Exit the loop once a match is found
    }
  }
}

# Output the total number of matches found
cat("Total number of matches found:", as.double(match_sum), "\n")
