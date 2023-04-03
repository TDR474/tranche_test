#Uploading compiled bid data
getwd()
#read.csv("Test_Data.csv")
BidData <- read.csv("Test_Data.csv")


supply_data <- BidData[,-1]

# Generate all possible combinations of tranches supplied by the suppliers
supplier_names <- c("Anthony", "Bernie", "Charlene", "Darryl", "Elizabeth", "Frank", "George", "Helen")
num_suppliers <- length(supplier_names)
supplier_tranches <- list()
for (i in 1:num_suppliers) {
  supplier_tranches[[i]] <- 1:16
}

all_combinations <- expand.grid(supplier_tranches)

# Calculate the cost of each combination
costs <- numeric(nrow(all_combinations))
for (i in 1:nrow(all_combinations)) {
  # Check if this combination satisfies the capacity constraint
  if (sum(all_combinations[i,]) == 16) {
    # Calculate the total cost of this combination
    for (j in 1:num_suppliers) {
      supplier_name <- supplier_names[j]
      prices <- supply_data[, supplier_name]
      tranches <- all_combinations[i, j]
      cost <- sum(prices * tranches)
      costs[i] <- costs[i] + cost
    }
  } else {
    costs[i] <- Inf
  }
}

# Find the combination with the lowest cost
min_cost <- min(costs)
min_cost_index <- which(costs == min_cost)[1]
min_cost_combination <- all_combinations[min_cost_index,]

# Print the result
result <- data.frame(supplier = supplier_names, tranches = min_cost_combination)
result

