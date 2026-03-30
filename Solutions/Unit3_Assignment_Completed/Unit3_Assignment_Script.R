# ── Grocery Store Customer Dataset ──
customer_data <- data.frame(
  CustomerID   = c(101, 102, 103, 104, 105, 106, 107, 108),
  AmountSpent  = c(2500, 1800, 500, 3200, 950, 1500, 2100, 800),
  Visits       = c(5, 3, 1, 7, 2, 4, 6, 2),
  Membership   = c("Gold", "Silver", "Bronze", "Gold",
                   "Bronze", "Silver", "Gold", "Bronze")
)

# View the dataset
print(customer_data)
str(customer_data)

# Task 1: Create Category column using nested ifelse()
# ifelse(test, yes, no) - vectorised, works on entire column at once
customer_data$Category <- ifelse(
  customer_data$AmountSpent > 2000, "High",
  ifelse(
    customer_data$AmountSpent >= 1000, "Medium",
    "Low"
  )
)

# Display updated dataset
print(customer_data[ , c("CustomerID", "AmountSpent", "Category")])

# Task 2: Loop through each customer and print ID + Category
cat("\n=== Customer Category Report ===\n")

for (i in 1:nrow(customer_data)) {
  cat("Customer ID:", customer_data$CustomerID[i],
      "| Category:", customer_data$Category[i], "\n")
}

# Task 3: Control flow with next (skip) and break (stop)
cat("\n=== Filtered Customer Report (Visits >= 3) ===\n")

for (i in 1:nrow(customer_data)) {
  
  # STOP: break if any amount is negative (data integrity check)
  if (customer_data$AmountSpent[i] < 0) {
    cat("ERROR: Negative amount found for Customer",
        customer_data$CustomerID[i], "- Stopping.\n")
    break
  }
  
  # SKIP: next if visits < 3 (not enough engagement)
  if (customer_data$Visits[i] < 3) {
    cat("Skipping Customer", customer_data$CustomerID[i],
        "(only", customer_data$Visits[i], "visit(s))\n")
    next
  }
  
  # Process eligible customers
  cat("Customer:", customer_data$CustomerID[i],
      "| Visits:", customer_data$Visits[i],
      "| Amount: Rs.", customer_data$AmountSpent[i], "\n")
}


# Task 4: Apply family functions

# sapply() applies a function to a vector and returns a simplified result
# Here we use it on a single-column numeric vector

amount_vector <- customer_data$AmountSpent

# Average amount spent
avg_amount <- sapply(list(avg = amount_vector), mean)
cat("Average Amount Spent: Rs.", round(avg_amount, 2), "\n")
# Output: Average Amount Spent: Rs. 1543.75

# Maximum amount spent
max_amount <- sapply(list(max = amount_vector), max)
cat("Maximum Amount Spent: Rs.", max_amount, "\n")
# Output: Maximum Amount Spent: Rs. 3200

# Alternative: Using apply() on a matrix column
marks_matrix <- as.matrix(customer_data$AmountSpent)
cat("Mean via apply():", apply(marks_matrix, 2, mean), "\n")
cat("Max  via apply():", apply(marks_matrix, 2, max),  "\n")


# Task 5: Function with default argument
calculate_discount <- function(amount, membership = "Bronze") {
  # Define discount rates per membership tier
  rate <- switch(membership,
                 "Gold"   = 0.20,   # 20%
                 "Silver" = 0.10,   # 10%
                 "Bronze" = 0.05,   # 5% (also the default)
                 stop("Unknown membership type: ", membership)
  )
  discount_amount <- amount * rate
  final_price     <- amount - discount_amount
  
  cat("Membership :", membership, "\n")
  cat("Amount     : Rs.", amount, "\n")
  cat("Discount(%): ", rate * 100, "%\n")
  cat("Discount   : Rs.", discount_amount, "\n")
  cat("Final Price: Rs.", final_price, "\n\n")
  
  return(final_price)
}

# Test with specific memberships
calculate_discount(2500, "Gold")    # 20% off
calculate_discount(1800, "Silver")  # 10% off
calculate_discount(500)             # Default: Bronze, 5% off

# Apply to entire dataset
customer_data$FinalPrice <- mapply(calculate_discount,
                                   customer_data$AmountSpent,
                                   customer_data$Membership)


# Task 6: Lazy Evaluation
# R uses lazy evaluation: arguments are only computed when first accessed.
# If an argument is never used, it is never evaluated - no error occurs.

lazy_discount <- function(amount, membership, verbose = FALSE) {
  # 'verbose' parameter is lazy - only evaluated if needed
  
  rate <- switch(membership,
                 "Gold" = 0.20, "Silver" = 0.10, "Bronze" = 0.05
  )
  
  # Discount is calculated ONLY when this line is reached
  # If we returned early before this, 'rate' would never be used
  discount <- amount * rate
  
  # 'verbose' argument is only evaluated here, not at function call time
  if (verbose) {
    cat("[Verbose] Rate applied:", rate * 100, "% on Rs.", amount, "\n")
  }
  
  return(amount - discount)
}

# Without verbose: discount logic runs, verbose block is skipped
result1 <- lazy_discount(2500, "Gold")
cat("Result (no verbose):", result1, "\n")

# With verbose: discount logic runs AND verbose block executes
result2 <- lazy_discount(2500, "Gold", verbose = TRUE)
cat("Result (verbose):", result2, "\n")
# [Verbose] Rate applied: 20 % on Rs. 2500

# Lazy evaluation demo: unused argument causes no error
f <- function(x, y) x + 1  # y is never used
f(5)  # Works fine - R never evaluates the missing y


# Task 7: Objects and Classes (S3 system)

# Create a list for Customer 101
customer_obj <- list(
  id         = 101,
  name       = "Rahul Sharma",
  amount     = 2500,
  visits     = 5,
  membership = "Gold",
  category   = "High"
)

# Assign a class to the list - now it is a 'Customer' object
class(customer_obj) <- "Customer"

# Check the class
cat("Object class:", class(customer_obj), "\n")
# Output: Object class: Customer

# Define a custom print method for the Customer class
print.Customer <- function(obj) {
  cat("========== Customer Profile ==========\n")
  cat("ID        :", obj$id,         "\n")
  cat("Name      :", obj$name,       "\n")
  cat("Membership:", obj$membership, "\n")
  cat("Amount    : Rs.", obj$amount, "\n")
  cat("Visits    :", obj$visits,     "\n")
  cat("Category  :", obj$category,  "\n")
  cat("======================================\n")
}

# Calling print() will now use our custom method
print(customer_obj)

# Check structure
str(customer_obj)
is.list(customer_obj)
inherits(customer_obj, "Customer")


# Task 8: Debugging

# ── BUGGY FUNCTION (contains an error) ──
calculate_total_buggy <- function(prices) {
  total <- sum(price)   # BUG: 'price' should be 'prices' (wrong variable name)
  return(total)
}

# Calling the buggy function produces an error:
# calculate_total_buggy(c(100, 200, 300))
# Error in calculate_total_buggy(c(100, 200, 300)) :
#   object 'price' not found

# ── DEBUGGING STEPS ──
# Step 1: Read the error message carefully - 'price' not found
# Step 2: Check the function body - parameter name is 'prices'
# Step 3: The bug is a typo: sum(price) should be sum(prices)

# ── CORRECTED FUNCTION ──
calculate_total_fixed <- function(prices) {
  total <- sum(prices)  # FIXED: correct variable name
  return(total)
}

# Test corrected function
result <- calculate_total_fixed(c(100, 200, 300))
cat("Total price: Rs.", result, "\n")

# ── Additional Debugging Tools ──
# traceback()   - shows call stack after an error
# debug(fn)     - step through function line by line
# browser()     - insert inside function to pause and inspect
# print() / cat() - add diagnostic output at key points


