# ================================================================
# Project Title : Air Quality Analysis - Environmental Monitoring
# Student Name  : Pawar Shubham Dilip
# Date          : 29-03-2026
# Unit          : Unit 1 - Introduction to R
# Description   : Exploratory data analysis of airquality dataset
#                 to identify pollution patterns and risk periods.
# ================================================================

# Load the built-in airquality dataset
data(airquality)

# Display the first 6 rows
head(airquality)
# Each row = one day of observation
# Columns: Ozone (ppb), Solar.R (lang), Wind (mph), Temp (°F), Month, Day

# View the dataset in tabular format (opens spreadsheet-like viewer in RStudio)
View(airquality)

# Summary statistics - min, max, mean, median, NA count for each column
summary(airquality)

# Column names
colnames(airquality)

# Structure of dataset - data types and sample values
str(airquality)

# STEP 1: Install the package (only once - downloads from CRAN)
# Run this line only the first time; comment it out after installation
#install.packages("ggplot2")

# STEP 2: Load the package into the current session (every new session)
library(ggplot2)

# Verify the package is loaded
packageVersion("ggplot2")   # Shows installed version

# ── Variable: Store average temperature ──
avg_temp <- mean(airquality$Temp, na.rm = TRUE)
cat("Average Temperature (F):", round(avg_temp, 2), "\n")

# ── Constant: Safe ozone limit (defined by environmental standards) ──
SAFE_OZONE_LIMIT <- 70   # parts per billion (ppb) - EPA guideline

# ── String: Analysis context ──
CITY_NAME     <- "New York"
ANALYSIS_YEAR <- "1973"
ANALYST_NAME  <- "Environmental Data Analyst"

# Print all values
cat("City          :", CITY_NAME, "\n")
cat("Analysis Year :", ANALYSIS_YEAR, "\n")
cat("Analyst       :", ANALYST_NAME, "\n")
cat("Avg Temp (F)  :", round(avg_temp, 2), "\n")
cat("Safe Ozone Lim:", SAFE_OZONE_LIMIT, "ppb\n")

# Check data type of each
class(avg_temp)          # "numeric"
class(SAFE_OZONE_LIMIT)  # "numeric"
class(CITY_NAME)         # "character"


# Remove rows with NA ozone values for this analysis
ozone_clean <- airquality$Ozone[!is.na(airquality$Ozone)]

# Identify days where ozone > safe limit (logical vector)
unsafe_days_logical <- ozone_clean > SAFE_OZONE_LIMIT

# Count unsafe and safe days
num_unsafe <- sum(unsafe_days_logical)
num_safe   <- sum(!unsafe_days_logical)
total_days <- length(ozone_clean)

cat("Total days with ozone data:", total_days, "\n")
cat("Unsafe days (Ozone > 70 ppb):", num_unsafe, "\n")
cat("Safe days   (Ozone <= 70 ppb):", num_safe, "\n")

# Calculate percentage
pct_unsafe <- round((num_unsafe / total_days) * 100, 1)
cat("Percentage of unsafe days   :", pct_unsafe, "%\n")

# Show which rows in the original dataframe are unsafe
unsafe_records <- airquality[!is.na(airquality$Ozone) & airquality$Ozone > SAFE_OZONE_LIMIT, ]
cat("\nUnsafe day records (first 5):\n")
print(head(unsafe_records, 5))


library(ggplot2)

# Remove rows with NA in Ozone or Temp for clean plotting
aq_clean <- airquality[!is.na(airquality$Ozone) & !is.na(airquality$Temp), ]

# Create scatter plot
ggplot(aq_clean, aes(x = Temp, y = Ozone)) +
  geom_point(aes(color = Ozone > SAFE_OZONE_LIMIT), size = 3, alpha = 0.7) +
  scale_color_manual(
    values = c("FALSE" = "steelblue", "TRUE" = "firebrick"),
    labels = c("FALSE" = "Safe (<=70 ppb)", "TRUE" = "Unsafe (>70 ppb)")
  ) +
  geom_hline(yintercept = SAFE_OZONE_LIMIT, linetype = "dashed",
             color = "orange", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen", linetype = "solid") +
  labs(
    title    = "Temperature vs Ozone Levels - New York (1973)",
    subtitle = "Orange dashed line = Safe Ozone Limit (70 ppb)",
    x        = "Temperature (degrees Fahrenheit)",
    y        = "Ozone Level (ppb)",
    color    = "Air Quality Status"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )


