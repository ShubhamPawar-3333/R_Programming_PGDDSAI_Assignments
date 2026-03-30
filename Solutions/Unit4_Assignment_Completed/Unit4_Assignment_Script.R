# Create the student dataset and save as CSV
student_data <- data.frame(
  Name       = c("Arjun", "Priya", "Rahul", "Sneha", "Karan",
                 "Meera", "Aditya", "Pooja", "Vikram", "Tanya"),
  Math       = c(85, 92, NA, 78, 65, 88, 72, NA, 90, 55),
  Science    = c(78, 85, 90, NA, 70, 82, 65, 88, 74, 60),
  English    = c(70, 88, 75, 80, NA, 76, 68, 92, 70, 72),
  Attendance = c(90, 95, 85, 88, 72, 93, 80, 91, 87, 65),
  Gender     = c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F")
)

# Save to CSV
write.csv(student_data, "student_data.csv", row.names = FALSE)
cat("CSV file created: student_data.csv\n")

# 1. Import the CSV file
df <- read.csv("student_data.csv", stringsAsFactors = FALSE)

# 2. Display the dataset
print(df)

# 3. Check structure - column names, data types, sample values
str(df)

# 4. First 5 and last 5 rows
head(df, 5)
tail(df, 5)

# 5. Identify missing values (TRUE = NA at that position)
is.na(df)

# 6. Count total missing values per column
colSums(is.na(df))

# 7. Replace NA in numeric columns with column mean
df$Math    <- ifelse(is.na(df$Math),    round(mean(df$Math,    na.rm=TRUE)), df$Math)
df$Science <- ifelse(is.na(df$Science), round(mean(df$Science, na.rm=TRUE)), df$Science)
df$English <- ifelse(is.na(df$English), round(mean(df$English, na.rm=TRUE)), df$English)

# 8. Verify - should show 0 NAs in all numeric columns
colSums(is.na(df))

cat("Dataset is clean - no missing values.\n")

# 9. Install and load dplyr
install.packages("dplyr")   # run once
library(dplyr)

# 10. Create Average column
df <- df %>%
  mutate(Average = round((Math + Science + English) / 3, 2))

# 11. Filter students with Average > 75
high_performers <- df %>% filter(Average > 75)
print(high_performers)

# 12. Select only Name and Average
df %>% select(Name, Average)

# 13. Arrange in descending order of Average
df %>% arrange(desc(Average))

# 14. Group by Gender and find average marks
df %>%
  group_by(Gender) %>%
  summarise(
    Avg_Math    = round(mean(Math), 2),
    Avg_Science = round(mean(Science), 2),
    Avg_English = round(mean(English), 2),
    Avg_Overall = round(mean(Average), 2),
    Count       = n()
  )


# 15. Summary statistics
summary(df)

# 16. Mean of each subject
cat("Mean Math    :", round(mean(df$Math), 2), "\n")
cat("Mean Science :", round(mean(df$Science), 2), "\n")
cat("Mean English :", round(mean(df$English), 2), "\n")

# 17. Correlation between subjects
cor_matrix <- cor(df[ , c("Math", "Science", "English")])
print(round(cor_matrix, 3))

# 18. Subject with highest average
subject_means <- c(Math = mean(df$Math), Science = mean(df$Science), English = mean(df$English))
cat("Subject with highest average:", names(which.max(subject_means)), "\n")
cat("Score:", round(max(subject_means), 2), "\n")

# 19. Install and load ggplot2
install.packages("ggplot2")   # run once
library(ggplot2)

# 20. Bar plot: Student vs Average marks
ggplot(df, aes(x = reorder(Name, -Average), y = Average, fill = Average)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#f5a742", high = "#1F3864") +
  labs(title = "Student Average Marks", x = "Student Name", y = "Average Marks") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 21. Scatter plot: Attendance vs Average
ggplot(df, aes(x = Attendance, y = Average, color = Gender, label = Name)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = FALSE, color = "gray40") +
  labs(title = "Attendance vs Average Marks", x = "Attendance (%)", y = "Average Marks") +
  theme_minimal()

# 22. Boxplot: Average marks by Gender
ggplot(df, aes(x = Gender, y = Average, fill = Gender)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Average Marks by Gender", x = "Gender", y = "Average Marks") +
  theme_minimal()

# 23. Histogram: Distribution of Average marks
ggplot(df, aes(x = Average)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Average Marks", x = "Average Marks", y = "Count") +
  theme_minimal()


# 24. Mean of Average marks
mean_avg <- mean(df$Average)
cat("Mean of Average marks:", round(mean_avg, 2), "\n")

# 25. Median of Average marks
median_avg <- median(df$Average)
cat("Median of Average marks:", round(median_avg, 2), "\n")

# 26. Standard deviation
sd_avg <- sd(df$Average)
cat("Standard Deviation:", round(sd_avg, 2), "\n")

# 27. Observe and compare mean vs median
cat("\n--- Mean vs Median Comparison ---\n")
cat("Mean  :", round(mean_avg, 2), "\n")
cat("Median:", round(median_avg, 2), "\n")
if (abs(mean_avg - median_avg) < 2) {
  cat("Observation: Mean and Median are close - distribution is approximately symmetric.\n")
} else if (mean_avg > median_avg) {
  cat("Observation: Mean > Median - distribution is right-skewed (high scores pull up mean).\n")
} else {
  cat("Observation: Mean < Median - distribution is left-skewed.\n")
}

# 28. Build linear regression model
# Formula: Average ~ Attendance means 'predict Average using Attendance'
model <- lm(Average ~ Attendance, data = df)

# 29. Display model summary
summary(model)
# Key outputs to look for:
# - Coefficients: Intercept and Attendance slope
# - R-squared: how much variance in Average is explained by Attendance
# - p-value: statistical significance (< 0.05 = significant)

# 30. Predict Average marks using the model
predicted_values <- predict(model, newdata = df)
cat("\nPredicted Average Marks:\n")
print(round(predicted_values, 2))

# 31. Compare actual vs predicted
comparison <- data.frame(
  Name      = df$Name,
  Actual    = df$Average,
  Predicted = round(predicted_values, 2),
  Residual  = round(df$Average - predicted_values, 2)
)
cat("\n--- Actual vs Predicted ---\n")
print(comparison)
# Residual = Actual - Predicted (positive = model under-predicted)


# 32. Save cleaned dataset as CSV
write.csv(df, "student_data_cleaned.csv", row.names = FALSE)
cat("Cleaned CSV saved: student_data_cleaned.csv\n")

# 33. Save dataset as R object file (.RData)
saveRDS(df, "student_data.RDS")
cat("R object saved: student_data.RDS\n")

# 34. Load the saved R object file
df_loaded <- readRDS("student_data.RDS")
cat("R object loaded successfully.\n")
identical(df, df_loaded)
head(df_loaded, 3)

# 35. Create Grade column based on Average
df$Grade <- ifelse(df$Average >= 85, "A",
                   ifelse(df$Average >= 70, "B", "C"))

cat("\n--- Grade Distribution ---\n")
print(table(df$Grade))
print(df[ , c("Name", "Average", "Grade")])

# 36. Top 3 students by Average
top3 <- df[order(-df$Average), ][1:3, ]
cat("\n--- Top 3 Students ---\n")
print(top3[ , c("Name", "Average", "Grade")])

# 37. Which Gender performs better overall
gender_performance <- tapply(df$Average, df$Gender, mean)
cat("\n--- Gender-wise Average Performance ---\n")
print(round(gender_performance, 2))
better_gender <- names(which.max(gender_performance))
cat("Better performing gender:", better_gender, "\n")

# 38. Additional visualization: Grade distribution pie-like bar chart
grade_counts <- as.data.frame(table(df$Grade))
colnames(grade_counts) <- c("Grade", "Count")

ggplot(grade_counts, aes(x = Grade, y = Count, fill = Grade)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("A" = "#2E86AB", "B" = "#A8D5A2", "C" = "#F6AE2D")) +
  geom_text(aes(label = Count), vjust = -0.5, size = 5) +
  labs(title = "Grade Distribution of Students",
       x = "Grade", y = "Number of Students") +
  theme_minimal()




