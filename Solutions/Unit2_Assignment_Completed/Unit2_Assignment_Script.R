# Task 1: Generate Student IDs from 1 to 120 using seq()
student_ids <- seq(1, 120, by = 1)

# Verify
length(student_ids)
head(student_ids, 5)
tail(student_ids, 5)

# Task 2: Create Section vector using rep()
# 40 students each in Section A, B, C
sections <- rep(c("A", "B", "C"), each = 40)

# Verify
length(sections)
table(sections)
head(sections, 5)
sections[39:42]


# Task 3: 60 Male + 60 Female students, randomized order
set.seed(42)  # For reproducibility
gender_base <- c(rep("Male", 60), rep("Female", 60))
gender <- sample(gender_base)  # randomize order

# Verify
length(gender)
table(gender)
head(gender, 10)

# Task 4: Generate marks for all 5 subjects
# Each: random marks 40-100, 10 NA values introduced
set.seed(100)
generate_marks <- function(n = 120, na_count = 10) {
  marks <- sample(40:100, n, replace = TRUE)
  na_positions <- sample(1:n, na_count)
  marks[na_positions] <- NA
  return(marks)
}

math_marks  <- generate_marks()
stats_marks <- generate_marks()
prog_marks  <- generate_marks()
db_marks    <- generate_marks()
ml_marks    <- generate_marks()

# Count NAs in each subject
cat("NA counts - Math:", sum(is.na(math_marks)),
    "| Stats:", sum(is.na(stats_marks)),
    "| Prog:",  sum(is.na(prog_marks)),
    "| DB:",    sum(is.na(db_marks)),
    "| ML:",    sum(is.na(ml_marks)), "\n")

# Calculate subject means (na.rm removes NAs before computing mean)
means <- c(
  Math  = mean(math_marks,  na.rm = TRUE),
  Stats = mean(stats_marks, na.rm = TRUE),
  Prog  = mean(prog_marks,  na.rm = TRUE),
  DB    = mean(db_marks,    na.rm = TRUE),
  ML    = mean(ml_marks,    na.rm = TRUE)
)
cat("Subject Means:\n"); print(round(means, 2))

# Replace NA with respective subject mean
math_marks[is.na(math_marks)]   <- round(means["Math"])
stats_marks[is.na(stats_marks)] <- round(means["Stats"])
prog_marks[is.na(prog_marks)]   <- round(means["Prog"])
db_marks[is.na(db_marks)]       <- round(means["DB"])
ml_marks[is.na(ml_marks)]       <- round(means["ML"])

# Verify no NAs remain
cat("NAs after replacement:", sum(is.na(math_marks)), "\n")


# Task 5: Combine all 5 subject vectors into a matrix
# Rows = students (120), Columns = subjects (5)
marks_matrix <- cbind(math_marks, stats_marks, prog_marks, db_marks, ml_marks)
colnames(marks_matrix) <- c("Mathematics", "Statistics", "Programming", "Database", "MachineLearning")
rownames(marks_matrix) <- paste0("S", student_ids)

cat("Matrix dimensions:", nrow(marks_matrix), "rows x", ncol(marks_matrix), "cols\n")
# Output: Matrix dimensions: 120 rows x 5 cols

# 1. Extract marks of Student 10
cat("\nStudent 10 marks:\n")
print(marks_matrix[10, ])

# 2. Extract all Programming marks
cat("\nFirst 10 Programming marks:\n")
print(head(marks_matrix[ , "Programming"], 10))

# 3. Add 5 grace marks to entire matrix
marks_matrix_graced <- marks_matrix + 5
# Cap at 100 (marks can't exceed 100)
marks_matrix_graced[marks_matrix_graced > 100] <- 100
cat("\nGrace marks added (capped at 100). Student 1 before/after:\n")
cat("Before:", marks_matrix[1, ], "\n")
cat("After :", marks_matrix_graced[1, ], "\n")

# 4. Total marks per student using rowSums()
total_marks <- rowSums(marks_matrix)
cat("\nTotal marks - first 5 students:\n")
print(head(total_marks, 5))

# 5. Subject averages using colMeans()
subject_averages <- colMeans(marks_matrix)
cat("\nSubject-wise Averages:\n")
print(round(subject_averages, 2))


# Task 6: Create a comprehensive list for student records
percentage <- round((total_marks / 500) * 100, 2)

student_record <- list(
  StudentIDs   = student_ids,
  Section      = sections,
  Gender       = gender,
  MarksMatrix  = marks_matrix,
  TotalMarks   = total_marks
)

# Access individual elements
cat("Number of students:", length(student_record$StudentIDs), "\n")
cat("First student ID  :", student_record$StudentIDs[1], "\n")
cat("First student total marks:", student_record$TotalMarks[1], "\n")

# Add a new element: Percentage
student_record$Percentage <- percentage
cat("\nElement added: Percentage\n")
cat("First student percentage:", student_record$Percentage[1], "%\n")

# Remove one element using NULL
student_record$MarksMatrix <- NULL
cat("MarksMatrix removed. Remaining elements:\n")
cat(names(student_record), "\n")

# Check structure
str(student_record)

# Task 7: Build the full data frame
percentage <- round((total_marks / 500) * 100, 2)

student_df <- data.frame(
  StudentID    = student_ids,
  Section      = sections,
  Gender       = gender,
  Mathematics  = math_marks,
  Statistics   = stats_marks,
  Programming  = prog_marks,
  Database     = db_marks,
  MachineLrn   = ml_marks,
  Total        = total_marks,
  Percentage   = percentage
)

cat("Data frame created:", nrow(student_df), "rows x", ncol(student_df), "cols\n")
head(student_df, 3)

# 1. Extract female students
female_students <- student_df[student_df$Gender == "Female", ]
cat("\nFemale students count:", nrow(female_students), "\n")

# 2. Extract Section B students
section_b <- student_df[student_df$Section == "B", ]
cat("Section B students count:", nrow(section_b), "\n")

# 3. Extract students with Percentage > 75
high_pct <- student_df[student_df$Percentage > 75, ]
cat("Students with % > 75:", nrow(high_pct), "\n")

# 4. Select only ID and Percentage columns
id_pct <- student_df[ , c("StudentID", "Percentage")]
cat("\nID and Percentage (first 5):\n")
print(head(id_pct, 5))


# Task 8.1: Add 5 new students using rbind()
new_students <- data.frame(
  StudentID   = 121:125,
  Section     = c("A", "B", "C", "A", "B"),
  Gender      = c("Male", "Female", "Male", "Female", "Male"),
  Mathematics = c(88, 72, 90, 65, 78),
  Statistics  = c(85, 68, 88, 70, 74),
  Programming = c(80, 75, 92, 60, 82),
  Database    = c(90, 70, 85, 72, 79),
  MachineLrn  = c(86, 66, 91, 68, 77),
  Total       = c(429, 351, 446, 335, 390),
  Percentage  = c(85.8, 70.2, 89.2, 67.0, 78.0)
)

student_df_extended <- rbind(student_df, new_students)
cat("After rbind() - Total rows:", nrow(student_df_extended), "\n")
# Output: After rbind() - Total rows: 125

# Task 8.2: Add Attendance column using cbind()
set.seed(55)
attendance_vec <- sample(60:100, nrow(student_df_extended), replace = TRUE)
student_df_extended <- cbind(student_df_extended, Attendance = attendance_vec)
cat("After cbind() - Total columns:", ncol(student_df_extended), "\n")

# Task 8.3: Alternative method using data.frame()
student_df_alt <- data.frame(student_df_extended,
                             Remarks = ifelse(student_df_extended$Percentage >= 75, "Pass", "Needs Improvement")
)
cat("Alternative method - columns:", colnames(student_df_alt), "\n")

# Task 9: Convert Section and Gender to factors
student_df$Section <- factor(student_df$Section)
student_df$Gender  <- factor(student_df$Gender)

# Check levels
cat("Section levels:", levels(student_df$Section), "\n")
# Output: Section levels: A B C
cat("Gender levels :", levels(student_df$Gender), "\n")
# Output: Gender levels : Female Male

# Rename factor levels (optional relabeling)
levels(student_df$Section) <- c("Section-A", "Section-B", "Section-C")
cat("Renamed Section levels:", levels(student_df$Section), "\n")

# Reset to original labels for further analysis
levels(student_df$Section) <- c("A", "B", "C")

# Count students per section
cat("\nStudents per section:\n")
print(table(student_df$Section))


# Task 10: Create ordered Grade factor based on Percentage
student_df$Grade <- cut(
  student_df$Percentage,
  breaks = c(-Inf, 50, 70, 85, Inf),
  labels = c("D", "C", "B", "A"),
  right  = TRUE
)

# Convert to ordered factor
student_df$Grade <- factor(student_df$Grade,
                           levels  = c("D", "C", "B", "A"),
                           ordered = TRUE
)

# Verify ordered factor
is.ordered(student_df$Grade)  # TRUE
cat("Grade levels (ordered):", levels(student_df$Grade), "\n")
# Output: Grade levels (ordered): D C B A

# Grade frequency distribution
cat("\nGrade Distribution:\n")
print(table(student_df$Grade))

# Proportion table
cat("\nGrade Proportions:\n")
print(round(prop.table(table(student_df$Grade)) * 100, 1))

# Task 11: Split data by Section
section_split <- split(student_df, student_df$Section)

# Calculate mean percentage of each section
cat("=== Section-wise Mean Percentage ===\n")
for (sec in names(section_split)) {
  mean_pct <- mean(section_split[[sec]]$Percentage)
  cat("Section", sec, ": Mean %", round(mean_pct, 2), "\n")
}

# More concise with sapply:
section_means <- sapply(section_split, function(s) round(mean(s$Percentage), 2))
cat("\nSapply result:\n")
print(section_means)
cat("Best performing section:", names(which.max(section_means)), "\n")


# Task 12: Use by() to calculate averages per gender and per section

# Average marks per gender
cat("=== Average Marks per Gender ===\n")
gender_avgs <- by(student_df$Percentage, student_df$Gender, mean)
print(round(gender_avgs, 2))

# Average marks per section
cat("\n=== Average Marks per Section ===\n")
section_avgs <- by(student_df$Percentage, student_df$Section, mean)
print(round(section_avgs, 2))

# by() for multiple statistics per group (use a function)
cat("\n=== Detailed Stats per Gender ===\n")
by(student_df$Percentage, student_df$Gender, function(x) {
  c(Mean = round(mean(x), 2),
    Median = round(median(x), 2),
    SD = round(sd(x), 2),
    Min = min(x), Max = max(x))
})

cat("======================================\n")
cat("     STUDENT PERFORMANCE REPORT       \n")
cat("======================================\n")

cat("\n--- Subject-wise Averages ---\n")
print(round(colMeans(marks_matrix), 2))

cat("\n--- Section-wise Performance ---\n")
print(round(by(student_df$Percentage, student_df$Section, mean), 2))

cat("\n--- Gender-wise Analysis ---\n")
print(round(by(student_df$Percentage, student_df$Gender, mean), 2))

cat("\n--- Grade Distribution ---\n")
print(table(student_df$Grade))

cat("\n--- Top 5 Students ---\n")
top5 <- head(student_df[order(-student_df$Percentage), 
                        c("StudentID", "Section", "Gender", "Total", "Percentage", "Grade")], 5)
print(top5)

cat("\n======================================\n")



