# Change working directory
setwd("E:/NM/Sem 3/Prob & Stats/R_project")

# Read marks csv file
df = read.csv("Book1.csv")

#-------------------------------------------------------------------------------

# Calculate Grade score for each subject
df$Grade_Score <- df$Grade.Points * df$Credits

print("Students Data: ")
print(df)

#-------------------------------------------------------------------------------

# Calculate the sum of Grade_Score for each student
student_summary <- aggregate(Grade_Score ~ Hallticket.No, data = df, FUN = sum)

# Calculate the total Credits for each student
student_summary$Total_Credits <- aggregate(Credits ~ Hallticket.No, data = df, FUN = sum)$Credits

# Calculate SGPA for each student
student_summary$SGPA <- student_summary$Grade_Score / 19

# Round SGPA to 2 decimal places in the student_summary data frame
student_summary$SGPA <- round(student_summary$SGPA, 2)

#-------------------------------------------------------------------------------

# Count the number of 'F' grades for each student
failed_subjects <- aggregate(Grade ~ Hallticket.No, data = df, FUN = function(x) sum(x == 'F'))

# Rename the columns in the failed_subjects data frame
colnames(failed_subjects) <- c("Hallticket.No", "Failed_Subjects")

# Merge the failed_subjects data frame with the student_summary data frame based on Hallticket.No
student_summary <- merge(student_summary, failed_subjects, by = "Hallticket.No")

#-------------------------------------------------------------------------------

# Sort student_summary by SGPA in descending order
student_summary <- student_summary[order(student_summary$SGPA, decreasing = TRUE), ]

# Reset the row names to start from 1
rownames(student_summary) <- 1:nrow(student_summary)

cat("\n\n")
print("Students Summary: ")
print(student_summary)

#-------------------------------------------------------------------------------

# Display the 5 toppers details
cat("\n\n\n")
print("Semester Toppers: ")
print(head(student_summary[,-which(names(student_summary)=='Failed_Subjects')], 5))

#-------------------------------------------------------------------------------

# Display the subject-wise results summary

# Create an empty data frame to store the details
toppers_details <- data.frame(SUBJECTS = character(0), ATTENDED = numeric(0), PASSED = numeric(0),
                              PERCENTAGE = numeric(0), TOPPERS = character(0))

# List of subjects
subjects <- unique(df$Subject.Name)

# Loop through each subject
for (subject in subjects) {
  # Filter the data frame for the current subject
  subject_df <- df[df$Subject.Name == subject, ]
  
  # Count the total number of students who attended the subject
  attended_students_count <- nrow(subject_df)
  
  # Calculate the number of students who passed the subject (excluding 'F' grades)
  passed_students_count <- sum(subject_df$Grade != 'F' & subject_df$Grade.Points > 0)
  
  # Calculate the percentage of students who passed the subject
  percentage <- (passed_students_count / attended_students_count) * 100
  
  # Find the top 1 topper for the subject
  top1_index <- which.max(subject_df$Grade.Points)
  topper_hallticket <- subject_df$Hallticket.No[top1_index]
  topper_grade <- subject_df$Grade[top1_index]
  
  # Create a data frame with the subject's details
  subject_details <- data.frame(
    SUBJECTS = subject,
    ATTENDED = attended_students_count,
    PASSED = passed_students_count,
    PERCENTAGE = percentage,
    TOPPERS = paste(topper_hallticket, "(", topper_grade, ")", collapse = ", ")
  )
  
  # Append the subject details to the toppers_details data frame
  toppers_details <- rbind(toppers_details, subject_details) }

# Display the toppers' details for all subjects
cat("\n\n")
print("Subject-Wise Summary: ")
print(toppers_details)

#-------------------------------------------------------------------------------

# Calculate the number of students who attended the exam
attended_students_count <- length(unique(df$Hallticket.No))

# Count the number of students who passed all subjects (0 failed subjects)
passed_all_subjects_count <- sum(student_summary$Failed_Subjects == 0)

# Count the number of students who failed in 1 subjects 
one_subject_failed_count <- sum(student_summary$Failed_Subjects == 1)

# Count the number of students who failed in 2 subjects 
two_subject_failed_count <- sum(student_summary$Failed_Subjects == 2)

# Calculate the pass percentage of class 
pass_percentage <- passed_all_subjects_count *100 / attended_students_count 

cat("\n\n")
print("Class-Wide Summary: ")
cat("Number of Students: ", attended_students_count)
cat("\nPassing percentage: ", pass_percentage)
cat("\nAll subjects passed: ", passed_all_subjects_count)
cat("\nOne subject failed: ", one_subject_failed_count)
cat("\nTwo subjects failed: ", two_subject_failed_count)