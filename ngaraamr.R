
install.packages("tidyverse")
library(tidyverse)

install.packages("gtsummary")
library(gtsummary)

library(readr)
ngaraamr <- read_csv("ngaraamr.csv")

colnames(ngaraamr)

# Step 2: Data Exploration
# Check the structure of the dataset
str(ngaraamr)

# Summary statistics
summary(ngaraamr)

# Check for missing values
colSums(is.na(ngaraamr))
sum(is.na(ngaraamr))

# Step 3: Data Cleaning
# Convert relevant columns to factors (categorical)
ngaraamr$Gender <- as.factor(ngaraamr$Gender)

# Convert Age to numeric 
ngaraamr$Age <- as.numeric(ngaraamr$Age)

summary(ngaraamr$Age)

ngaraamr$Age[ngaraamr$Age == 458] <- 58
ngaraamr$Age[ngaraamr$Age == 3] <- 30
ngaraamr$Age[ngaraamr$Age == 19/02/2000] <- 25

summary(ngaraamr$Age)


#Imputation of age by mean
# Step 1: Convert Age to numeric 
ngaraamr$Age <- as.numeric(ngaraamr$Age)

# Step 2: Calculate the mean age, excluding NA values
mean_age <- mean(ngaraamr$Age, na.rm = TRUE)

# Step 3: Impute NA values with the mean age
ngaraamr$Age[is.na(ngaraamr$Age)] <- mean_age


# Check if there are any remaining NA values in Age
sum(is.na(ngaraamr$Age))

summary(ngaraamr$Age)

#Dropping the Residence column
ngaraamr <- ngaraamr %>% select(-c(Residence))

# Descriptive Statistics
#Age
age_summary <- summary(ngaraamr$Age)
age_mean <- mean(ngaraamr$Age, na.rm = TRUE)
age_median <- median(ngaraamr$Age, na.rm = TRUE)
age_sd <- sd(ngaraamr$Age, na.rm = TRUE)

# Combine summary statistics into a dataframe for easier viewing
age_stats <- data.frame(
  Statistic = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "SD"),
  Value = c(age_summary[1], age_summary[2], age_median, age_mean, age_summary[5], age_summary[6], age_sd)
)

print(age_stats)

age_stats
write.csv(age_stats, "age_stats.csv")


# Step 2: Visualizations
# Histogram
ggplot(ngaraamr, aes(x = Age)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency") +
  theme_minimal()

# Frequency of Gender
# Create the gender_distribution dataframe
gender_distribution <- ngaraamr %>%
  count(Gender) %>%
  mutate(Percentage = n / sum(n) * 100,
         cumulative = cumsum(Percentage),
         midpoint = cumulative - Percentage / 2)

# Create a pie chart
ggplot(gender_distribution, aes(x = "", y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  # White border for clarity
  coord_polar("y") +
  labs(title = "Gender Distribution") +
  theme_void() +  # Removes axes and background
  scale_fill_brewer(palette = "Set1") +  # Use a colorful palette
  geom_text(aes(y = midpoint, label = paste0(n, " (", round(Percentage, 1), "%)")),  # Display counts and percentages
            color = "black", 
            position = position_stack(vjust = 0.5)  # Center the text in each slice
  )


#Education

unique(ngaraamr$Education)

# Count the occurrences of each education level
education_distribution <- ngaraamr %>%
  count(Education) %>%
  mutate(Percentage = n / sum(n) * 100)


ggplot(education_distribution, aes(x = Education, y = n, fill = Education)) +
  geom_bar(stat = "identity", color = "black") +  # Bar colors with black outlines
  labs(title = "Education Distribution", x = "Education Level", y = "Count") +
  theme_minimal() +  # Minimal theme for cleaner look
  scale_fill_brewer(palette = "Set2") +  # Use a colorful palette
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black")  # Add counts and percentages on top of bars


#Residence
# Summarize the Residence data
#residence_distribution <- ngaraamr %>%
#mutate(Residence = ifelse(Residence == "Ngaramtoni", "Ngaramtoni", "Others")) %>%
#count(Residence) %>%
#mutate(Percentage = n / sum(n) * 100)

# Create a bar chart for the residence distribution
#ggplot(residence_distribution, aes(x = Residence, y = n, fill = Residence)) +
#geom_bar(stat = "identity", color = "black") +  # Bar colors with black outlines
#labs(title = "Residence Distribution", x = "Residence", y = "Count") +
#theme_minimal() +  # Minimal theme for cleaner look
#scale_fill_brewer(palette = "Set3") +  # Use a colorful palette
#geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
#vjust = -0.5, color = "black")  # Add counts and percentages on top of bars



#KNOWLEDGE QUESTIONS

#Question 1-Antibiotics are effective for the treatment of bacterial infections

ngaraamr$`Antibiotics are effective for the treatment of bacterial infections`



# Count responses and calculate percentages
antibiotics_distribution <- ngaraamr %>%
  count(`Antibiotics are effective for the treatment of bacterial infections`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the bar plot
ggplot(antibiotics_distribution, aes(x = `Antibiotics are effective for the treatment of bacterial infections`, y = n, fill = `Antibiotics are effective for the treatment of bacterial infections`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Knowledge on Antibiotics Effectiveness", 
       x = "Response", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black")



#Question 2-Mention any Antibiotics you know

ngaraamr$`Mention any Antibiotics you know`

# Split the antibiotics mentioned into a long format
antibiotics_long <- ngaraamr %>%
  select(`Mention any Antibiotics you know`) %>%
  separate_rows(`Mention any Antibiotics you know`, sep = ";") %>%
  mutate(`Mention any Antibiotics you know` = trimws(`Mention any Antibiotics you know`))  # Trim whitespace

# Count occurrences of each antibiotic
antibiotic_counts <- antibiotics_long %>%
  count(`Mention any Antibiotics you know`, name = "Count") %>%
  arrange(desc(Count))

# Check the summarized data
print(antibiotic_counts)

# Create a bar chart for antibiotic mentions
ggplot(antibiotic_counts, aes(x = reorder(`Mention any Antibiotics you know`, -Count), y = Count, fill = `Mention any Antibiotics you know`)) +
  geom_bar(stat = "identity", color = "black") +  # Bar colors with black outlines
  labs(title = "Frequency of Antibiotics Mentioned", x = "Antibiotics", y = "Count") +
  theme_minimal() +  # Minimal theme for cleaner look
  scale_fill_brewer(palette = "Set3") +  # Use a colorful palette
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

#A distribution table

# Split the antibiotics into a long format
antibiotics_long <- ngaraamr %>%
  select(`Mention any Antibiotics you know`) %>%
  separate_rows(`Mention any Antibiotics you know`, sep = ";") %>%
  mutate(`Mention any Antibiotics you know` = trimws(`Mention any Antibiotics you know`))

# Count occurrences of each antibiotic
antibiotic_counts <- antibiotics_long %>%
  count(`Mention any Antibiotics you know`, name = "Count") %>%
  arrange(desc(Count))

# Create a frequency distribution table
total_counts <- sum(antibiotic_counts$Count)
frequency_distribution <- antibiotic_counts %>%
  mutate(Frequency_Percentage = (Count / total_counts) * 100)

# Print the frequency distribution table
print(frequency_distribution)

frequency_distribution

# Export to CSV
write.csv(frequency_distribution, "antibiotic_frequency_distribution.csv", row.names = FALSE)



# Question 3-Antibiotics are effective for the treatment of viral infections

# Count responses and calculate percentages
viral_antibiotics_distribution <- ngaraamr %>%
  count(`Antibiotics are effective for the treatment of viral infections`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the bar plot
ggplot(viral_antibiotics_distribution, aes(x = `Antibiotics are effective for the treatment of viral infections`, 
                                           y = n, 
                                           fill = `Antibiotics are effective for the treatment of viral infections`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Knowledge on Antibiotics Effectiveness for Viral Infections", 
       x = "Response", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black")


# Question 4-Loss of effectiveness of an antibiotic is antibiotic resistance

# Count responses and calculate percentages
resistance_distribution <- ngaraamr %>%
  count(`Loss of effectiveness of an antibiotic is antibiotic resistance`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the bar plot
ggplot(resistance_distribution, aes(x = `Loss of effectiveness of an antibiotic is antibiotic resistance`, 
                                    y = n, 
                                    fill = `Loss of effectiveness of an antibiotic is antibiotic resistance`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Knowledge on Antibiotic Resistance", 
       x = "Response", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black")


# Question 5-Antibiotic resistance increases due to missing an antibiotic dose
ngaraamr$`Antibiotic resistance increases due to missing of an antibiotic dose`

# Count responses and calculate percentages
missing_dose_distribution <- ngaraamr %>%
  count(`Antibiotic resistance increases due to missing of an antibiotic dose`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the bar plot
ggplot(missing_dose_distribution, aes(x = `Antibiotic resistance increases due to missing of an antibiotic dose`, 
                                      y = n, 
                                      fill = `Antibiotic resistance increases due to missing of an antibiotic dose`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Knowledge on Missing Antibiotic Doses and Resistance", 
       x = "Response", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black")


#Question 6-`The more we use antibiotics, the higher the risk that resistance develops`
# Count responses and calculate percentages

ngaraamr$`The more we use antibiotic, the higher the risk that resistance develops`

usage_risk_distribution <- ngaraamr %>%
  count(`The more we use antibiotic, the higher the risk that resistance develops`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the bar plot
ggplot(usage_risk_distribution, aes(x = `The more we use antibiotic, the higher the risk that resistance develops`, 
                                    y = n, 
                                    fill = `The more we use antibiotic, the higher the risk that resistance develops`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Knowledge on Antibiotic Use and Resistance Risk", 
       x = "Response", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black")


#Question 7-Antibiotic resistance can develop due to use of antibiotic without a doctor's prescription

ngaraamr$`Antibiotic resistance can develop due to use of antibiotic without doctor's prescription`

# Count responses and calculate percentages
prescription_use_distribution <- ngaraamr %>%
  count(`Antibiotic resistance can develop due to use of antibiotic without doctor's prescription`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the bar plot
ggplot(prescription_use_distribution, aes(x = `Antibiotic resistance can develop due to use of antibiotic without doctor's prescription`, 
                                          y = n, 
                                          fill = `Antibiotic resistance can develop due to use of antibiotic without doctor's prescription`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Knowledge on Antibiotic Use Without Prescription and Resistance", 
       x = "Response", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black")

#Question 8-After feeling better, one should stop a partially completed antibiotic dose
ngaraamr $`After feeling better, one should stop a partially completed antibiotic dose`

# Count responses and calculate percentages
stop_antibiotic_distribution <- ngaraamr %>%
  count(`After feeling better, one should stop a partially completed antibiotic dose`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the bar plot
ggplot(stop_antibiotic_distribution, aes(x = `After feeling better, one should stop a partially completed antibiotic dose`, 
                                         y = n, 
                                         fill = `After feeling better, one should stop a partially completed antibiotic dose`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Knowledge on Stopping Antibiotics After Feeling Better", 
       x = "Response", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black")

#Question 9-Antibiotic resistance can develop due to misuse of antibiotic

ngaraamr$`Antibiotic resistance can develop due to misuse of antibiotic`

# Count responses and calculate percentages
misuse_distribution <- ngaraamr %>%
  count(`Antibiotic resistance can develop due to misuse of antibiotic`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the bar plot
ggplot(misuse_distribution, aes(x = `Antibiotic resistance can develop due to misuse of antibiotic`, 
                                y = n, 
                                fill = `Antibiotic resistance can develop due to misuse of antibiotic`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Knowledge on Misuse of Antibiotics and Resistance", 
       x = "Response", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black")

#ATTITUDE QUESTIONS
#Question 10-Antibiotics are safe, so they can commonly be used
ngaraamr$`Antibiotics are safe, so they can commonly be used`

# Count responses and calculate percentages
safety_distribution <- ngaraamr %>%
  count(`Antibiotics are safe, so they can commonly be used`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the bar plot
ggplot(safety_distribution, aes(x = `Antibiotics are safe, so they can commonly be used`, 
                                y = n, 
                                fill = `Antibiotics are safe, so they can commonly be used`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Perception of Antibiotic Safety and Common Use", 
       x = "Response", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black")


#Question 11-Without prescription, sale of antibiotics should be banned

ngaraamr$`Without prescription, sale of antibiotics should be banned`

# Count responses and calculate percentages
ban_distribution <- ngaraamr %>%
  count(`Without prescription, sale of antibiotics should be banned`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the bar plot
ggplot(ban_distribution, aes(x = `Without prescription, sale of antibiotics should be banned`, 
                             y = n, 
                             fill = `Without prescription, sale of antibiotics should be banned`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Perception on Banning Antibiotic Sales Without Prescription", 
       x = "Response", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black")


#Question 12-Unused antibiotics can be saved for future use or to give someone else
ngaraamr$`Unused antibiotics can be saved for future use or to give someone else`

# Count responses and calculate percentages
unused_antibiotics_distribution <- ngaraamr %>%
  count(`Unused antibiotics can be saved for future use or to give someone else`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the bar plot
ggplot(unused_antibiotics_distribution, aes(x = `Unused antibiotics can be saved for future use or to give someone else`, 
                                            y = n, 
                                            fill = `Unused antibiotics can be saved for future use or to give someone else`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Perception of Saving Unused Antibiotics", 
       x = "Response", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black")

#Question 13-Do you think antimicrobial resistance is a serious problem

ngaraamr$`Do you think antimicrobial resistance is a serious problem?`

# Count responses and calculate percentages
resistance_problem_distribution <- ngaraamr %>%
  count(`Do you think antimicrobial resistance is a serious problem?`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the bar plot
ggplot(resistance_problem_distribution, aes(x = `Do you think antimicrobial resistance is a serious problem?`, 
                                            y = n, 
                                            fill = `Do you think antimicrobial resistance is a serious problem?`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Perception of Antimicrobial Resistance as a Serious Problem", 
       x = "Response", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black")


#Question 14-Do you believe that antimicrobial resistance affects you personally?
ngaraamr$`Do you believe that antimicrobial resistance affects you personally?`

# Count responses and calculate percentages
personal_effect_distribution <- ngaraamr %>%
  count(`Do you believe that antimicrobial resistance affects you personally?`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the bar plot
ggplot(personal_effect_distribution, aes(x = `Do you believe that antimicrobial resistance affects you personally?`, 
                                         y = n, 
                                         fill = `Do you believe that antimicrobial resistance affects you personally?`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Perception of Personal Impact of Antimicrobial Resistance", 
       x = "Response", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black")


#PRACTISE QUESTION
#Question 15-`Have you ever taken antibiotics without a prescription?`
ngaraamr$`Have you ever taken antibiotics without a prescription?`

# Count responses and calculate percentages
no_prescription_distribution <- ngaraamr %>%
  count(`Have you ever taken antibiotics without a prescription?`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the bar plot
ggplot(no_prescription_distribution, aes(x = `Have you ever taken antibiotics without a prescription?`, 
                                         y = n, 
                                         fill = `Have you ever taken antibiotics without a prescription?`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Responses on Taking Antibiotics Without a Prescription", 
       x = "Response", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black")


#Question 16-Do you complete the full course of antibiotics as prescribed by your healthcare provider?

ngaraamr$`Do you complete the full course of antibiotics as prescribed by your healthcare provider?`

# Count responses and calculate percentages
complete_course_distribution <- ngaraamr %>%
  count(`Do you complete the full course of antibiotics as prescribed by your healthcare provider?`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the bar plot
ggplot(complete_course_distribution, aes(x = `Do you complete the full course of antibiotics as prescribed by your healthcare provider?`, 
                                         y = n, 
                                         fill = `Do you complete the full course of antibiotics as prescribed by your healthcare provider?`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Responses on Completing Full Course of Antibiotics", 
       x = "Response", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black")

#Question 17-Have you ever shared antibiotics with others?
ngaraamr$`Have you ever taken antibiotics without a prescription?`

# Count responses and calculate percentages
no_prescription_distribution <- ngaraamr %>%
  count(`Have you ever taken antibiotics without a prescription?`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the bar plot
ggplot(no_prescription_distribution, aes(x = `Have you ever taken antibiotics without a prescription?`, 
                                         y = n, 
                                         fill = `Have you ever taken antibiotics without a prescription?`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Responses on Taking Antibiotics Without a Prescription", 
       x = "Response", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black")


#Question 18-In the past year, have you asked a healthcare professional for antibiotics even if they were not prescribed?
ngaraamr$`In the past year, have you asked a healthcare professional for antibiotics even if they were not prescribed`

# Count responses and calculate percentages
asked_antibiotics_distribution <- ngaraamr %>%
  count(`In the past year, have you asked a healthcare professional for antibiotics even if they were not prescribed`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the bar plot
ggplot(asked_antibiotics_distribution, aes(x = `In the past year, have you asked a healthcare professional for antibiotics even if they were not prescribed`, 
                                           y = n, 
                                           fill = `In the past year, have you asked a healthcare professional for antibiotics even if they were not prescribed`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Requests for Antibiotics Without Prescription in the Past Year", 
       x = "Response", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black")

#Question 19-Do you use antibiotics for viral infections, such as the common cold or flu?
ngaraamr$`Do you use antibiotics for viral infections, such as the common cold or flu?`

# Count responses and calculate percentages
viral_infections_distribution <- ngaraamr %>%
  count(`Do you use antibiotics for viral infections, such as the common cold or flu?`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the bar plot
ggplot(viral_infections_distribution, aes(x = `Do you use antibiotics for viral infections, such as the common cold or flu?`, 
                                          y = n, 
                                          fill = `Do you use antibiotics for viral infections, such as the common cold or flu?`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Use of Antibiotics for Viral Infections", 
       x = "Response", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black")


# Step 6: Inferential Analysis

#Knowledge dataset
knowledge <- ngaraamr[, 1:12]

# Create the knowledge_level variable
knowledge$knowledge_level <- 0  # Initialize the knowledge_level column to 0

# Step 2: Apply conditions to each column and sum the scores
knowledge$knowledge_level <- knowledge$knowledge_level + 
  # Column 3: If response is anything other than 'None'
  ifelse(knowledge[, 3] != "None", 1, 0) + 
  # Column 5: If response is anything other than 'None'
  ifelse(knowledge[, 5] != "None", 1, 0) + 
  # Column 4: If response is 'Yes'
  ifelse(knowledge[, 4] == "Yes", 1, 0) + 
  # Column 6: If response is 'No'
  ifelse(knowledge[, 6] == "No", 1, 0) + 
  # Column 7: If response is 'Yes'
  ifelse(knowledge[, 7] == "Yes", 1, 0) + 
  # Column 8: If response is 'Yes'
  ifelse(knowledge[, 8] == "Yes", 1, 0) + 
  # Column 9: If response is 'Yes'
  ifelse(knowledge[, 9] == "Yes", 1, 0) + 
  # Column 10: If response is 'Yes'
  ifelse(knowledge[, 10] == "Yes", 1, 0) + 
  # Column 12: If response is 'No'
  ifelse(knowledge[, 11] == "No", 1, 0) + 
  # Column 13: If response is 'Yes'
  ifelse(knowledge[, 12] == "Yes", 1, 0)


# Retain only columns 1, 2, 3, 4, and 14 in the knowledge dataset
knowledge <- knowledge[, c(1, 2, 3, 13)]

colnames(knowledge)[4] <- "knowledge level"
colnames(knowledge)[colnames(knowledge) == "knowledge level"] <- "knowledge_level"

knowledge_1 <- knowledge

# Rename knowledge_level responses to "High" if score is 7 or above, otherwise "Low"
knowledge$knowledge_level <- ifelse(knowledge$knowledge_level >= 7, "Good Knowledge", "Poor Knowledge")

# Rename Age responses to "Below 32" if age is less than 32, otherwise "Above 32"
#32 id the mean age for Ngaramtoni residence
knowledge$Age <- ifelse(knowledge$Age < 32, "Below 32", "Above 32")

# Rename Residence responses to "Ngaramtoni" if response is "Ngaramtoni", otherwise "Others"
#knowledge$Residence <- ifelse(knowledge$Residence == "Ngaramtoni", "Ngaramtoni", "Others")


#Table Summary
knowledge_table <-tbl_summary(knowledge, by = knowledge_level) %>% add_p()

knowledge_table

#Attitude dataset

attitude <- ngaraamr[, c(1, 2, 3, 13, 14, 15, 16, 17)]

# Rename Age responses to "Below 32" if age is less than 32, otherwise "Above 32"
#32 id the mean age for Ngaramtoni residence
attitude$Age <- ifelse(attitude$Age < 32, "Below 32", "Above 32")

# Rename Residence responses to "Ngaramtoni" if response is "Ngaramtoni", otherwise "Others"
#attitude$Residence <- ifelse(attitude$Residence == "Ngaramtoni", "Ngaramtoni", "Others")

#Creating attitude level column

# Initialize attitude_level to 0
attitude$attitude_level <- 0

# Apply the conditions and sum the scores
attitude$attitude_level <- attitude$attitude_level + 
  # Column 4: If response is "I agree" or "I strongly agree"
  ifelse(attitude[, 4] == "I agree" | attitude[, 4] == "I strongly agree", 1, 0) +
  # Column 5: If response is "I agree" or "I strongly agree"
  ifelse(attitude[, 5] == "I agree" | attitude[, 5] == "I strongly agree", 1, 0) +
  # Column 6: If response is "I agree" or "I strongly agree"
  ifelse(attitude[, 6] == "I agree" | attitude[, 6] == "I strongly agree", 1, 0) +
  # Column 7: If response is "I agree" or "I strongly agree"
  ifelse(attitude[, 7] == "I agree" | attitude[, 7] == "I strongly agree", 1, 0) +
  # Column 8: If response is "I agree" or "I strongly agree"
  ifelse(attitude[, 8] == "I agree" | attitude[, 8] == "I strongly agree", 1, 0)

# Select only columns 1, 2, 3, 9 (assuming column 9 is the attitude_level column now)
attitude <- attitude %>%
  select(1, 2, 3, 9)

colnames(attitude)[4] <- "attitude level"
colnames(attitude)[colnames(attitude) == "attitude level"] <- "attitude level"

attitude_1 <- attitude

# Create a new column or modify the existing one to categorize the attitude level
attitude$`attitude level` <- ifelse(attitude$`attitude level` > 3, "Good Attitude", "Poor Attitude")


#Table Summary
attitude_table <-tbl_summary(attitude, by = `attitude level`) %>% add_p()

attitude_table


#Practise dataset

practice <- ngaraamr[, c(1, 2, 3, 18, 19, 20, 21, 22)]

# Rename Age responses to "Below 32" if age is less than 32, otherwise "Above 32"
#32 id the mean age for Ngaramtoni residence
practice$Age <- ifelse(practice$Age < 32, "Below 32", "Above 32")

# Rename Residence responses to "Ngaramtoni" if response is "Ngaramtoni", otherwise "Others"
#practice$Residence <- ifelse(practice$Residence == "Ngaramtoni", "Ngaramtoni", "Others")


#Creating Practice level column
# Initialize the 'practice_level' variable with 0
practice$practice_level <- 0

# Apply the conditions and sum the scores
practice$practice_level <- practice$practice_level + 
  # Column 4: If response is "No"
  ifelse(practice[, 4] == "No", 1, 0) +
  
  # Column 5: If response is "Yes"
  ifelse(practice[, 5] == "Yes", 1, 0) +
  
  # Column 6: If response is "No"
  ifelse(practice[, 6] == "No", 1, 0) +
  
  # Column 7: If response is "No"
  ifelse(practice[, 7] == "No", 1, 0) +
  
  # Column 8: If response is "No"
  ifelse(practice[, 8] == "No", 1, 0)


# Select only columns 1, 2, 3, 4, and 10 from the 'attitude' dataset
practice <- practice %>%
  select(1, 2, 3, 9)

practice_1 <- practice

colnames(practice)[4] <- "practice level"
colnames(practice)[colnames(practice) == "practice level"] <- "practice level"


# Create a new column or modify the existing one to categorize the attitude level
practice$`practice level` <- ifelse(practice$`practice level`  > 3, "Good Practice", "Poor Practice")

#Table Summary
practice_table <-tbl_summary(practice, by = `practice level`) %>% add_p()

practice_table


#Correlation between Knowldege, Attitude and Practice
#using Pearson's correlation 

#create a kap dataset
kap <- cbind(attitude_1, practice_1, knowledge_1)

#Retain three variables

kap <- kap [, c(4, 8, 12)]

pearson_correlation <- cor(kap[, c("knowledge_level", "attitude level", "practice_level")], 
                           method = "pearson", 
                           use = "complete.obs")


# View the Pearson correlation matrix
pearson_correlation

# Save the Pearson correlation matrix as a CSV
write.csv(pearson_correlation, file = "pearson_correlation_matrix.csv")

#interpretation: https://www.researchgate.net/figure/Interpretation-of-Pearson-correlation-coefficient-values_tbl3_338505421


#Step 8: Logistic Regression

#Risky Practices from Practice Questions
practice_qn <- ngaraamr[, c(18, 19, 20, 21, 22)]

practice_2 <- practice_1[, 4]
knowledge_2 <- knowledge_1[, 4]
attitude_2 <- attitude_1

kap_1 <- cbind(attitude_2, practice_2, knowledge_2, practice_qn)

colnames(kap_1) <- make.unique(colnames(kap_1))


# Rename columns by their column index (7, 8, 9, 10, 11)
kap_1 <- kap_1 %>%
  rename(
    `Unprescribed Use` = colnames(kap_1)[7],             # Column 7 renamed to "Unprescribed Use"
    `Incomplete dose` = colnames(kap_1)[8],              # Column 8 renamed to "Incomplete dose"
    `Sharing Antibiotics` = colnames(kap_1)[9],          # Column 9 renamed to "Sharing Antibiotics"
    `Request Without Prescription` = colnames(kap_1)[10], # Column 10 renamed to "Request Without Prescription"
    `Use for Viral Infection` = colnames(kap_1)[11]       # Column 11 renamed to "Use for Oral Infection"
  )

# Replace 'Ndio' with 'Yes' in the 'Unprescribed Use' column
kap_1 <- kap_1 %>%
  mutate(`Unprescribed Use` = recode(`Unprescribed Use`, "Ndio" = "Yes"))

#Renaming the responses
kap_1 <- kap_1 %>%
  mutate(Age = ifelse(Age == "Above 32", 1, ifelse(Age == "Below 32", 0, Age)))

kap_1 <- kap_1 %>%
  mutate(Gender = ifelse(Gender == "M", 1, ifelse(Gender == "F", 0, Gender)))

#kap_1 <- kap_1 %>%
#mutate(Residence = ifelse(Residence == "Ngaramtoni", 1, ifelse(Residence == "Others", 0, Residence)))

kap_1 <- kap_1 %>%
  mutate(Education = ifelse(Education == "None", 0, 
                            ifelse(Education == "Degree, Diploma, Secondary, Primary", 1, Gender)))

kap_1 <- kap_1 %>%
  mutate(`Unprescribed Use` = ifelse(`Unprescribed Use` == "Yes", 1, 
                                     ifelse(`Unprescribed Use` == "No", 0, `Unprescribed Use`)))

kap_1 <- kap_1 %>%
  mutate(`Incomplete dose` = ifelse(`Incomplete dose` == "Yes", 1, 
                                    ifelse(`Incomplete dose` == "No", 0, `Incomplete dose`)))

kap_1 <- kap_1 %>% mutate(`Sharing Antibiotics` = ifelse(`Sharing Antibiotics` == "Yes", 1, 
                                                         ifelse(`Sharing Antibiotics` == "No", 0, `Sharing Antibiotics`)))                                   

kap_1 <- kap_1 %>%
  mutate(`Request Without Prescription` = ifelse(`Request Without Prescription` == "Yes", 1, 
                                                 ifelse(`Request Without Prescription` == "No", 0,
                                                        `Request Without Prescription`)))

kap_1 <- kap_1 %>% mutate(`Use for Viral Infection` = ifelse(`Use for Viral Infection` == "Yes", 1, 
                                                             ifelse(`Use for Viral Infection` == "No", 0,
                                                                    `Use for Viral Infection`)))


# Performing Logistic Regression
# Unprescribed Use, Incomplete dose, Sharing Antibiotics, Request Without Prescription, Use for Viral Infection
# Independent variables are: Age, Gender, Residence, Education, Attitude level, Knowledge level, Practice level

str(kap_1)

# Make sure categorical variables are factors (if not already)
kap_1$Age <- as.numeric(kap_1$Age)
kap_1$Gender <- as.factor(kap_1$Gender)
#kap_1$Residence <- as.factor(kap_1$Residence)
kap_1$Education <- as.factor(kap_1$Education)
kap_1$`Unprescribed Use` <- as.factor(kap_1$`Unprescribed Use`)
kap_1$`Incomplete dose` <- as.factor(kap_1$`Incomplete dose`)
kap_1$`Sharing Antibiotics` <- as.factor(kap_1$`Sharing Antibiotics`)
kap_1$`Request Without Prescription` <- as.factor(kap_1$`Request Without Prescription`)
kap_1$`Use for Viral Infection` <- as.factor(kap_1$`Use for Viral Infection`)
kap_1$`attitude level` <- as.numeric(kap_1$`attitude level`)
kap_1$knowledge_level <- as.numeric(kap_1$knowledge_level)
kap_1$practice_level <- as.numeric(kap_1$practice_level)



# Logistic regression models for each dependent variable

# 1. Unprescribed Use
model_unprescribed_use <- glm(`Unprescribed Use` ~ Age + Gender + Education + 
                                `attitude level` + knowledge_level + practice_level, 
                              data = kap_1, family = binomial())

# 2. Incomplete Dose
model_incomplete_dose <- glm(`Incomplete dose` ~ Age + Gender + Education + 
                               `attitude level` + knowledge_level + practice_level,
                             data = kap_1, family = binomial())

# 3. Sharing Antibiotics
model_sharing_antibiotics <- glm(`Sharing Antibiotics` ~ Age + Gender + Education + 
                                   `attitude level` + knowledge_level + practice_level, 
                                 data = kap_1, family = binomial())

# 4. Request Without Prescription
model_request_without_prescription <- glm(`Request Without Prescription` ~ Age + Gender + Education + 
                                            `attitude level` + knowledge_level + practice_level, 
                                          data = kap_1, family = binomial())

# 5. Use for Viral Infection
model_use_for_viral_infection <- glm(`Use for Viral Infection` ~ Age + Gender + Education + 
                                       `attitude level` + knowledge_level + practice_level, 
                                     data = kap_1, family = binomial())

# Summarize the models
summary(model_unprescribed_use)
summary(model_incomplete_dose)
summary(model_sharing_antibiotics)
summary(model_request_without_prescription)
summary(model_use_for_viral_infection)


# Step 9: Conclusions
# Extracting and saving model summaries (coefficients) as CSV files

# For model_unprescribed_use
model_unprescribed_use_summary <- summary(model_unprescribed_use)
model_unprescribed_use_coefficients <- as.data.frame(model_unprescribed_use_summary$coefficients)
write.csv(model_unprescribed_use_coefficients, "model_unprescribed_use_summary.csv")

# For model_incomplete_dose
model_incomplete_dose_summary <- summary(model_incomplete_dose)
model_incomplete_dose_coefficients <- as.data.frame(model_incomplete_dose_summary$coefficients)
write.csv(model_incomplete_dose_coefficients, "model_incomplete_dose_summary.csv")

# For model_sharing_antibiotics
model_sharing_antibiotics_summary <- summary(model_sharing_antibiotics)
model_sharing_antibiotics_coefficients <- as.data.frame(model_sharing_antibiotics_summary$coefficients)
write.csv(model_sharing_antibiotics_coefficients, "model_sharing_antibiotics_summary.csv")

# For model_request_without_prescription
model_request_without_prescription_summary <- summary(model_request_without_prescription)
model_request_without_prescription_coefficients <- as.data.frame(model_request_without_prescription_summary$coefficients)
write.csv(model_request_without_prescription_coefficients, "model_request_without_prescription_summary.csv")

# For model_use_for_viral_infection
model_use_for_viral_infection_summary <- summary(model_use_for_viral_infection)
model_use_for_viral_infection_coefficients <- as.data.frame(model_use_for_viral_infection_summary$coefficients)
write.csv(model_use_for_viral_infection_coefficients, "model_use_for_viral_infection_summary.csv")

















