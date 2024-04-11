library(dplyr)

household_data <- read.csv('1_Household_Public.csv')
person_data <- read.csv('2_Person_Public.csv')

person_data_filtered <- person_data[!is.na(person_data$HOURS), ]

income_to_midpoint <- function(income_category) {
  switch(as.character(income_category),
         "1" = 4999.5,
         "2" = 17499.5,
         "3" = 29999.5,
         "4" = 42499.5,
         "5" = 62499.5,
         "6" = 87499.5,
         "7" = 124999.5,
         "8" = 174999.5,
         "9" = 224999.5,
         "10" = 250000,
         NA)
}

household_data$Midpoint_Income <- sapply(household_data$INCOME, income_to_midpoint)

# Group by HH_ID, sum the hours, and count the number of persons per household
household_summary <- person_data_filtered %>% 
  group_by(HH_ID) %>% 
  summarise(Total_Hours = sum(HOURS), Num_Persons = n())

# Merge the summary with the household midpoint income data
merged_data <- merge(household_summary, household_data[, c("HH_ID", "Midpoint_Income")], by = "HH_ID")

# Calculate the average hourly wage for each household
merged_data$Average_Hourly_Wage <- merged_data$Midpoint_Income / (merged_data$Total_Hours * 48)

# Export the results to a CSV file
write.csv(merged_data, "average_hourly_wage_per_household.csv", row.names = FALSE)

