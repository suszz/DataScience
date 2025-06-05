data <- data.frame(
  Age = c(45, 60, 30, 70, 39, 50, 66, 25),
  Service = c(10, 8.5, 4, 7, 9, 6, 7.2, 5),
  Study = c(10, 9.2, 4.5, 8, 9.8, 6, 7.5, 6),
  Promotion = c(5, 3, 2, 1, 4, 6, 3, 2)
)

#function extractRows to extract data where Age is between 39 and 66
extractRows <- function(df) {
  result <- subset(df, Age >= 39 & Age <= 66)
  return(result)
}

#function call
filtered_data <- extractRows(data)

#printing result
print("Filtered rows where Age is between 39 and 66 inclusive:")
print(filtered_data)







data <- data.frame(
  Age = c(45, 60, 30, 70, 39, 50, 66, 25),
  Service = c(10, 8.5, 4, 7, 9, 6, 7.2, 5),
  Study = c(10, 9.2, 4.5, 8, 9.8, 6, 7.5, 6),
  Promotion = c(5, 3, 2, 1, 4, 6, 3, 2)
)

extractProRatioRows <- function(df) {
  df[df$Study / df$Promotion > 0.91, ]
}

filtered <- extractProRatioRows(data)
print("Filtered:")
print(filtered)





data <- data.frame(
  Age = c(45, 60, 30, 70, 39, 50, 66, 25),
  Service = c(10, 8.5, 4, 7, 9, 6, 7.2, 5),
  Study = c(10, 9.2, 4.5, 8, 9.8, 6, 7.5, 6),
  Promotion = c(5, 3, 2, 1, 4, 6, 3, 2)
)

rowNumber <- function(df) {
  difference <- df$Service - df$Study
  
#Locating the index for max difference maximum difference
  row_num <- which.max(difference)
  
  return(row_num)
}

max_diff_row <- rowNumber(data)
print(paste("Row number with max(Service - Study) is:", max_diff_row))

#First, it calculates the difference between Service and Study for each row.
#Then, it looks for the row where this difference is the biggest.
#Finally, it returns the row number of that employee.





data <- data.frame(
  Age = c(45, 60, 30, 70, 39, 50, 66, 25),          
  Service = c(10, 8.5, 4, 7, 9, 6, 7.2, 5),           
  Study = c(10, 9.2, 4.5, 8, 9.8, 6, 7.5, 6),     
  Promotion = c(5, 3, 2, 1, 4, 6, 3, 2)
)

deepMean <- function(df) {
#filtering rows where Age is greater than 39
filtered_df <- df[df$Age > 39, ]
  
# mean of the Service column for the filtered rows
mean_service <- mean(filtered_df$Service)
return(mean_service)
}

mean_service_result <- deepMean(data)
print(paste("Mean Service (only for Age > 39):", mean_service_result))








































