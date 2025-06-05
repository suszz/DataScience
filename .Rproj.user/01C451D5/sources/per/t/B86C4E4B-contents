# Name: Shushil Rana
# ID: [30476851]

#1list containing a string, a number, and a logical value

my_list <- list("Hello", 42, TRUE)

#2 check if a number is divisible by 3(change x for variablity)
x <- 15
if (x %% 3 == 0) {
  print("Divisible by 3")
} else {
  print("Not divisible by 3")
}


#3 Printing vector of first 9 prime numbers
prime_vector <- c(2, 3, 5, 7, 11, 13, 17, 19, 23)
print(prime_vector)


#4 Logical operation in implementation
result <- c(TRUE, FALSE) | c(FALSE, TRUE)
print(result)

#5 function cimputation where domain of x is set between -10 and 10
x_vals <- -10:10
f_vals <- numeric(length(x_vals))

for (i in seq_along(x_vals)) {
  x <- x_vals[i]
  if (x < -3) {
    f_vals[i] <- 2 * x^2 - 5 * x - 2
  } else {
    f_vals[i] <- x^2 + 5 * x + 1
  }
}
print(data.frame(x = x_vals, f_x = f_vals))

# 6using for loop to compute y[i] = 0.62^i + 0.95^(i - 1) for i = 1 to 10
n <- 10
y <- numeric(n)
for (i in 1:n) {
  y[i] <- 0.6^i + 0.95^(i-1)
}
print(y)


# 7calculkating sum of 9th column (am) from mtcars
df <- mtcars
y <- sum(df[[9]])
print(y)

#8 Work with vector, matrix, list, dataframe

v <- c("dog", "cat", "chicken", "rabbit", "cow", "horse")
m <- matrix(1:20, nrow = 4, ncol = 5)
l <- list(
  list(method = "knn", repetitions = 40, accuracy = 0.67, std_err = 0.04),
  list(method = "svm", repetitions = 20, accuracy = 0.78, std_err = 0.12)
)
d <- data.frame(
  country = c("Australia", "New Zealand", "India"),
  battingAverage = c(25.6, 24.8, 29.3),
  bowlingAverage = c(22.1, 21.6, 25.2)
)

# a) getting row 2 column 5
element <- m[2, 5]
print(element)

# b) for 1st and 3rd element
print(v[c(1, 3)])

# c) Print 2nd element on list
print(l[[2]])

# d) fetch 3rd row of dataframe
print(d[3, ])

# basic function that returns 4x^2 + x + 5
basic <- function(x) {
  return(4 * x^2 + x + 5)
}
print(basic(2))


#10
default <- function(x, y = FALSE) {
  if (y) {
    return(1 * x + 3 * x)
  } else {
    return(1)
  }
}
print(default(4, TRUE))
print(default(4))  


















# Create the data frame with the given data
data <- data.frame(
  Age = c(45, 60, 30, 70, 39, 50, 66, 25),
  Service = c(10, 8.5, 4, 7, 9, 6, 7.2, 5),
  Study = c(10, 9.2, 4.5, 8, 9.8, 6, 7.5, 6),
  Promotion = c(5, 3, 2, 1, 4, 6, 3, 2)
)

# Function to extract rows where Age is between 39 and 66 (inclusive)
extractRows <- function(df) {
  # Logical filter for Age between 39 and 66
  result <- subset(df, Age >= 39 & Age <= 66)
  return(result)
}

# Call the function
filtered_data <- extractRows(data)

# Display the result
print("Filtered rows where Age is between 39 and 66 inclusive:")
print(filtered_data)



























