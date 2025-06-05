dfx <- data.frame(
  black = c(4, 2, 3, 1),
  green = c('A', 'C', 'B', 'D'),
  red = c('x', 'y', 'z', 'w'),
  white = c(5, 10, 15, 20),
  purple = c(100, 200, 300, 400)
)

dfy <- data.frame(
  cyan = c(2, 3, 4, 5),
  yellow = c('B', 'C', 'D', 'E'),
  pink = c('y', 'z', 'w', 'v'),
  orange = c(22, 33, 44, 55)
)

library(dplyr)
dataJoin <- function(dfx, dfy) {
  result <- inner_join(dfx, dfy, by = c("black" = "cyan"))
  return(result)
}
joined_result <- dataJoin(dfx, dfy)
print(joined_result)

#b

library(dplyr)
dataLeftJoin <- function(dfx, dfy) {
  # Perform a left join using green from dfx and yellow from dfy
  result <- left_join(dfx, dfy, by = c("green" = "yellow"))
  return(result)
}


dfx <- data.frame(
  black = c(4, 2, 3, 1),
  green = c('A', 'C', 'B', 'D'),
  red = c('x', 'y', 'z', 'w'),
  white = c(5, 10, 15, 20),
  purple = c(100, 200, 300, 400)
)

dfy <- data.frame(
  cyan = c(2, 3, 4, 5),
  yellow = c('B', 'C', 'D', 'E'),
  pink = c('y', 'z', 'w', 'v'),
  orange = c(22, 33, 44, 55)
)

#left join
joined_df <- dataLeftJoin(dfx, dfy)
print(joined_df)

#c

dataJoinFind <- function(dfx, dfy) {
#inner join on red from dfx and pink from dfy
  joined_df <- inner_join(dfx, dfy, by = c("red" = "pink"))
  
#white == 5 and purple
  result <- joined_df %>%
    filter(white == 5) %>%
    pull(purple)  # Extract the 'purple' column value(s)
  
  return(result)
}

dfx <- data.frame(
  black = c(4, 2, 3, 1),
  green = c('A', 'C', 'B', 'D'),
  red = c('x', 'y', 'z', 'w'),
  white = c(5, 10, 15, 20),
  purple = c(100, 200, 300, 400)
)

dfy <- data.frame(
  cyan = c(2, 3, 4, 5),
  yellow = c('B', 'C', 'D', 'E'),
  pink = c('y', 'z', 'w', 'v'),
  orange = c(22, 33, 44, 55)
)
dataJoinFind(dfx, dfy)

library(ggplot2)

simplePlot <- function(df) {
  #scatter plot using black (x-axis) and white (y-axis)
  g <- ggplot(df, aes(x = black, y = white)) +
    geom_point(color = "steelblue", size = 3) +
    labs(
      title = "Scatter Plot of Black vs White",
      x = "Black",
      y = "White"
    ) +
    theme_minimal()
  return(g) 
  }
  
dfx <- data.frame(
  black = c(4, 2, 3, 1),
  green = c('A', 'C', 'B', 'D'),
  red = c('x', 'y', 'z', 'w'),
  white = c(5, 10, 15, 20),
  purple = c(100, 200, 300, 400)
)

#calling variable in g function
g <- simplePlot(dfx)
print(g)






