library(dplyr)
library(ggplot2)
library(readr)
library(plotly)  # for interactivity

# Load datasets
books <- read_csv("books.csv")
pga <- read_csv("RatingPGA.csv") %>% select(-`...1`) %>% rename(Rating = `Book.Rating`)
pgb <- read_csv("RatingPGB.csv") %>% select(-`...1`) %>% rename(Rating = `Book.Rating`)

#ISBN columns to character for proper joining
books$isbn <- as.character(books$isbn)
pga$ISBN <- as.character(pga$ISBN)
pgb$ISBN <- as.character(pgb$ISBN)

# Combining both rating datasets
ratings <- bind_rows(
  pga %>% select(ISBN, Rating),
  pgb %>% select(ISBN, Rating)
)

# Joining with books metadata and filtering missing authors
combined <- ratings %>%
  inner_join(books, by = c("ISBN" = "isbn")) %>%
  filter(!is.na(authors) & authors != "")

# Calculating average rating and count by author
author_summary <- combined %>%
  group_by(authors) %>%
  summarise(
    average_combined_rating = mean(Rating, na.rm = TRUE),
    total_ratings = n()
  ) %>%
  arrange(desc(total_ratings))

#authors with the most ratings
top10_authors <- author_summary %>% slice_max(total_ratings, n = 10)

# ggplot
p <- ggplot(top10_authors, aes(x = reorder(authors, average_combined_rating), y = average_combined_rating,
                               text = paste("Total Ratings:", total_ratings))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Authors by Number of Ratings",
    x = "Author",
    y = "Average Combined Rating"
  ) +
  theme_minimal()

ggplotly(p, tooltip = c("x", "y", "text"))
















library(plotly)
users <- read.csv("users.csv")
pga <- read.csv("RatingPGA.csv")

# Renamed columns to enable merging
colnames(users)[colnames(users) == "User.ID"] <- "User"
colnames(pga)[colnames(pga) == "Book.Rating"] <- "Rating"

# Merging user and rating data
merged <- merge(pga, users, by = "User")

# Filtering ages between 5 and 100
valid_data <- merged[!is.na(merged$Age) & merged$Age >= 5 & merged$Age <= 100, ]

# Creating age groups
valid_data$AgeGroup <- cut(valid_data$Age,
                           breaks = c(0, 18, 30, 45, 60, Inf),
                           labels = c("0-18", "19-30", "31-45", "46-60", "61+"),
                           right = TRUE)

#boxplot using plotly
plot_ly(data = valid_data,
        x = ~AgeGroup,
        y = ~Rating,
        type = "box",
        boxpoints = "all",
        jitter = 0.3,
        pointpos = -1.8,
        marker = list(color = 'rgba(7,40,89,0.5)', size = 4),
        line = list(color = 'rgba(7,40,89,1)')) %>%
  layout(title = " Boxplot of Book Ratings by Age Group",
         xaxis = list(title = "Age Group"),
         yaxis = list(title = "Book Rating"))
















library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(plotly) 

# Loading data
users <- read_csv("users.csv")
pga <- read_csv("RatingPGA.csv") %>% select(-`...1`) %>% rename(Rating = `Book.Rating`)
pgb <- read_csv("RatingPGB.csv") %>% select(-`...1`) %>% rename(Rating = `Book.Rating`)

# Extracting and cleaning country names
users <- users %>%
  mutate(Country = str_trim(str_extract(Location, "[^,]+$"))) %>%
  filter(!is.na(Country) & Country != "")

# Joining ratings with user data
pga_merged <- inner_join(pga, users, by = c("User" = "User-ID"))
pgb_merged <- inner_join(pgb, users, by = c("User" = "User-ID"))

# Summarising average ratings by country
pga_country <- pga_merged %>%
  group_by(Country) %>%
  summarise(avg_rating = mean(Rating, na.rm = TRUE), n = n()) %>%
  filter(n >= 20) %>%
  arrange(desc(avg_rating)) %>%
  slice_head(n = 5)

pgb_country <- pgb_merged %>%
  group_by(Country) %>%
  summarise(avg_rating = mean(Rating, na.rm = TRUE), n = n()) %>%
  filter(n >= 20) %>%
  arrange(desc(avg_rating)) %>%
  slice_head(n = 5)

# plot for PGA
fig_pga <- ggplot(pga_country, aes(x = reorder(Country, avg_rating), y = avg_rating)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Top 5 Countries with Highest Avg Book Ratings (PGA)",
       x = "Country", y = "Average Rating")

ggplotly(fig_pga)

# plot for PGB
fig_pgb <- ggplot(pgb_country, aes(x = reorder(Country, avg_rating), y = avg_rating)) +
  geom_bar(stat = "identity", fill = "salmon") +
  coord_flip() +
  labs(title = "Top 5 Countries with Highest Avg Book Ratings (PGB)",
       x = "Country", y = "Average Rating")

ggplotly(fig_pgb)






















library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)

# Reading data
books <- read_csv("books.csv")
pga <- read_csv("RatingPGA.csv") %>% select(-`...1`) %>% rename(Rating = `Book.Rating`)
users <- read_csv("users.csv")

# Converting formats for join
books$isbn <- as.character(books$isbn)
pga$ISBN <- as.character(pga$ISBN)
users$`User-ID` <- as.integer(users$`User-ID`)

# Converting publication_date
books <- books %>%
  mutate(publication_date = mdy(publication_date))

# Filtering books published after 2000
books_after_2000 <- books %>%
  filter(!is.na(publication_date) & year(publication_date) > 2000)

# Filtering valid ages
users <- users %>% filter(!is.na(Age) & Age >= 5 & Age <= 100)

# Joining ratings, books, and users
pga_merged <- pga %>%
  inner_join(books_after_2000, by = c("ISBN" = "isbn")) %>%
  inner_join(users, by = c("User" = "User-ID"))

# Creating age groups
pga_merged$age_group <- cut(
  pga_merged$Age,
  breaks = c(0, 18, 30, 45, 60, Inf),
  labels = c("0-18", "19-30", "31-45", "46-60", "61+"),
  right = FALSE
)

# Aggregating average rating
pga_age_summary <- pga_merged %>%
  group_by(age_group) %>%
  summarise(average_rating = round(mean(Rating, na.rm = TRUE), 2), .groups = "drop")

#interactive bar chart
p <- ggplot(pga_age_summary, aes(x = age_group, y = average_rating, fill = age_group)) +
  geom_col(width = 0.6, color = "white", alpha = 0.9, show.legend = FALSE) +
  geom_text(aes(label = average_rating), vjust = -0.5, color = "black", size = 4.5) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = " Average Book Rating by Age Group (Post-2000 Books)",
       subtitle = "Dataset: RatingPGA | Filter: Books published after 2000",
       x = "Age Group",
       y = "Average Rating") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)),
    axis.text = element_text(size = 12)
  )
ggplotly(p, tooltip = c("x", "y"))
