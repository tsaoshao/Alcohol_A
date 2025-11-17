# tidying: Comment.csv, Post.csv, Like.csv, Login.csv
library(tidyverse)
library(lubridate)
library(snakecase)

# data importing
comment <- read_csv("~/Desktop/Alcohol_A/AlcoholData/Comment.csv")
like <- read_csv("~/Desktop/Alcohol_A/AlcoholData/Like.csv")
login <- read_csv("~/Desktop/Alcohol_A/AlcoholData/Login.csv")
post <- read_csv("~/Desktop/Alcohol_A/AlcoholData/Post.csv")

# Renaming variables by removing uppercase and adding underscores
post <- post |>
  rename_with(~ snakecase::to_snake_case(.x))|> 
  mutate(
    # Convert post ID and viewer ID to character to avoid accidental numeric behaviour
    post_id = as.character(post_id),
    viewer_id = as.character(viewer_id),
    # Convert from double to integer type
    post_comments = as.integer(post_comments),
    post_likes = as.integer(post_likes),
    post_views = as.integer(post_views),
    # Convert from double to datetime type
    post_time = parse_date_time(post_time, orders = c("Ymd HMS", "Y-m-d H:M:S", "d/m/Y H:M:S", "Y-m-d", "ymd HMS")),
    sync_time = parse_date_time(sync_time, orders = c("Ymd HMS", "Y-m-d H:M:S", "d/m/Y H:M:S", "Y-m-d", "ymd HMS"))
  )

# For each value of the selected type of case...
post |> 
  group_by(post_id, viewer_id) |>
  # ... calculate the number of distinct values of the selected variable.
  summarise(n_distinct = n_distinct(post_time), .groups = "drop") |>
  # Count number of distinct variable values per type of case: 
  # If 1, then variable has unique value for each case.
  count(n_distinct)

# Splitting post and view information into 2 separate tibbles
# post_sub_1: post information, post_sub_2: viewer information
post_sub_1 <- post |> 
  # Select only post-level variables
  select(post_id : sync_time) |> 
  # Transform tidied post_sub_1 to make it clearer
  distinct(post_id, .keep_all = TRUE) 

post_sub_2 <- post |> 
  select(post_id, viewer_id) |> 
  # Ensure unique (post_id, viewer_id) rows
  distinct(post_id, viewer_id, .keep_all = TRUE)

# Primary key check
post_sub_1 |> 
  count(post_id) |> 
  filter(n > 1)

post_sub_2 |> 
  count(post_id, viewer_id) |> 
  filter(n > 1)

# comment, like, login 
# Comment.csv
comment <- comment |> 
  rename_with(~ snakecase::to_snake_case(.x))

comment |> 
  count(post_id, commenter_id, comment_time, comment_content) |> 
  filter(n > 1)

comment |> 
  count(post_id, commenter_id, comment_time) |> 
  filter(n > 1)



# Like.csv
like <- like |> 
  rename_with(~ snakecase::to_snake_case(.x))|> 
  mutate(liker_id = as.character(liker_id))

like |> 
  count(post_id, liker_id) |> 
  filter(n > 1)

like |> 
  distinct(post_id, liker_id, .keep_all = TRUE)
  

# Login.csv
login <- login |> 
  rename_with(~ snakecase::to_snake_case(.x)) |> 
  mutate(user_id = as.character(user_id))

login |> 
  count(user_id, user_login_time) |> 
  filter(n > 1)




