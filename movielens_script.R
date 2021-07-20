### Capstone Movie Lens


### EDX Code ###
################################################################################
#
# Create edx set, validation set (final hold-out test set)
#
################################################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings-predicted_ratings)^2,na.rm=T))
}


################################################################################
#
## Data exploration
#
################################################################################

# Number of rows
nrow(edx)
# Number of columns or variables
ncol(edx)
# Variables and their class
str(edx)
# Summary of the variables
summary(edx) 

# The variable timestamp is of class integer, but it needs to be
# parsed into datetime format, se we need the lubridate library
# that will help to do it in an easier way.

class(edx$timestamp) # This is in integer format
edx$timestamp <- as_datetime(edx$timestamp)

# Also the validation set
validation$timestamp <- as_datetime(validation$timestamp)

# Check formatted variables is in new POSIXt format
class(edx$timestamp)
class(validation$timestamp)


# Initially the variables userId, movieId, timestamp may not be meaningful
# but will be used as identification and aggregation variables
# in order to expand the analysis and have a better organization of the datasets.

# The title variable has a format of "title (year)",
# so we need to create two columns, for title and for year
# and apply this to both edx and validation sets

# We can use RegEx or, given that the year component of the title variable is
# in the last part of the string, as (xxxx), we can subtract it without the parenthesis

edx <- edx %>% mutate(year = as.numeric(str_sub(title, -5,-2)))
validation <- validation %>% mutate(year = as.numeric(str_sub(title, -5,-2)))

# We also need to subtract the title name from title and create the title_name variable
edx <- edx %>% mutate(title_name = as.character(str_sub(title, end = -8)))
validation <- validation %>% mutate(title_name = as.character(str_sub(title, end = -8)))

# The rating date can also be separated for better understanding
edx$rating_year <- format(edx$timestamp, "%Y")
edx$rating_month <- format(edx$timestamp, "%m")

validation$rating_year <- format(validation$timestamp, "%Y")
validation$rating_month <- format(validation$timestamp, "%m")

# Genres are also grouped by different names in the same movie title
# a movie can have multiple genre classifications, so we need to separate them
# Also, trying to manipulate data with such large strings and classifications
# can become a problem with memory management

edx <- edx %>% separate_rows(genres, sep="\\|")
validation <- validation %>% separate_rows(genres, sep="\\|")

################################################################################
#
## Data Analysis
#
################################################################################

#Users
edx %>% select(userId) %>% 
  n_distinct()
# There are 69,878 unique users

# Titles
edx %>% select(title) %>% 
  n_distinct()
# there are 10,676 unique titles

# Ratings
edx %>% select(rating) %>% 
  group_by(rating) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

# The most common rating is 4 with 2,588,430 count, followed by
# 3 and 5.

# Plot ratings
# Bar plot
edx %>% select(rating) %>% 
  group_by(rating) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  ggplot(aes(x = rating, y = n)) + geom_bar(stat = "identity",
                                            aes(fill = rating)) +
  labs(x = "Rating",
       y = "Rating count",
       title = "Ratings")

# It seems that people usually rate movies in whole numbers (i.e. 1,2,3,4,5 vs 0.5,1.5,2.5, etc.)

# Ratings by year
edx %>% select(rating, rating_year) %>% 
  group_by(rating_year) %>% 
  summarise(mean_rating = mean(rating)) %>% 
  arrange(rating_year)

# There seems to by a correlation between rating year and average ratings
# which started pretty high, 4.33, but it has normalized between
# 3.4 and 3.6 band.
# We need to cast the variable rating_year into numeric for the smoothing process.

edx %>% select(rating, rating_year) %>% 
  group_by(rating_year) %>% 
  summarise(mean_rating = mean(rating)) %>% 
  ggplot(aes(x = as.numeric(rating_year), y = mean_rating)) +
  geom_point() +
  geom_smooth()

# If we do the same with movie ratings, there seems to be a preference for
# classic movies, around 1931-1962
edx %>% select(rating, year) %>% 
  group_by(year) %>% 
  summarise(mean_rating = mean(rating)) %>% 
  arrange(-mean_rating)

# And movies in general get lower ratings over time or year of release

edx %>% select(rating, year) %>% 
  group_by(year) %>% 
  summarise(mean_rating = mean(rating)) %>% 
  ggplot(aes(x = as.numeric(year), y = mean_rating)) +
  geom_point() +
  geom_smooth()


# The average rating doesn't change over time, but it can be an effect of
# 4 being the most common rating.
# It seems that 1995 had the best ratings overall.

edx %>% select(title_name, rating) %>% 
  group_by(title_name) %>% 
  summarise(mean_rating = mean(rating),
            ratings = n()) %>% 
  arrange(-ratings)

# If we concentrate in the top 5 rated movies, there is a 
# downward trend in time for a decrease in average ratings until  around 2005,
# then a considerable increase in the avg rating for the next years.
# Only Toy Story and True Lies have a descending rating in 2009.

edx %>% select(title_name, rating_year, rating) %>% 
  group_by(title_name) %>% 
  filter(title_name %in% c("Forrest Gump",
                           "Toy Story",
                           "Jurassic Park",
                           "True Lies",
                           "Aladdin")) %>% 
  group_by(title_name, rating_year) %>% 
  summarise(mean_rating = mean(rating)) %>% 
  ggplot(aes(x = rating_year, y = mean_rating, color = title_name,
             group = title_name)) +
  geom_line()

# We can also check that there is a correlation between number of ratings
# and mean rating, which shows a trend into the values between 3.5 and 4,
# the more ratings the movie has.
edx %>% select(title_name, rating) %>% 
  group_by(title_name) %>% 
  summarise(mean_rating = mean(rating), ratings = n()) %>% 
  ggplot(aes(x = ratings, y = mean_rating)) +
  geom_point() +
  geom_smooth()


# Movies by year
# The most amount of reviews by year of release is 1995, followed by 1994 and 1996.
edx %>%  select(year) %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

# If we arrange by frequency, 10 years have half the reviews and all of them are within
# 1993 and 2002.

edx %>%  select(year) %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)) %>%
  arrange(-n)

# Arranging by year of release, we can see more clearly the concentration of
# the movie ratings in time.
edx %>%  select(year) %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)) %>% 
  ggplot() +
  geom_bar(aes(x = year,
               y = n),
               stat = "identity") +
  labs( x = "Title release year",
        y = "Ratings count")

# in the case of specific titles
edx %>% select(title_name) %>% 
  group_by(title_name) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)) %>% 
  arrange(-n)

# Forrest Gump has the most reviews with 124,316 followed by
# Toy Story and Jurassic Park

# In the case of genres there is a considerable preference for some
# such as Drama, Comedy and Action
edx %>% group_by(genres) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = genres, y = n)) + 
  geom_bar(stat = "identity")+ 
  labs(x = "Genre", y = "Reviews")

# In the case of users, the number of ratings is around 100
edx %>% count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, binwidth=0.2, color="black", show.legend = FALSE) + 
  scale_x_log10() + 
  labs(x = "Number of ratings", y = "Number of users")

################################################################################
#
## Data Modeling
#
################################################################################

# First we create a test set to train the models, according to the specifications
set.seed(1)
test_index <- createDataPartition(y = edx$rating, 
                                  times = 1,
                                  p = 0.2,
                                  list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

# Remove probable coincidences from the other sets so it doesn't repeat values

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# First Model, Average or Naive Average
# The average movie rating is 3.5269
mu <- mean(train_set$rating)
mu

# The RMSE for mu is not really good, this can be affected by a lot of variables
# and considerations, such as the trend in time or preference for newer or older movies
rmse_mu <- RMSE(mu, test_set$rating)
rmse_mu

# Create a table to store the results
rmse_results <- tibble(method = "Average", RMSE = rmse_mu)

# Least Squares tries to create a model taking into account
# some other variables, in this case we will predict our value Y^_i
# with some values b_i, which in this case will be defined as the
# mean difference between the actual rating in the training set vs 
# the mean value (ls_mu)
ls_mu <- mean(train_set$rating)
movie_mu <- train_set %>% group_by(movieId) %>% 
  summarize(b_i = mean(rating - ls_mu))

# Which gives us the following values
movie_mu

# Visualizing it in an histogram, most of the values are near 0, with a
# skewness towards negative values.
movie_mu %>% qplot(b_i,
                   geom= "histogram", 
                   bins = 10,
                   data = .)

# Now we try to predict ratings given our model LS
predicted_ratings <- ls_mu + test_set %>% 
  left_join(movie_mu, by="movieId") %>% 
  pull(b_i)

# And we calculate the RMSE to check if it is better that the simple average
rmse_ls <- RMSE(predicted_ratings , test_set$rating)
# With a value of 0.9409 it is a little bit better that a simple average
rmse_ls

# We store the results in the rmse_results tibble
rmse_results <- bind_rows(rmse_results, tibble(method = "Least Squares Movie",
                                               RMSE = rmse_ls))

# For the third model we will use user effects
user_avg <- train_set %>%
  group_by(userId) %>% 
  left_join(movie_mu, by = "movieId") %>%
  summarise(b_u = mean(rating - movie_mu - b_i))

predicted_ratings <- predicted_ratings + test_set %>% 
  left_join(user_avg, by = "userId") %>% 
  pull(b_u)

RMSE(predicted_ratings , test_set$rating)


## RMSE of the validation set

valid_pred_rating <- validation %>%
  left_join(movie_mu, by = "movieId" ) %>% 
  left_join(user_avg , by = "userId") %>%
  mutate(pred = ls_mu + b_i + b_u ) %>%
  pull(pred)

model_valid <- RMSE(validation$rating, valid_pred_rating)
model_valid

rmse_results <- bind_rows(rmse_results, 
                           tibble(Method = "Validation Results" , RMSE = model_3_valid))
rmse_results
