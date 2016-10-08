
# read the .dat file so as to replace "::" by ";" in R
# ratings.dat
x <- readLines("D:/Upswing Quest/Projects/ml-1m/ml-1m/ratings.dat")
y <- gsub("::", ";", x)
cat(y, file="newRatings.dat", sep="\n")

#users.dat
x <- readLines("D:/Upswing Quest/Projects/ml-1m/ml-1m/users.dat")
y<- gsub("::", ";", x)
cat(y, file="D:/Upswing Quest/Projects/ml-1m/ml-1m/newUsers.dat", sep="\n")

#read movies.dat, I replaced the characters manually for this file.
movies <- read.delim("D:/Upswing Quest/Projects/ml-1m/ml-1m/movies.dat", sep = ";", col.names = c("Id", "Movie", "Genre"), header = FALSE)
View(movies)

#read updated users.dat
users <- read.delim("D:/Upswing Quest/Projects/ml-1m/ml-1m/newUsers.dat", sep = ";", col.names = c("Id", "Gender", "Age", "Occupation", "ZipCode"), header = FALSE)
View(users)

#read ratings.dat
ratings <- read.delim("D:/Upswing Quest/Projects/ml-1m/ml-1m/newRatings.dat", sep = ";", col.names = c("UserId", "MovieId", "Rating", "Timestamp"), header = FALSE)
View(ratings)

library(dplyr)

filterRatings <- ratings %>% 
  group_by(MovieId) %>% 
  summarise_each_(funs = funs(avgRating = "mean", views= "length"), 
                  vars = c("Rating", "UserId")) %>% 
  arrange(MovieId)

# remove unwanted columns introduced due to summarize_each_
filterRatings <- filterRatings[,-c(3,4)]

#merge with movie for Names
filterRatings <- merge(filterRatings, movies, by.x = "MovieId", by.y = "Id")
filterRatings <- filterRatings %>% rename(avgRating = Rating_avgRating)

# round off the Ratings to 2 decimals
filterRatings$avgRating <- round(filterRatings$avgRating, 2)

#arrange by ratings
filterRatings <- filterRatings %>% arrange(desc(avgRating))

#1. Top ten most viewed movies with their movies Name (Ascending or Descending order)
mostViewedMovies <- filterRatings %>% arrange(desc(UserId_views)) %>% head(n=10) %>% select(UserId_views, Movie)
View(mostViewedMovies)

#2. Top twenty rated movies (Condition : The movie should be rated/viewed by at least 40 users)
topratedMovies <- filterRatings %>% 
  filter(UserId_views > 40) %>% 
  head(filterRatings, n=20) %>% 
  select(UserId_views, avgRating, Movie)

View(topratedMovies)

#3. Top twenty rated movies (which is calculated in the previous step) with no of views in the 
#following age group
#(Age group : 1. Young (<20 years),
#2. Young Adult (20-40 years),
#3.adult (> 40years) )

filterRatingsByAgeGroup <- merge(ratings, users, by.x = "UserId", by.y="Id")

filterRatingsByAgeGroup <- merge(filterRatingsByAgeGroup, movies, by.x = "MovieId", by.y="Id")

x <- filterRatingsByAgeGroup %>% group_by(Age, MovieId, Movie) %>% summarize_each_(funs = funs(avgRating = mean, views = length), vars=c("Rating", "UserId"))
x <- x[, -c(5,6)]
x$Rating_avgRating <- round(x$Rating_avgRating, 2)

y <- x %>% group_by(Age, MovieId, Movie, Rating_avgRating) %>% 
       summarize(viewsByAge = sum(UserId_views)) %>%  
       arrange(desc(Rating_avgRating))

View(y)
head(y, n=20)

#4. Top ten critics (Users who have given very low ratings; Condition : The users should have at least 
 #                   rated 40 movies)

View(filterRatingsByAgeGroup)
a <- ratings %>% group_by(UserId) %>% 
  summarize_each_(funs= funs(avgRating = mean, views = length), 
                  vars= c("Rating", "UserId")) %>% 
  filter(views > 40) %>% 
  arrange(avgRating)

a$avgRating <- round(a$avgRating,2)
View(a)

head(a, 10)
