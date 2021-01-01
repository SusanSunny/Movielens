#Movielens Project
#Susan Sunny
#30/12/2020

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))
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


#Exploration of Movielens
dim(edx)
head(edx)
dim(validation)

#Graph: Number of movies per year
part<- edx%>%mutate(years=str_extract(title, "\\([0-9]{4}\\)"))%>%
  mutate(years=str_extract(years, "[0-9]{4}"))%>%
  group_by(years)%>%
  summarize(movies_per_year=n_distinct(movieId))%>%
  ungroup()
part%>%
  ggplot(aes(years,movies_per_year))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5))


#Graph: Ratings-per-movie per year
rpm<-edx%>%mutate(years=str_extract(title, "\\([0-9]{4}\\)"))%>%
  mutate(years=str_extract(years, "[0-9]{4}"))%>%
  group_by(movieId)%>%
  summarize(ratings_per_movie=n(), year=unique(years))
rpm%>%
  ggplot(aes(year, ratings_per_movie))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=5))


#Linear Regression: Movie + User + Year + Genre Effect Model 
mu <- mean(validation$rating) # overall average rating 

#Movie Effect 
b_i<- edx%>%
  group_by(movieId)%>%
  summarize(b_i = mean(rating-mu))

#User Effect
b_u<- edx%>%
  left_join(b_i, by = "movieId")%>%
  group_by(userId)%>%
  summarize(b_u = mean(rating-mu-b_i))

#Genre Effect
b_g<- edx%>%
  left_join(b_i, by = "movieId")%>%
  left_join(b_u, by = "userId")%>%
  group_by(genres)%>%
  summarize(b_g = mean(rating-mu-b_i-b_u))

#Year Effect (year in which the movie was released)
b_y<- edx%>%
  left_join(b_i, by = "movieId")%>%
  left_join(b_u, by = "userId")%>%
  left_join(b_g, by= "genres")%>%
  mutate(years=str_extract(title, "\\([0-9]{4}\\)"))%>%
  group_by(years)%>%
  summarize(b_y = mean(rating- mu - b_i - b_u - b_g))


#Validation
#join b_i, b_u, b_g into validation set 
predicted_ratings<- validation%>%
  left_join(b_i, by = "movieId")%>%
  left_join(b_u, by = "userId")%>%
  left_join(b_g, by= "genres")%>%
  mutate(years=str_extract(title, "\\([0-9]{4}\\)"))%>%
  left_join(b_y, by= "years")%>%
  mutate(pred= mu + b_i + b_u + b_g + b_y)%>%
  .$pred 

#calculate RMSE 
iugy_rmse<-RMSE(predicted_ratings, validation$rating)
paste0("RMSE of Movie+User+Genre+Year Model: ", iugy_rmse)

##Use regularization
#calculate best lambda

lambdas<- seq(0,10,0.5) 

#build linear model depending on different lambdas 
rmses<- sapply(lambdas, function(lambda){  
  
  #Movie Effect
  b_i_reg<- edx%>%
    group_by(movieId)%>%
    summarize(b_i_reg = sum(rating-mu)/(n()+lambda)) 
  
  #User Effect
  b_u_reg<- edx%>%
    left_join(b_i_reg, by = "movieId")%>%
    group_by(userId)%>%
    summarize(b_u_reg = sum(rating-mu-b_i_reg)/(n()+lambda))
  
  #Genre Effect
  b_g_reg<- edx%>%
    left_join(b_i_reg, by = "movieId")%>%
    left_join(b_u_reg, by = "userId")%>%
    group_by(genres)%>%
    summarize(b_g_reg = sum(rating-mu-b_i_reg-b_u_reg)/(n()+lambda))
  
  #Year Effect (year in which the movie was released)
  b_y_reg<- edx%>%
    left_join(b_i_reg, by = "movieId")%>%
    left_join(b_u_reg, by = "userId")%>%
    left_join(b_g_reg, by= "genres")%>%
    mutate(years=str_extract(title, "\\([0-9]{4}\\)"))%>%
    group_by(years)%>%
    summarize(b_y_reg = sum(rating-mu-b_i_reg-b_u_reg-b_g_reg)/(n()+lambda))
  
  #join b_i, b_u, b_g into the validation set 
  predicted_ratings_reg<- validation%>%
    left_join(b_i_reg, by = "movieId")%>%
    left_join(b_u_reg, by = "userId")%>%
    left_join(b_g_reg, by= "genres")%>%
    mutate(years=str_extract(title, "\\([0-9]{4}\\)"))%>%
    left_join(b_y_reg, by= "years")%>%
    mutate(pred= mu + b_i_reg + b_u_reg + b_g_reg + b_y_reg)%>%
    .$pred 
  
  #calculate RMSE
  return(RMSE(predicted_ratings_reg, validation$rating))
})

#Plot lambdas against RMSEs in order to find best lambda
plot(lambdas, rmses)
paste0("Lambda with best RMSE: ", lambdas[which.min(rmses)])
best_reg_rmse<-min(rmses)
paste0("RMSE of Final Model: ", best_reg_rmse)

#generate RSMES for Movie, User, Genre, Years, Movie+User, Movie+User+Genre, Movie+User+Genre+Year
predicted_ratings<- validation%>%
  left_join(b_i, by = "movieId")%>%
  left_join(b_u, by = "userId")%>%
  left_join(b_g, by= "genres")%>%
  mutate(years=str_extract(title, "\\([0-9]{4}\\)"))%>%
  left_join(b_y, by= "years")%>%
  mutate(no = mu, i = mu + b_i)%>%
  mutate(iu= mu + b_i + b_u, iug= mu + b_i + b_u + b_g, iugy= mu+ b_i + b_u + b_g+ b_y)%>%
  dplyr::select(no, i, iu, iug, iugy)

#convert to Matrix 
prm<- as.matrix(predicted_ratings)
#calculate RSME
all_rmses<- apply(prm, 2, function(x) RMSE(validation$rating, x))
#sort RMSE and convert to data.table
all_rmses<- sort(all_rmses, decreasing=TRUE)
all_rmses_df<- data.table(Model=names(all_rmses), RMSE=all_rmses)


#add Models and RMSEs in table 
rmse_results<- data.frame(Model=c("Average Rating", "Movie Effect", "Movie+User Effect", "Movie+User+Genre Effect", "Movie+User+Genre+Year Effect", "Regularization+Movie+User+Genre+Year"), 
                          RMSE = c(all_rmses_df$RMSE, best_reg_rmse))
rmse_results %>% knitr::kable()

