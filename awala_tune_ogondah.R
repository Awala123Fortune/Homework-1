## Awala Fortune
## MovieLens Project 
## HarvardX: PH125.9x - Capstone Project

### I. Introduction ###
###II. Method/Analysis###
##Dataset

load("~/Awala Capstone Project/Awala_Fortune_Project.rda")

##Loading library and dataset

library(tidyverse)
library(caret)
library(data.table)
library(kableExtra)
library(lubridate)
library(Matrix.utils)
library(DT)
library(wordcloud) 
library(RColorBrewer)
library(ggthemes) 
library(irlba)
library(recommenderlab)
library(recosystem)
library(h2o)
library(googledrive)
library(stringr)

url <- "https://drive.google.com/drive/folders/1IZcBBX0OmL9wu9AdzMBFUG8GoPbGQ38D?usp=sharing"

dl <- tempfile()
download.file(url, dl)

validation <- readRDS("C:/Users/HP/Downloads/validation.rds")
edx <- readRDS("C:/Users/HP/Downloads/edx.rds")

###III. Results/Discussion###

##A. Data Exploration

head(edx)

class(edx)

glimpse(edx)


dim(edx)

summary(edx)

#create a dataframe "explore_edx_ratings" which contains half star and whole star ratings  from the edx dataset
group <-  ifelse((edx$rating == 1 |edx$rating == 2 | edx$rating == 3 | 
                    edx$rating == 4 | edx$rating == 5) ,
                 "whole_star", 
                 "half_star") 


explore_edx_ratings <- data.frame(edx$rating, group)

#Histogram
ggplot(explore_edx_ratings, aes(x= edx.rating, fill = group)) +
  geom_histogram( binwidth = 0.2) +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
  scale_fill_manual(values = c("half_star"="blue", "whole_star"="red")) +
  labs(x="rating", y="number of ratings", caption = "source data: edx set") +
  ggtitle("histogram : number of ratings per rating")

#summary of five rating count
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5, count) %>%
	arrange(desc(count))

#geom_line showing rating
edx %>%
	group_by(rating) %>%
	summarize(count = n()) %>%
	ggplot(aes(x = rating, y = count)) +
	geom_line()+
  labs(x="rating", y="number of ratings", caption = "source data: edx set") +
  ggtitle("geomplot : number of ratings per count")

#top five title and genres
 edx %>%
  group_by(title, genres) %>%
  summarize(count=n()) %>%
  top_n(5,count) %>%
  arrange(desc(count))


# the data frame top_title contains the top 20 movies which count the major number of ratings
top_title <- edx %>%
  group_by(title) %>%
  summarize(count=n()) %>%
  top_n(20,count) %>%
  arrange(desc(count))
# with the head function i output the top 5 
kable(head(edx %>%
     group_by(title,genres) %>%
     summarize(count=n()) %>%
     top_n(20,count) %>%
     arrange(desc(count)) ,
     5)) %>%
  kable_styling(bootstrap_options = "bordered", full_width = F , position ="center") %>%
  column_spec(1,bold = T ) %>%
  column_spec(2,bold =T) %>%
  column_spec(3,bold=T)

#bar chart of top_title
top_title %>% 
  ggplot(aes(x=reorder(title, count), y=count)) +
  geom_bar(stat='identity', fill="blue") + coord_flip(y=c(0, 40000)) +
  labs(x="", y="Number of ratings") +
  geom_text(aes(label= count), hjust=-0.1, size=3) +
  labs(title="Top 20 movies title based \n on number of ratings" , caption = "source data: edx set")

edx %>%
group_by(movieId) %>%
summarize(count = n()) %>%
filter(count == 1) %>%
left_join(edx, by = "movieId") %>%
group_by(title) %>%
summarize(rating = rating, n_rating = count) %>%
slice(1:20) %>%
knitr::kable()

#An error bar plots for genres with more than 100000 ratings
edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 100000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "error bar plots by genres" , caption = "source data : edx set") +
  theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )

#unique userId and movieId
edx %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

# histogram of number of ratings by movieId
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram( bins=30, color = "black") +
  scale_x_log10() + 
  ggtitle("Movies") +
  labs(subtitle  ="number of ratings by movieId", 
       x="movieId" , 
       y="number of ratings", 
       caption ="source data : edx set") +
  theme(panel.border = element_rect(colour="purple", fill=NA)) 

# histogram of number of ratings by userId
edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram( bins=30, color = "gold") +
  scale_x_log10() + 
  ggtitle("Users") +
  labs(subtitle ="number of ratings by UserId", 
       x="userId" , 
       y="number of ratings") +
  theme(panel.border = element_rect(colour="black", fill=NA))

#ggplot showing timestamp per week
edx %>% 
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Timestamp, time unit : week")+
  labs(subtitle = "average ratings",
       caption = "source data : edx set")

#ggplot showing timestamp per year
edx %>% 
  mutate(date = round_date(as_datetime(timestamp), unit = "year")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Timestamp, time unit : year")+
  labs(subtitle = "average ratings",
       caption = "source data : edx set")

##B.Data Preprocessing

##C. Data transformation

#Edx dataset transformation:usersId and movieId should be treat as factors for some analysis purposes.
edx.copy <- edx
edx.copy$userId <- as.factor(edx.copy$userId)
edx.copy$movieId <- as.factor(edx.copy$movieId)

#SparseMatrix function is used in order to get an output 0f sparse matrix of class dgcMatrix.
# To use this function, the userId & movieId are converted to numeric vectors.
edx.copy$userId <- as.numeric(edx.copy$userId)
edx.copy$movieId <- as.numeric(edx.copy$movieId)
sparse_ratings <- sparseMatrix(i = edx.copy$userId,
                               j = edx.copy$movieId ,
                               x = edx.copy$rating, 
                               dims = c(length(unique(edx.copy$userId)),
                                        length(unique(edx.copy$movieId))),  
                               dimnames = list(paste("u", 1:length(unique(edx.copy$userId)), sep = ""), 
                                               paste("m", 1:length(unique(edx.copy$movieId)), sep = "")))


#Remove the copy created
rm(edx.copy)

#The first 10 users
sparse_ratings[1:10,1:10]

##Convert rating matrix into a recommenderlab sparse matrix
rate_Mat <- new("realRatingMatrix", data = sparse_ratings)
rate_Mat

##D. Similarity measures

#calculate the user similarity using the cosine similarity
similarity_users <- similarity(rate_Mat[1:50,], 
                               method = "cosine", 
                               which = "users")

image(as.matrix(similarity_users), main = "similarity of Users")

#Using the same approach, compute similarity between  movies.
similarity_movies <- similarity(rate_Mat[,1:50], 
                                method = "cosine", 
                                which = "items")

image(as.matrix(similarity_movies), main = "similarity of Movies")

##E. Dimension Reduction

#implicitly restarted Lanczos bidiagonalization algorithm (IRLBA)
set.seed(1, sample.kind = "Rounding")
Y <- irlba(sparse_ratings,tol=1e-4,verbose=TRUE,nv = 100, maxit = 1000)

# plot singular values
plot(Y$d, pch=20, col = "blue", cex = 1.5, xlab='Singular Value', ylab='Magnitude', 
     main = "User-Movie Matrix")

# calculate sum of squares of all singular values
all_sing_val <- sum(Y$d^2)

# variability described by first 6, 12, and 20 singular values
first_six <- sum(Y$d[1:6]^2)
print(first_six/all_sing_val)

first_twl <- sum(Y$d[1:12]^2)
print(first_twl/all_sing_val)

first_twt <- sum(Y$d[1:20]^2)
print(first_twt/all_sing_val)

perc_vec <- NULL
for (i in 1:length(Y$d)) {
  perc_vec[i] <- sum(Y$d[1:i]^2) / all_sing_val
}
plot(perc_vec, pch=20, col = "blue", cex = 1.5, xlab='Singular Value', ylab='% Sum of Squares of Singular Values', main = "k for Dimensionality Reduction")
lines(x = c(0,100), y = c(.90, .90))

#value of K
k = length(perc_vec[perc_vec <= .90])
k

#get the decomposition of Y ; matrices U, D, and V
U_k <- Y$u[, 1:k]
dim(U_k)

D_k <- Diagonal(x = Y$d[1:k])
dim(D_k)

V_k <- t(Y$v)[1:k, ]
dim(V_k)

##F. Relevant Data

#1. Determine the minimum number of movies per user.
min_no_movies <- quantile(rowCounts(rate_Mat), 0.9)
print(min_no_movies)

#2. Determine the minimum number of users per movie.
min_no_users <- quantile(colCounts(rate_Mat), 0.9)
print(min_no_users)

#3. Select the users and movies matching these criteria.
rate_movies <- rate_Mat[rowCounts(rate_Mat) > min_no_movies,
                        colCounts(rate_Mat) > min_no_users]
rate_movies

#define the RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


#Movie effect#

#calculate the average of all ratings of the edx dataset
mu <- mean(edx$rating)


#calculate b_i on the training dataset
movie_m <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# predicted ratings
predicted_ratings_bi <- mu + validation %>% 
  left_join(movie_m, by='movieId') %>%
  .$b_i

#Movie and user effect#

#calculate b_u using the training set 
user_m <- edx %>%  
  left_join(movie_m, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#predicted ratings
predicted_ratings_bu <- validation %>% 
  left_join(movie_m, by='movieId') %>%
  left_join(user_m, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

#Movie, user and time effect#

#create a copy of validation set , valid, and create the date feature which is the timestamp converted to a datetime object  and  rounded by week.
valid <- validation
valid <- valid %>%
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) 

#calculate time effects ( b_t) using the training set
temp_m <- edx %>%
  left_join(movie_m, by='movieId') %>%
  left_join(user_m, by='userId') %>%
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(b_t = mean(rating - mu - b_i - b_u))


#predicted ratings
predicted_ratings_bt <- valid %>% 
  left_join(movie_m, by='movieId') %>%
  left_join(user_m, by='userId') %>%
  left_join(temp_m, by='date') %>%
  mutate(pred = mu + b_i + b_u + b_t) %>%
  .$pred

#calculate the RMSE for movies
rmse_model_1 <- RMSE(validation$rating,predicted_ratings_bi)  
rmse_model_1

#calculate the RMSE for users
rmse_model_2 <- RMSE(validation$rating,predicted_ratings_bu)
rmse_model_2

#calculate the RMSE for time effects
rmse_model_3 <- RMSE(valid$rating,predicted_ratings_bt)
rmse_model_3

#remove valid before regularization
rm(valid)

##G. Regularization## 

#remembering that lambda is a tuning parameter. We can use cross-validation to choose it
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu_reg <- mean(edx$rating)
  
  b_i_reg <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i_reg = sum(rating - mu_reg)/(n()+l))
  
  b_u_reg <- edx %>% 
    left_join(b_i_reg, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u_reg = sum(rating - b_i_reg - mu_reg)/(n()+l))
  
  predicted_ratings_b_i_u <- 
    validation %>% 
    left_join(b_i_reg, by = "movieId") %>%
    left_join(b_u_reg, by = "userId") %>%
    mutate(pred = mu_reg + b_i_reg + b_u_reg) %>%
    .$pred
  
  return(RMSE(validation$rating,predicted_ratings_b_i_u))
})

qplot(lambdas, rmses)  

#For the full model, the optimal  λ  is given as
lambda <- lambdas[which.min(rmses)]
lambda

rmse_model_4 <- min(rmses)
rmse_model_4

#summarize all the rmse on validation set for Linear regression models
rmse_results <- data.frame(methods=c("movie effect","movie + user effects","movie + user + time effects", "Regularized Movie + User Effect Model"),rmse = c(rmse_model_1, rmse_model_2,rmse_model_3, rmse_model_4))
kable(rmse_results) %>%
  kable_styling(bootstrap_options = "striped" , full_width = F , position = "center") %>%
  kable_styling(bootstrap_options = "bordered", full_width = F , position ="center") %>%
  column_spec(1,bold = T ) %>%
  column_spec(2,bold =T ,color = "white" , background ="#D7261E")








