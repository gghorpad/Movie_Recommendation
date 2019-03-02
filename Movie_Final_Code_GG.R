#This package is needed, install if not present
library(stringr)
#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

library(data.table)
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
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


#####################
##Data Visualization##

#load("data_generate.RData") #You can load the data here to run faster

edx%>%ggplot()+geom_histogram(aes(rating),binwidth = 0.5)
#We see that movies are likely to be rated somewhere in the middle than extremes
#We see that increment of 0.5 is less likely to be rated

set.seed(10)
random_viz<-edx[sample(edx$movieId,8),"movieId"]
edx%>%filter(movieId%in%random_viz)%>%group_by(movieId)%>%summarize(avg_rating=mean(rating),stddev=sd(rating),Title=first(title))%>%ggplot(aes(x=Title,y=avg_rating,ymin=avg_rating-stddev,ymax=avg_rating+stddev))+geom_col(fill="orange")+geom_errorbar()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle("Average Ratings vs Movies")
#Some movies are rated higher than others

set.seed(19)
random_viz<-edx[sample(edx$userId,8),"userId"]
edx%>%filter(userId%in%random_viz)%>%group_by(userId)%>%summarize(avg_rating=mean(rating),stddev=sd(rating))%>%ggplot(aes(x=userId,y=avg_rating,ymin=avg_rating-stddev,ymax=avg_rating+stddev))+geom_col(fill="orange")+geom_errorbar()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle("Average Ratings vs Users")
#Some users are more generous than others in rating movies

su<-edx%>%filter(userId==5)
#Extract data for single user(su) with many views

#Summarise data for this user
graph<-data.frame(su%>%filter(str_detect(genres,"Action"))%>%summarise(Avg=mean(rating),stddev=sd(rating),ymin=Avg-stddev,ymax=Avg+stddev,genre="Action"))
graph[2,]<-data.frame(su%>%filter(str_detect(genres,"Drama"))%>%summarise(Avg=mean(rating),stddev=sd(rating),ymin=Avg-stddev,ymax=Avg+stddev,genre="Drama"))
graph[3,]<-data.frame(su%>%filter(str_detect(genres,"Crime"))%>%summarise(Avg=mean(rating),stddev=sd(rating),ymin=Avg-stddev,ymax=Avg+stddev,genre="Crime"))
graph[4,]<-data.frame(su%>%filter(str_detect(genres,"Adventure"))%>%summarise(Avg=mean(rating),stddev=sd(rating),ymin=Avg-stddev,ymax=Avg+stddev,genre="Adventure"))
graph[5,]<-data.frame(su%>%summarise(Avg=mean(rating),stddev=sd(rating),ymin=Avg-stddev,ymax=Avg+stddev,genre="Overall"))

#Visualise graph for some genres
graph%>%ggplot(aes(x=genre,y=Avg,ymin=ymin,ymax=ymax))+geom_col()+geom_errorbar()+ggtitle("UserId 5: Average Rating vs Genre")

####################################
## Data Analysis##

#Model based approach


#Overall Average Rating

mu<-mean(edx$rating)

movie_avgs<-edx%>%group_by(movieId)%>%summarize(b_i=mean(rating-mu))
#Calculate Movie Effect

user_avgs<-edx%>%left_join(movie_avgs,by='movieId')%>%group_by(userId)%>%summarize(b_u=mean(rating-mu-b_i))
#Calculate User Overall Effect

validation1<-validation%>%left_join(movie_avgs,by='movieId')%>%left_join(user_avgs,by='userId')
validation1[is.na(validation1)]<-0

predicted_ratings1<-validation1%>%mutate(pred=mu+b_i)%>%.$pred
predicted_ratings2<-validation1%>%mutate(pred=mu+b_i+b_u)%>%.$pred

#model_1_rmse<-RMSE
#model_2_rmse<-RMSE(predicted_ratings,test3$rating)
model_0_rmse<-RMSE(mu,validation$rating)
model_1_rmse<-RMSE(predicted_ratings1,validation$rating)
model_2_rmse<-RMSE(predicted_ratings2,validation$rating)

#Generate a list of all common genres

set.seed(2019)
#Make a small sample of 10000 entries
indx<-sample(edx$movieId,10000)
sample1<-edx[indx,]

library(stringr)
temp_genres_list<-melt(str_split(sample1$genres,"[|]"))
#Make a list of genres
genres_list<-unique(temp_genres_list[,1])

#Add User Specific Genre Effect
load("C:/R_projects/Move_recommendation/genres_list")
edx2<-edx%>%left_join(movie_avgs,by='movieId')%>%left_join(user_avgs,by='userId')                                                                                                                                         
edx2[is.na(edx2)]<-0
validation2<-validation1%>%mutate(numerator=0,denominator=0)

for(genre_inst in genres_list){
  #Drop genre effect variable to prevent any values in previous loop from carrying over
  drops <- c("genre_effect")
  validation2<-validation2[ ,!(names(validation2) %in% drops)]
  
  #Detect which Movies are in current genre iteration
  validation2<-validation2%>%mutate(genre_flag=str_detect(genres,genre_inst))
  
  #Calculate genre effect for each user
  temp<-edx2%>%filter(str_detect(genres,genre_inst))%>%group_by(userId)%>%summarize(genre_effect:=0+mean(rating-mu-b_i-b_u))
  
  validation2<-validation2%>%left_join(temp,by='userId')
  validation2[is.na(validation2)]<-0
  
  #The overall genre effect for each movie is combination of all relevant genre effects for that user 
  validation2<-validation2%>%mutate(numerator=numerator+genre_flag*genre_effect)
  validation2<-validation2%>%mutate(denominator=denominator+genre_flag)
  
}
#Each movie has multiple genres, the net effect of all genres is limited even if many genres are present
validation2<-validation2%>%mutate(b_g=numerator/denominator)

predicted_ratings3<-validation2%>%mutate(pred=mu+b_i+b_u+b_g)%>%.$pred
model_3_rmse<-RMSE(predicted_ratings3,validation$rating)

#Results
model_0_rmse #Only Mean
model_1_rmse #Mean , Movie Effect
model_2_rmse #Mean , Movie Effect, User Overall Effect
model_3_rmse #Mean , Movie Effect, User Overall Effect, User Genre Effect

#Model works well
save.image("Movie_Prediction_GG")

