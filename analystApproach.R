library(data.table)
library(tidyr)

#Data Prep####
  
  #movies
    movies<-read.delim("movies.dat",header = FALSE)
    movies<-as.data.table(movies)
    movies[, c("Movie_ID", "Movie_Name","Genre") := tstrsplit(V1, "::", fixed=TRUE)]
    movies[,V1:=NULL]
    #control
    nrow(movies[is.na(Movie_ID)]) 
    nrow(movies[is.na(Movie_Name)]) 
    nrow(movies[is.na(Genre)]) 
    View(movies[is.na(Genre)]) 
    #Note: Some movies do not have Genre information
    #To find the max number of genres which a movie has. we will take the max(Num_of_Genre+1) since num of genre is calculated by counting "l" character(separator).
    str(movies)
    library(stringr)
    Num_of_Genre <- str_count(movies$Genre, "\\|")
    max(Num_of_Genre,na.rm = TRUE)+1 #max num genre = 4
    movies[, c("Genre1", "Genre2","Genre3","Genre4") := tstrsplit(Genre, "|", fixed=TRUE)]
    movies[, c("Movie_Name", "Year") := tstrsplit(Movie_Name, "(", fixed=TRUE)]
    movies[,Year:=gsub( ")", "", as.character(movies$Year))]
  
  #ratings
    ratings<-read.delim("ratings.dat", sep=":",header = FALSE)
    ratings<-as.data.table(ratings)
    #control
    nrow(ratings[is.na(V2)]) 
    ratings[,V2:=NULL]
    nrow(ratings[is.na(V4)]) 
    ratings[,V4:=NULL]
    nrow(ratings[is.na(V6)]) 
    ratings[,V6:=NULL]
    nrow(ratings[is.na(V1)])
    nrow(ratings[is.na(V3)])
    nrow(ratings[is.na(V5)])
    nrow(ratings[is.na(V7)])
    setnames(ratings,"V1", "User_ID")
    setnames(ratings,"V3", "Movie_ID")
    setnames(ratings,"V5", "Rating")
    setnames(ratings,"V7", "Rating_TimeStamp")
  
  #users
    users<-read.delim("users.dat", sep=":",header = FALSE)
    users<-as.data.table(users)
    users[,V2:=NULL]
    setnames(users,"V1", "User_ID")
    setnames(users,"V3", "User_Twit_ID")
  
  options(scipen=999)
  
  #########################################
  #movies
    nrow(movies)# number of movies #31698
    min(movies$Year)
    max(movies$Year)
    length(which(is.na(movies$Genre))) #number of movies having no genre
  
  #ratings
    nrow(ratings) #727362
    movies[,`:=`(Movie_ID=as.integer(Movie_ID))]
    not_rated<-movies$Movie_ID[!(movies$Movie_ID %in% ratings$Movie_ID)] #No rated movies
    mean(ratings$Rating) #7.301803
  
  #users
    nrow(users) #54702
    users_dont_rate<-users$User_ID[!(users$User_ID %in% ratings$User_ID)] #everybody rated

###################################
#Data Validation####

  #rating range
    #all ratings should lie between 0 and 10
    min(ratings$Rating) #0
    max(ratings$Rating) #10
  
  #rating time before release year???
    my.lt = as.POSIXlt("1970-01-01 00:00:00")
    ratings[,`:=`(TimeStamp_date_hour=(my.lt +Rating_TimeStamp))]
    ratings[,`:=`(Year=year(ratings$TimeStamp_date_hour))]
    ratings[,`:=`(Month=month(ratings$TimeStamp_date_hour))]
    ratings[,`:=`(Day=mday(ratings$TimeStamp_date_hour))]
    str(ratings)
    ratings[,`:=`(Year=as.numeric(Year),Month=as.numeric(Month))]
    
    ReleaseY_TweetY<-data.table(Movie_ID=movies$Movie_ID,Release_Year=movies$Year)
    ReleaseY_TweetY[,`:=`(Movie_ID=as.integer(Movie_ID))]
    setkey(ratings,Movie_ID)
    setkey(ReleaseY_TweetY,Movie_ID)
    ReleaseY_TweetY<-ratings[ReleaseY_TweetY]
    ReleaseY_TweetY[,`:=`(Rating=NULL,Rating_TimeStamp=NULL,TimeStamp_date_hour=NULL)]
    setnames(ReleaseY_TweetY,"Year", "Rating_Year")
    str(ratings)
    str(ReleaseY_TweetY)
    ReleaseY_TweetY[,`:=`(Release_Year=as.numeric(Release_Year))]
    
    ReleaseY_TweetY[Rating_Year-Release_Year<0,Mistake:=Rating_Year-Release_Year]
    ReleaseY_TweetY[!is.na(Mistake)]
    #  User_ID Movie_ID Rating_Year Month Release_Year Mistake
    # 1:   33814  1850418        2014    12         2015      -1
    # 2:   20238  2250912        2016     9         2017      -1
    # 3:   50252  2446042        2014    12         2015      -1
    #these 3 movies were rated before release year. not convenient. they are deleted.
    which(ratings[, Movie_ID] == 1850418 & ratings[,Year]==2014 & ratings[,User_ID]==33814)
    which(ratings[, Movie_ID] == 2250912 & ratings[,Year]==2016 & ratings[,User_ID]==20238)
    which(ratings[, Movie_ID] == 2446042 & ratings[,Year]==2014 & ratings[,User_ID]==50252)
    
  ratings_last<-ratings[-c( 416601, 506299, 550625), ]
  # check : which(ratings_last[, Movie_ID] == 1850418 & ratings_last[,Year]==2014 & ratings_last[,User_ID]==33814)
############################################################################

#Analysis in detail####

  #Ratings #dynamics of being a good or bad movie
    setorder(ratings_last,Movie_ID)
    ratings_last[,`:=`(Movie_Mean_Ratings=mean(Rating)),by=Movie_ID]
    ratings_last[,Rating_Counts:=.N,by=Movie_ID]
    setorder(ratings_last,-Rating_Counts)
    head(unique(ratings_last$Movie_Mean_Ratings), n=30L)
    #most rated 30 movies have rating more than almost 7 which can show good movies have been rated more than the bad ones. Or people hear good things about the movie and have been affected by others who think the movie is really good and watch it with prejudice and rate it high.
    tail(unique(ratings_last$Movie_Mean_Ratings), n=30L)
    #when we look at the least rated movies, it can be seen that mean rating for each movie deviates too much. The reason is that just one person can affect the rating excessively. So can we say these movies are good or bad? dont think so.But given that most of the least rated movies have low rating, it would not be wrong to say they are generally bad.
    
    movie_vs_rating<-data.table(movie_id=ratings_last$Movie_ID,counts=ratings_last$Rating_Counts,rating=ratings_last$Movie_Mean_Ratings)
    movie_vs_rating<-unique(movie_vs_rating)
    mean(ratings_last$Rating)
    hist(movie_vs_rating$rating,xlab="Movie Mean Ratings",ylab="Frequency of Movies",main="Histogram of Movie Mean Ratings")
    movie_vs_rating<-movie_vs_rating[!(rating<8 & rating>5)]
    movie_vs_rating[rating>=8,isGood:=1]
    movie_vs_rating[rating<=5,isGood:=0]
    boxplot(movie_vs_rating$counts,data=movie_vs_rating)
    abline(h=quantile(movie_vs_rating$counts,0.25),col="red",lty=2)
    abline(h=quantile(movie_vs_rating$counts,0.75),col="red",lty=2)
    #find Q1 and remove the point below Q1(lower whisker)
    quantile(movie_vs_rating$counts, c(0.25,0.5,0.75)) #1
    # check ::median(movie_vs_rating$counts)
    #then the movies rated 1 or less than 1 are removed from movie_vs_rating table
    movie_vs_rating<-subset(movie_vs_rating,counts>1)
    #one way anova test to see if "good" movies are rated more than "bad" ones.
    #see the relation between isGood and counts
    anova_rating<-aov(counts~isGood,data = movie_vs_rating)
    summary(anova_rating)
    #p-value is very low then we can see that there is a significant relation between rating count and being a "good" movie

  #Genre
    setorder(ratings_last,-Rating_Counts)
    most_rated_1000<-head(unique(ratings_last$Movie_ID), n=1000L) #first 1000 movies. the more data the more accurate decision however we examine most rated movies and as the number of movies examined increases, the number of rating decreases. Should be careful at this point.similar for high rated movies
    
    setorder(ratings_last,-Movie_Mean_Ratings)
    avg_number_ratings<-nrow(ratings_last)/nrow(movies)
    ratings_last[Rating_Counts>avg_number_ratings,] #movies have ratings more than the ave number of ratings
    high_rated_1000<-head(unique(ratings_last[Rating_Counts>avg_number_ratings,]$Movie_ID), n=1000L) # there are movies which have pretty low number of ratings therefore i added a constraint which chooses movies with rate number more than average number of ratings per a movie.
    
    #eliminate common movies
    most_rated_1000<-as.character(most_rated_1000)
    high_rated_1000<-as.character(high_rated_1000)
    matching_movies<-intersect(most_rated_1000,high_rated_1000)
    most_rated_1000<-as.integer(most_rated_1000 [! most_rated_1000 %in% matching_movies])
    high_rated_1000<-as.integer(high_rated_1000 [! high_rated_1000 %in% matching_movies])
    
    setorder(ratings_last,-Rating_Counts)
    x<-movies[Movie_ID %in% most_rated_1000]
    Genres_most_rated<-append(x$Genre1,x$Genre2)
    Genres_most_rated<-append(Genres_most_rated,x$Genre3)
    Genres_most_rated<-append(Genres_most_rated,x$Genre4)
    most_rated_table<-as.data.table(table(Genres_most_rated))
    most_rated_table[,k:=N/sum(most_rated_table$N)]
    
    setorder(ratings_last,-Movie_Mean_Ratings)
    y<-movies[Movie_ID %in% high_rated_1000]
    Genres_high_rated<-append(y$Genre1,y$Genre2)
    Genres_high_rated<-append(Genres_high_rated,y$Genre3)
    Genres_high_rated<-append(Genres_high_rated,y$Genre4)
    high_rated_table<-as.data.table(table(Genres_high_rated))
    high_rated_table[,k2:=N/sum(high_rated_table$N)]
    #common genres
    setkey(most_rated_table,Genres_most_rated)
    setkey(high_rated_table,Genres_high_rated)
    common<-most_rated_table[high_rated_table]
    common[is.na(k),k:=0]
    common[is.na(k2),k2:=0]
    common[,difference:=k-k2]
    common[,percentage:=difference*100]
    
    par(mar=c(5,8,3,1))
    plot(common$percentage,factor(common$Genres_most_rated),col=factor(common$Genres_most_rated),yaxt='n',xlab="",ylab = "",main="Percentage Difference Deviations")#differences deviate around zero. No relation !!
    title(ylab="Genres",line=6,cex.lab=1)
    title(xlab="Percentage Differences",line=3,cex.lab=1)
    axis(2L,seq_len(nlevels(factor(common$Genres_most_rated))),levels(factor(common$Genres_most_rated)),las=1)

  #Time
    #do ratings for a movie change over time?
    #i chose one of most rated movies which ratings are around ave. rating since the decrease in ratings over years can be observed with more data clearly and easy
    mean(ratings_last$Rating) #7.30
    # x is a data frame which includes most rated 1000 movies
    #i chose one of top movies in x data frame. Release dates:::2013 which is the earliest rating time. this eliminates the condition that the number of ratings per particular length of time decreases to some degree over years after the release date of the movie.
    unique(ratings_last[Movie_ID==1454468,]$Movie_Mean_Ratings) #8.2396 #Gravity
    
    library(zoo)
    ratings_last$TimeFlow<- with(ratings_last, paste0(Year,"-", Month))
    ratings_last[,TimeFlow:=as.yearmon(ratings_last$TimeFlow, "%Y-%m")] 
    ratings_last$TimeFlowDetailed<- with(ratings_last, paste0(Year,"-", Month,"-",Day))
    ratings_last[,TimeFlowDetailed:=as.Date(ratings_last$TimeFlowDetailed, "%Y-%m-%d")] 
    #ave rating per month
    ratings_last[,Monthly_Mean_Rating:=mean(Rating),by=.(Movie_ID,TimeFlow)]
    #ave rating count per month
    ratings_last[,Rating_Count_Monthly:=.N,by=.(Movie_ID,TimeFlow)]
    
    setorder(ratings_last,TimeFlow)
    gravity_ratings<-ratings_last[Movie_ID==1454468,(Rating)]
    gravity_rating_timeflow<-ratings_last[Movie_ID==1454468,(TimeFlow)]
    gravity_monthly_mean_rating<-ratings_last[Movie_ID==1454468,(Monthly_Mean_Rating)]
    ratingcount<-ratings_last[Movie_ID==1454468,(Rating_Count_Monthly)]
    gravity<-data.table(rating=gravity_ratings,TimeFlow=gravity_rating_timeflow,Monthly_mean_rating=gravity_monthly_mean_rating,ratingcount=ratingcount)
    plot(gravity$TimeFlow,gravity$rating,xlab = "Time Flow",ylab = "Rating",main = "Gravity")
    
    gravity_plot<-unique(gravity[,.(TimeFlow,Monthly_mean_rating,ratingcount)])
    plot(gravity_plot$TimeFlow,gravity_plot$Monthly_mean_rating,type = "l",xlab = "Time Flow",ylab = "Monthly Mean Rating",main = "Gravity")
    abline(h=unique(ratings_last[Movie_ID==1454468,(Movie_Mean_Ratings)])) #average rating
    plot(gravity_plot$TimeFlow,gravity_plot$ratingcount,xlab = "Time Flow",ylab = "Monthly Rating Count",main = "Gravity",ylim=c(0,30),type="l")
    cor(gravity_plot$Monthly_mean_rating,gravity_plot$ratingcount) #0.1633 weak correlation
    




