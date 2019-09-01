###Clean Spotify Sheet for Regressions and Other Fun Things###

rm(list=ls())

##Installing Things Here

install.packages("devtools")
devtools::install_github('charlie86/spotifyr')
install.packages("tidyverse")
install.packages("knitr")
install.packages("ggplot2")
install.packages("ISLR")
install.packages("class")

##Put all your books here (libraries)

library(spotifyr)
library(tidyverse)
library(knitr)
library(ggplot2)
library(ISLR)
library(class)

##Load in your data    #the first two functions are how I scraped my data through spotify

Sys.setenv(SPOTIFY_CLIENT_ID = '91c2474098404ded96ef46ab1650a6c3') #client API
Sys.setenv(SPOTIFY_CLIENT_SECRET = '83ef1afbf1b14d92a11488892015a10b') #secret API

access_token <- get_spotify_access_token() #gets connection through localhost


#################################Scraping section


#playlist_uris <- c('0q02QXiqyqGE4zk4StL26G', '2vjkDmU3e1MyhsVSxitfZO', '4lslShx7HhDS9VQ7AMujdi', '3DvKXVtUE2s4wAO8qvT6JI', '5FeB43Rn5cFI5by3Qbj5Hb', '6Ep9mFRIiAomCbBhQ9kALS', '5B7WvPY4OZomU7m4Zc6SFr', '41nZ32R9XAiESOzPUwNOdP', '2q6Sp2Xb6dIiDJWS8xlRIf', '0gt8DpctyrYjUsyV569nHI', '6TkCNVZftNn8APpqBT58nA', '28hwJkMxRTkQqIRBdCb0eM', '1LVZQeAGkiCsYxLg6IB6JD', '4KqT8xeo0ixnQjOhfkCFkc', '5yK4uS6WVNK8yGRxz0Q4O3', '1tU5gOCZXmLDVmFIzvElIR')

#scrapefeatures <- get_playlist_audio_features('reicst13', playlist_uris) #gathers audio features of selected URIs

spotifyset <- read.csv(file = "SpotfiyWithModifiers.csv") #reads our gathered csv file

sdf <- data.frame(spotifyset) #spotifyset data frame

csdf <- subset(sdf, select = -c(1, 2, 3, 4, 5, 6, 7, 9, 12, 13, 15, 16)) #clean spotifyet data frame by deselecting various rows not helpful to our analysis

#pop <- get_playlist_audio_features('thesoundsofspotify', '6gS3HhOiI17QNojjPuPzqc')

#write.csv(pop, "spotifypop.csv")

#rap <- get_playlist_audio_features('thesoundsofspotify', '6s5MoZzR70Qef7x4bVxDO1')

#write.csv(rap, "spotifyrap.csv")

#rock <- get_playlist_audio_features('thesoundsofspotify', '7dowgSWOmvdpwNkGFMUs6e')

#write.csv(rock, "spotifyrock.csv")

#latin <-  get_playlist_audio_features('thesoundsofspotify', '1IGB0Uz7x2VY28qMagUC24')

#write.csv(latin, "spotifylatin.csv")

#metal <-  get_playlist_audio_features('thesoundsofspotify', '0zJrEnj3O8CohOpFFUVSo9')

#write.csv(metal, "spotifymetal.csv")

#indie <-  get_playlist_audio_features('thesoundsofspotify', '1aYiM4zLmBuFq0Fg6NQb6a')

#write.csv(indie, "spotifyindie.csv")

#country <- get_playlist_audio_features('thesoundsofspotify', '4mijVkpSXJziPiOrK7YX4M')

#write.csv(country, "spotifycountry.csv")

#funk <- get_playlist_audio_features('thesoundsofspotify', '0MBvtOIm5fuBbRHEltDY8A')

#write.csv(funk, "spotifyfunk.csv")

#classical <- get_playlist_audio_features('thesoundsofspotify', '3HYK6ri0GkvRcM6GkKh0hJ')

#write.csv(classical, 'spotifyclassical.csv')

#folk <- get_playlist_audio_features('thesoundsofspotify', '4JuKjgd76AZn2fUaeXNCuo')

#write.csv(folk, "spotifyfolk.csv")

#ddd <- read.csv("spotifypop.csv") #this was a test set I wanted to showcase

#ddd <- data.frame(ddd) 

#str(ddd)
#names(ddd)

######################################## The section below is exploratory, the final R Markdown contains the better version of this

#plots

plot(x=csdf$tempo, y = csdf$track_popularity)

plot(x=csdf$danceability, y = csdf$track_popularity)

plot(x=csdf$valence, y = csdf$track_popularity)

plot(x=csdf$energy, y = csdf$track_popularity)

plot(x=csdf$loudness, y = csdf$track_popularity)

plot(x=csdf$acousticness, y = csdf$track_popularity)

plot(x=csdf$liveness, y = csdf$track_popularity)

plot(x=csdf$duration_s, y = csdf$track_popularity)

plot(x=csdf$time_signature, y = csdf$track_popularity)

plot(x=csdf$instrumentalness, y = csdf$track_popularity)

plot(x=csdf$speechiness, y = csdf$track_popularity)

plot(x=csdf$mode_binary, y = csdf$track_popularity)

plot(x=csdf$numeric_key, y = csdf$track_popularity)

csdf %>%  #gets most popular keys
  count(key_mode, sort = TRUE) %>% 
  head(32) %>% 
  kable()

######################################################################### Linear Regression

set.seed(5072) #setting random seed because that's what you're supposed to do

csdf.reg <- lm(track_popularity ~ energy + danceability + valence + loudness + liveness + speechiness + duration_s + numeric_key + mode_binary + time_signature + acousticness + instrumentalness + tempo, data = csdf)

str(csdf.reg)
anova(csdf.reg)
summary(csdf.reg)
#plot(csdf.reg)


pop01 <- rep(0, length(csdf$track_popularity)) #making popular or not binary variable
pop01[csdf$track_popularity > median(csdf$track_popularity)] <- 1
csdf <- data.frame(csdf, pop01) #append dataset with new variable

##########making training data

trainindices <- sample(1:nrow(csdf), .80*nrow(csdf))
testindices <- setdiff(1:nrow(csdf), trainindices)        
trainset <- csdf[trainindices, ]
testset <- csdf[testindices, ]


############# Knn regression

sel.variables <- which(names(trainset)%in%c("pop01 + danceability + energy + numeric_key + loudness + mode_binary + speechiness + acousticness + instrumentalness + liveness + valence + tempo + duration_m"))

accuracies <- data.frame("k"=1:50, acc=NA)
for(k in 1:50){
  knn.pred <- knn(train=trainset[, sel.variables], test=testset[, sel.variables], cl=trainset$pop01, k=k)
  
  # test-error
  accuracies$acc[k]= round(sum(knn.pred!=testset$pop01)/nrow(testset)*100,2)
}

accuracies  ###### tests for best k based on error rate

knn.pred <- knn(train=trainset[, sel.variables], test=testset[, sel.variables], cl=trainset$pop01, k=)

summary(knn.pred)

plot(knn.pred)

knn.table <- table(knn.pred, testset$pop01)

knn.table

knn.OverallCorrectPredictions <- (knn.table["0", "0"] + knn.table["1", "1"])/sum(knn.table)

knn.OverallErrorRate <- (knn.table[2] + knn.table[3]) / sum(knn.table) 

knn.type1FalsePositiveErrorRate <- knn.table[1, 2] / sum(knn.table[1, ])

knn.type2FalseNegativeErrorRate <- knn.table[2, 1] / sum(knn.table[2, ])

knn.sensitivityPowerRecall <- knn.table[2, 2] / sum(knn.table[2, ])

knn.precision <- knn.table["0", "0"] / sum(knn.table[, "0"])

print(paste("The overal fraction of correct predictions:", knn.OverallCorrectPredictions))

print(paste("The overall error rate:", knn.OverallErrorRate))

print(paste("The Type 1 Error Rate:", knn.type1FalsePositiveErrorRate))

print(paste("The Type 2 Error Rate:", knn.type2FalseNegativeErrorRate))

print(paste("The Power of the model:", knn.sensitivityPowerRecall))

print(paste("The Precision of the model:", knn.precision))