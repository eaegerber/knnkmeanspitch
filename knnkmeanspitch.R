install.packages("pitchRx")
library(pitchRx)

#Setting up the strike zone

topKzone <- 3.5
botKzone <- 1.6
inKzone <- -0.95
outKzone <- 0.95
kZone <- data.frame(
  x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
  y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

#Getting the data from the July 23, 2009 Tampa Bay Rays vs. Chicago White Sox game

data(gids, package="pitchRx")
wsox <- gids[grepl("cha", gids) & grepl("2009", gids)]
buehrle <- scrape(game.ids = wsox[138])
dim(buehrle$pitch)

#One dataset with all pitches in the game, one with only Mark Buehrle pitches

pitchperfect <- buehrle$pitch 
newpitch <- pitchperfect[-which(pitchperfect$inning_side == "bottom"),]

#Number of different types of pitches thrown for each dataset

table(pitchperfect$pitch_type)
table(newpitch$pitch_type)

#The pitch location and type for every pitch, by pitch type and overall

ggplot(pitchperfect, aes(px, pz, color=pitch_type)) + geom_point(size = 3) +
#  facet_wrap(~ pitch_type) + 
  geom_path(aes(x, y), data=kZone, lwd=2, col="black", alpha = 0.2) + 
  scale_y_continuous(limits=c(0,5)) +
  theme_grey(base_size = 17)

#The pitch location and type for Buehrle's pitches, by pitch type and overall

ggplot(newpitch, aes(px, pz, color=pitch_type)) + geom_point(size = 3) +
#   facet_wrap(~ pitch_type) + 
  geom_path(aes(x, y), data=kZone, lwd=2, col="black", alpha = 0.2) + 
  scale_y_continuous(limits=c(0,5)) +
  theme_grey(base_size = 17)

#Writing functions for performing K-nearest neighbours
#Euclidean distance

euc_dist <- function(x1, x2){
  sum((x1-x2)^2)
}

#Getting distances from each point to every other point

mat_dist <- function(my_test, my_train){
  dist <- NULL
  for(i in 1:nrow(my_train)) {
    dist[i] <- euc_dist(my_test, my_train[i,])
  }
  return(dist)
}

#Get the indices for the K-nearest neighbours for each point

get_knn <- function(k, my_test, my_train) {
  t_dist <- mat_dist(my_test, my_train)
  srtd <- sort(t_dist, index.return=TRUE)
  return(srtd$ix[1:k])
}

#Get the labels for those K-nearest neighbours and find the majority label

maj_lab <- function(k_ind_vec){
  k_match <- c()
  for(i in k_ind_vec){
    k_match[i] <- pitchperfect$pitch_type[i]
  }
  k_match <- na.omit(k_match)
  maj <- function(x){
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
return(maj(k_match))
}

#Returning the majority label for the K-nearest neighbours of each point

my_knn <- function(k, my_data){
  labs <- c()
  for(i in 1:nrow(my_data)){
    k_ind_vec <- get_knn(k, my_data[i,], my_data[-i,])
    labs <- rbind(labs,maj_lab(k_ind_vec))
  }
  return(labs)
}

#Correct function returns the percentage of pitches were correctly classified under specific K

correct <- function(k, my_data, tru_labels){
  myK <- my_knn(k, my_data)
  pct <- sum(tru_labels==myK)/length(tru_labels)
  return(pct)
}

#Simply isolating the x, y coordinates for the pitches in the overall game

pitchloc <- matrix(c(pitchperfect$px, pitchperfect$pz), nrow=length(pitchperfect$px))

#Function that chooses the K that maximizes correct classifications

bestK <- function(my_data, tru_labels){
  acc <- c()
  for(i in 1:(length(tru_labels)-1)){
    acc[i] <- correct(i, my_data, tru_labels)
  }
  return(list(which.max(acc[]), max(acc), acc))
}

#Finding the best K for the overall dataset and then running K-NN with it, returning number in each classification

accu1 <- bestK(pitchloc, pitchperfect$pitch_type)
plot(accu1[[3]]);lines(accu1[[3]])
best1 <- my_knn(accu1[[1]], pitchloc)

table(best1)

#Plotting the points based on their classification

ggplot(pitchperfect, aes(px, pz, color=best1)) + geom_point(size = 3) +
#  facet_wrap(~ pitch_type) + 
  geom_path(aes(x, y), data=kZone, lwd=2, col="black", alpha = 0.2) + 
  scale_y_continuous(limits=c(0,5)) +
  theme_grey(base_size = 17)

#Doing the same as above for the Buehrle pitch data

pitchloc2 <- matrix(c(newpitch$px, newpitch$pz), nrow=length(newpitch$px))

accu2 <- bestK(pitchloc2, newpitch$pitch_type)
plot(accu2[[3]]);lines(accu2[[3]])
best2 <- my_knn(accu2[[1]], pitchloc2)
table(best2)

ggplot(newpitch, aes(px, pz, color=best2)) + geom_point(size = 3) +
#  facet_wrap(~ pitch_type) + 
  geom_path(aes(x, y), data=kZone, lwd=2, col="black", alpha = 0.2) + 
  scale_y_continuous(limits=c(0,5)) +
  theme_grey(base_size = 17)

#Now moving on to K-means.  First find the mean location for each type of pitch from the raw data
#Then, perform K-means to find clusters based on those initial mean locations
#Find percentage of pitches correctly identified by K-means
#Plot of the pitches with their K-means cluster classifications with original centers
#Plot of original pitch classifications with cluster centers

dd <- by(pitchperfect, INDICES = pitchperfect$pitch_type, FUN = function(x) c(mean(x$px), mean(x$pz)))
dd1 <- unlist(dd)
dd1 <- matrix(dd1, ncol = 2, byrow = TRUE)
dd2 <- as.data.frame(dd1)
row.names(dd2) <- c('CH','CU','FC','FF','FT','SI','SL')
cl <- (kmeans(pitchperfect[,c('px','pz')], dd2))
pitchperfect$cluster <- row.names(dd2)[cl$cluster]
sum(pitchperfect$pitch_type==pitchperfect$cluster)/244
centers <- as.data.frame(cl$centers)
ggplot(data=pitchperfect, aes(px, pz, color=cluster)) + 
  geom_point(size = 3) +
  geom_path(aes(x, y), data=kZone, lwd=2, col="black", alpha = 0.2) +
  geom_point(data=dd2, aes(x=V1,y=V2, color=row.names(dd2)), size=50, alpha=.3, show_guide=F) +
  theme_grey(base_size = 17)

ggplot(data=pitchperfect, aes(px, pz, color=pitch_type)) + 
  geom_point(size = 3) +
  geom_path(aes(x, y), data=kZone, lwd=2, col="black", alpha = 0.2) +
  geom_point(data=centers, aes(x=px,y=pz, color=row.names(dd2)), size=50, alpha=.3, show_guide=F) +
  theme_grey(base_size = 17)

#Now with Buehrle data

ee <- by(newpitch, INDICES = newpitch$pitch_type, FUN = function(x) c(mean(x$px), mean(x$pz)))
ee1 <- unlist(ee)
ee1 <- matrix(ee1, ncol = 2, byrow = TRUE)
ee2 <- as.data.frame(ee1)
row.names(ee2) <- c('CH','CU','FC','FF','FT','SL')
cl2 <- (kmeans(newpitch[,c('px','pz')], ee2))
newpitch$cluster <- row.names(ee2)[cl2$cluster]
sum(newpitch$pitch_type==newpitch$cluster)/116
centers2 <- as.data.frame(cl2$centers)
ggplot(data=newpitch, aes(px, pz, color=cluster)) + 
  geom_point(size = 3) +
  geom_path(aes(x, y), data=kZone, lwd=2, col="black", alpha = 0.2) +
  geom_point(data=ee2, aes(x=V1,y=V2, color=row.names(ee2)), size=50, alpha=.3, show_guide=F) +
  theme_grey(base_size = 17)

ggplot(data=newpitch, aes(px, pz, color=pitch_type)) + 
  geom_point(size = 3) +
  geom_path(aes(x, y), data=kZone, lwd=2, col="black", alpha = 0.2) +
  geom_point(data=centers2, aes(x=px,y=pz, color=row.names(ee2)), size=50, alpha=.3, show_guide=F) +
  theme_grey(base_size = 17)
