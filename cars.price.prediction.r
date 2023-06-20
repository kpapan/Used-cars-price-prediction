library(dplyr)
library(tidyr)
library(tidyverse)
library(caret)
library(ggplot2)
library(ggcorrplot)
library(factoextra)
library(cluster)
library(dendextend)
library(caTools)
library(e1071)
library(ROCR)
library("writexl")
library(factoextra)
library(ggridges)
library(randomForest)
library(multcomp)
library(pROC)


cars.p<- read.csv(file.choose(), sep=',')  #loading files

#########  data preprocess select specific columns and  condition 'used' cars only 
#select columns and rows
cars.d <- cars.p%>%
  select( -1,-3,-7,-8,-14,-20:-24)%>%
  filter(Condition=="Used")

# Remove Nas
#check for Nas
sum(is.na(cars.d)) #[1] 3018
apply(X = is.na(cars.d), MARGIN = 2, FUN = sum)

#Price        Condition    Vehicle_brand    Vehicle_model  Production_year       Mileage_km         Power_HP Displacement_cm3 
#0                0                0                0                0                0              594             1215 
#Fuel_type            Drive     Transmission             Type     Doors_number           Colour         Features 
#0                0                0                0             1209                0                0 


apply(X = is.na(cars.d), MARGIN =2, FUN = mean)

#Price        Condition    Vehicle_brand    Vehicle_model  Production_year       Mileage_km         Power_HP Displacement_cm3 
#0.000000000      0.000000000      0.000000000      0.000000000      0.000000000      0.000000000      0.003248564      0.006644791 
#Fuel_type            Drive     Transmission             Type     Doors_number           Colour         Features 
#0.000000000      0.000000000      0.000000000      0.000000000      0.006611977      0.000000000      0.000000000

#substitute nas with the mean of column
cars <- cars.d %>% 
  group_by(cars.d$Fuel_type) %>% 
  mutate_at(vars(7, 8, 13), ~replace_na(., mean(., na.rm = TRUE)))%>% 
  
  #check for Nas
  apply(X = is.na(cars), MARGIN = 2, FUN = sum)

#Remove NaNs

#check for NaNs
sum(is.nan(cars$Displacement_cm3))#830

#remove Nans and calculate mean and median
Displacement_cm3_remove <- cars$Displacement_cm3[!is.nan(cars$Displacement_cm3)]
mean(Displacement_cm3_remove)#[1] 1909.79
median(Displacement_cm3_remove)#1800

#substitute Nans with median
cars$Displacement_cm3 <- cars$Displacement_cm3                              
cars$Displacement_cm3[is.nan(cars$Displacement_cm3)] <- 1800 #we use median of 'Displacement_cm3'

#check for Nans
apply(X = is.na(cars), MARGIN = 2, FUN = sum)
#Price        Condition    Vehicle_brand    Vehicle_model  Production_year       Mileage_km         Power_HP Displacement_cm3 
#0                0                0                0                0                0                0                0 
#Fuel_type            Drive     Transmission             Type     Doors_number           Colour         Features cars.d$Fuel_type 
#0                0                0                0                0                0                0                0 


#create new variables from Features

# Get Features column values of type array
cars.features <- cars$Features

# replace square brackets white spaces and single quotes from the values
cars.features <- gsub('\\[|\\]|\'', '', cars.features)

# split the values by comma
cars.features <- strsplit(cars.features, ',')

# create a data frame with features if a feature not already exists
cars.features <- data.frame(unlist(ifelse(!duplicated(unlist(cars.features)), unlist(cars.features), NA)))

# remove NA values
cars.features <- cars.features[!is.na(cars.features),]

# remove white spaces
cars.features <- gsub('\\s', '', cars.features)

# sort the values
cars.features <- sort(cars.features)

# get unique features
cars.features <- unique(cars.features)

#cars[i, cars.features[y]] <- ifelse(grepl(cars.features[y], cars.features[i]), 1, 0)

for (i in 1:nrow(cars)) {
  # for each feature
  for (y in 1:length(cars.features)) {
    cars[i, cars.features[y]] <- ifelse(grepl(cars.features[y], cars$Features[i]), 1, 0)
  }
}


#many of the new feature variables have only 0, so we cannot use them in our model and we select the variables which have 1 and 0. 
cars.1 <- cars%>%
  select(1:14,16,17,23,30,31,56,58,59,64,80)

#check for Nas
apply(X = is.na(cars.1), MARGIN = 2, FUN = sum)()

#Principal Components Analysis 
#we use PCA in order to check which variables are important for our analysis
df1  <- select((ungroup(cars.1)) , 1,5:8,13,16:24)
str(df1)

pr.out <- prcomp(df1, scale=TRUE)

names(pr.out)

pr.out$rotation

dim(pr.out$x)

biplot(pr.out, scale=0)
pr.out$x <- -pr.out$x

biplot(pr.out, scale=0)

pr.out$sdev
pr.var <- pr.out$sdev^2
print(pr.var)
pve <- pr.var / sum(pr.var)
print(pve)

plot(cumsum(pve), xlab="Principal Component",
     ylab="Cumulative Proportion of Variance Explained", ylim=c(0.5, 1), type='b')

#All variables are important for our analysis

cars.final <- select((ungroup(cars.1)) , 1 : 24)

#final dataset

extras <- rowSums(cars.final[,c(16: 24 )])
cars.final <- mutate(cars.final,extras)

cars.final <- cars.final%>%
  select( -16:-24)

ca <- cars.final #back up for cars.final
#Transmission
Transmission <- integer() 
Transmission[as.character(ca$Transmission) == 'Manual'] <- 1
Transmission[as.character(ca$Transmission) == 'Automatic'] <- 2
Transmission[as.character(ca$Transmission) == '""'] <- 2
unique(Transmission)
sum(is.na(Transmission))
Transmission[is.na(Transmission)] <- 1
ca$Transmission_cat <- Transmission
ca[,c("Transmission_cat","Transmission")]

#Fuel_type
uniqueFuel <- unique(ca$Fuel_type)

fuelType <- integer() 
fuelType[as.character(ca$Fuel_type) == 'Hybrid'] <- 2
fuelType[as.character(ca$Fuel_type) == 'Gasoline'] <- 0
fuelType[as.character(ca$Fuel_type) == 'Diesel'] <- 1
fuelType[as.character(ca$Fuel_type) == 'Gasoline + LPG'] <- -1
fuelType[as.character(ca$Fuel_type) == 'Gasoline + CNG'] <- -1
fuelType[as.character(ca$Fuel_type) == 'Ethanol'] <- -1
fuelType[as.character(ca$Fuel_type) == 'Hybrid'] <- 2
fuelType[as.character(ca$Fuel_type) == 'Electric'] <- 2
unique(fuelType)
sum(is.na(fuelType))
which(is.na(fuelType))
ca$Fuel_type[c(125343,149424)]
fuelType[c(125343,149424)] <- 2
sum(is.na(fuelType))
ca$Fuel_cat <- fuelType
ca[,c("Fuel_cat","Fuel_type")]


#Drive
Drive <- integer() 
Drive[as.character(ca$Drive) == 'Front wheels'] <- 0
Drive[as.character(ca$Drive) == '""'] <- 0
Drive[as.character(ca$Drive) == '4x4 (attached automatically)'] <- 1
Drive[as.character(ca$Drive) == '4x4 (permanent)'] <- 1
Drive[as.character(ca$Drive) == '4x4 (attached manually)'] <- 1
Drive[as.character(ca$Drive) == 'Rear wheels'] <- -1

unique(Drive)
sum(is.na(Drive))
Drive[is.na(Drive)] <- 0
ca$Drive_cat <-Drive
ca[,c("Drive_cat","Drive")]

#Type
Type <- integer() 
Type[as.character(ca$Type) == 'city_cars'] <- 0
Type[as.character(ca$Type) == 'compact'] <- 0
Type[as.character(ca$Type) == 'small_cars'] <- 0
Type[as.character(ca$Type) == 'sedan'] <- 1
Type[as.character(ca$Type) == 'coupe'] <- -1
Type[as.character(ca$Type) == 'convertible'] <- -1
Type[as.character(ca$Type) == 'SUV'] <- 2
Type[as.character(ca$Type) == 'station_wagon'] <- -2
Type[as.character(ca$Type) == 'minivan'] <- -2

unique(Type)
#No Nas
ca$Type_cat <-Type
ca[,c("Type_cat","Type")]

#new dataframe with numerical values
cars.c <- data.frame(ca$Price, ca$Production_year, ca$Mileage_km, ca$Power_HP, ca$Displacement_cm3,ca$Doors_number, ca$extras,ca$Fuel_cat,ca$Transmission_cat,ca$Drive_cat,ca$Type_cat)
str(cars.c)

#Clustering
#trials for  number of clusters (voting)
set.seed(123)
mini.c <- sample_n(cars.c ,5000)
mini.c <- scale(mini.c)
head(mini.c)

fviz_nbclust(mini.c, kmeans, method = "wss") #8
######
gap_stat <- clusGap(mini.c,FUN = kmeans,nstart = 25, K.max = 10,B = 50)
fviz_gap_stat(gap_stat)
#######
set.seed(1)
mini.ca <- sample_n(cars.c ,5000)
mini.ca <- scale(mini.ca)
head(mini.ca)

fviz_nbclust(mini.ca, kmeans, method = "wss") #8-9
gap_stat <- clusGap(mini.ca,FUN = kmeans,nstart = 25, K.max = 10,B = 50)
fviz_gap_stat(gap_stat)#8-9

set.seed(2)
mini.cb <- sample_n(cars.c ,5000)
mini.cb <- scale(mini.cb)
head(mini.cb)

fviz_nbclust(mini.cb, kmeans, method = "wss") #8-9
gap_stat <- clusGap(mini.cb,FUN = kmeans,nstart = 25, K.max = 10,B = 50)
fviz_gap_stat(gap_stat)

#full dataset k means
set.seed(1)
kme <- kmeans(cars.c, centers = 9, nstart = 25)
kme                   
kme$cluster                    
str(kme)
kme.cluster <- data.frame(cars.c, kme$cluster) 
kme.cluster <-scale(kme.cluster)
str(kme.cluster)

#svm

# Encoding the target feature as factor
s <- kme.cluster #back up for kme.cluster
set.seed(123)
sa<- sample_n(s ,30000)
sa$km.cluster = factor(sa$km.cluster, levels = c(1 , 2, 3, 4, 5, 6, 7, 8 ,9))

set.seed(123)
split = sample.split(sa$km.cluster, SplitRatio = 0.8)

training_set = subset(sa, split == TRUE)
test_set = subset(sa, split == FALSE)

training_set[-12] = scale(training_set[-12])
test_set[-12] = scale(test_set[-12])

set.seed(1)
tune.out <- tune(svm, km.cluster ~ ., data=training_set, kernel="radial",
                 ranges=list(cost=c(0.01, 1, 10, 100, 1000),
                             gamma=c(0.5, 1, 2, 3, 4)))

summary(tune.out)

bestmod <- tune.out$best.model
summary(bestmod)

ypred <- predict(bestmod, test_set)
t <- table(predict=ypred, truth=test_set$km.cluster)

(sum(diag(t))/sum(t))
#[1] 0.9794966

#ROC curve
svmROC <- roc( as.numeric(test_set$km.cluster)~as.numeric(ypred) ,plot=TRUE,print.auc=TRUE,col="red",lwd = 2,print.auc.y=0.4,legacy.axes=TRUE)            

#predict best model kme.cluster
clust1 <- predict(bestmod, kme.cluster)
str(clust1)
final.data1 <- data.frame(kme.cluster,clust1)
str(final.data1)
summary(final.data1)

#diagnostic plots for the price variable for each cluster

ggplot(final.data1, 
       aes(x = ca.Price, 
           y = clust1, 
           fill = clust1)) +
  geom_density_ridges() + 
  theme_ridges() +
  labs(title="Used cars price distribution by cluster") +
  theme(legend.position = "none")+
  xlim(-1, 13)

ggplot(final.data1, aes(x = clust1, 
                        y = ca.Price)) +
  geom_boxplot(notch = TRUE, 
               fill = "darkblue", 
               alpha = .7) +
  labs(title = "Price distribution by cluster")+
  ylim(-1, 20)

# filtering rows by cluster value
df1 <- filter(final.data1,clust1 == 1)
df2 <- filter(final.data1,clust1 == 2)
df3 <- filter(final.data1,clust1 == 3)
df4 <- filter(final.data1,clust1 == 4)
df5 <- filter(final.data1,clust1 == 5)
df6 <- filter(final.data1,clust1 == 6)
df7 <- filter(final.data1,clust1 == 7)
df8 <- filter(final.data1,clust1 == 8)
df9 <- filter(final.data1,clust1 == 9)

# random forest regression at each cluster

# df1 random forest
rf.df1= select(df1, 1:11)
str(rf.df1)

split= sample.split(rf.df1$ca.Price, SplitRatio= 0.7)
train.c1= subset(rf.df1, split == TRUE)
test.c1= subset(rf.df1,split == FALSE)

set.seed(123)
rf.pros.c1<-randomForest(train.c1$ca.Price~.,data = train.c1)
summary(rf.pros.c1)

plot(rf.pros.c1)
which.min(rf.pros.c1$mse)
#[1] 426

set.seed(123)
rf.pros.1<-randomForest(train.c1$ca.Price~.,data = train.c1,ntree=426)
rf.pros.1
#% Var explained: 65.47

varImpPlot(rf.pros.1)
importance(rf.pros.1)

rf.pros.test.c1<-predict(rf.pros.1,newdata = test.c1)
rf.resid1<-(rf.pros.test.c1-test.c1$ca.Price)
mean(rf.resid1^2)
#[1] 1.497148

# df2 random forest
rf.df2= select(df2, 1:11)
str(rf.df2)

split= sample.split(rf.df2$ca.Price, SplitRatio= 0.7)
train.c2= subset(rf.df2, split == TRUE)
test.c2= subset(rf.df2,split == FALSE)

set.seed(123)
rf.pros.c2<-randomForest(train.c2$ca.Price~.,data = train.c2)
summary(rf.pros.c2)

plot(rf.pros.c2)
which.min(rf.pros.c2$mse)
#[1] 281

set.seed(123)
rf.pros.2<-randomForest(train.c2$ca.Price~.,data = train.c2,ntree=281)
rf.pros.2
#% Var explained: 86.21

varImpPlot(rf.pros.2)
importance(rf.pros.2)

rf.pros.test.c2<-predict(rf.pros.2,newdata = test.c2)
rf.resid2<-(rf.pros.test.c2-test.c2$ca.Price)
mean(rf.resid2^2)
#[1] 0.01167635

#df3 random forest
rf.df3= select(df3, 1:11)
str(rf.df3)

split= sample.split(rf.df3$ca.Price, SplitRatio= 0.7)
train.c3= subset(rf.df3, split == TRUE)
test.c3= subset(rf.df3,split == FALSE)

set.seed(123)
rf.pros.c3<-randomForest(train.c3$ca.Price~.,data = train.c3)
summary(rf.pros.c3)

plot(rf.pros.c3)
which.min(rf.pros.c3$mse)
#[1] 478

set.seed(123)
rf.pros.3<-randomForest(train.c3$ca.Price~.,data = train.c3,ntree=478)
rf.pros.3
# % Var explained: 85.34

varImpPlot(rf.pros.3)
importance(rf.pros.3)

rf.pros.test.c3<-predict(rf.pros.3,newdata = test.c3)
rf.resid3<-(rf.pros.test.c3-test.c3$ca.Price)
mean(rf.resid3^2)
#[1] 0.01036242

# df4 random forest
rf.df4= select(df4, 1:11)
str(rf.df4)

split= sample.split(rf.df4$ca.Price, SplitRatio= 0.7)
train.c4= subset(rf.df4, split == TRUE)
test.c4= subset(rf.df4,split == FALSE)

set.seed(123)
rf.pros.c4<-randomForest(train.c4$ca.Price~.,data = train.c4)
summary(rf.pros.c4)

plot(rf.pros.c4)
which.min(rf.pros.c4$mse)
#[1] 479

set.seed(123)
rf.pros.4<-randomForest(train.c4$ca.Price~.,data = train.c4,ntree=479)
rf.pros.4
# % Var explained: 90.92

varImpPlot(rf.pros.4)
importance(rf.pros.4)

rf.pros.test.c4<-predict(rf.pros.4,newdata = test.c4)
rf.resid4<-(rf.pros.test.c4-test.c4$ca.Price)
mean(rf.resid4^2)
#[1] 0.008292667

# df5 random forest
rf.df5= select(df5, 1:11)
str(rf.df5)

split= sample.split(rf.df5$ca.Price, SplitRatio= 0.7)
train.c5= subset(rf.df5, split == TRUE)
test.c5= subset(rf.df5,split == FALSE)


set.seed(123)
rf.pros.c5<-randomForest(train.c5$ca.Price~.,data = train.c5)
summary(rf.pros.c5)

plot(rf.pros.c5)
which.min(rf.pros.c5$mse)
#[1] 490

set.seed(123)
rf.pros.5<-randomForest(train.c5$ca.Price~.,data = train.c5,ntree=490)
rf.pros.5
#% Var explained: 80.79

varImpPlot(rf.pros.5)
importance(rf.pros.5)

rf.pros.test.c5<-predict(rf.pros.5,newdata = test.c5)
rf.resid5<-(rf.pros.test.c5-test.c5$ca.Price)
mean(rf.resid5^2) #[1] 0.01141423

#  df6 random forest

rf.df6= select(df6, 1:11)
str(rf.df6)

split= sample.split(rf.df6$ca.Price, SplitRatio= 0.7)
train.c6= subset(rf.df6, split == TRUE)
test.c6= subset(rf.df6,split == FALSE)

set.seed(123)
rf.pros.c6<-randomForest(train.c6$ca.Price~.,data = train.c6)
summary(rf.pros.c6)

plot(rf.pros.c6)
which.min(rf.pros.c6$mse)
#[1] 374


set.seed(123)
rf.pros.6<-randomForest(train.c6$ca.Price~.,data = train.c6,ntree=374)
rf.pros.6
#% Var explained: 55.06

varImpPlot(rf.pros.6)
importance(rf.pros.6)

rf.pros.test.c6<-predict(rf.pros.6,newdata = test.c6)
rf.resid6<-(rf.pros.test.c6-test.c6$ca.Price)
mean(rf.resid6^2)
#[1] 0.8738356

# df7 random forest

rf.df7= select(df7, 1:11)
str(rf.df7)

split= sample.split(rf.df7$ca.Price, SplitRatio= 0.7)
train.c7= subset(rf.df7, split == TRUE)
test.c7= subset(rf.df7,split == FALSE)

set.seed(123)
rf.pros.c7<-randomForest(train.c7$ca.Price~.,data = train.c7)
summary(rf.pros.c7)

plot(rf.pros.c7)
which.min(rf.pros.c7$mse)
#[1] 472

set.seed(123)
rf.pros.7<-randomForest(train.c7$ca.Price~.,data = train.c7,ntree=472)
rf.pros.7
# % Var explained: 82.33

varImpPlot(rf.pros.7)
importance(rf.pros.7)

rf.pros.test.c7<-predict(rf.pros.7,newdata = test.c7)
rf.resid7<-(rf.pros.test.c7-test.c7$ca.Price)
mean(rf.resid7^2)

#[1] 0.1376878

# df8 random forest
rf.df8= select(df8, 1:11)
str(rf.df8)

split= sample.split(rf.df8$ca.Price, SplitRatio= 0.7)
train.c8= subset(rf.df8, split == TRUE)
test.c8= subset(rf.df8,split == FALSE)

set.seed(123)
rf.pros.c8<-randomForest(train.c8$ca.Price~.,data = train.c8)
summary(rf.pros.c8)

plot(rf.pros.c8)
which.min(rf.pros.c8$mse)
#[1] 496

set.seed(123)
rf.pros.8<-randomForest(train.c8$ca.Price~.,data = train.c8,ntree=472)
rf.pros.8
#% Var explained: 47.75

varImpPlot(rf.pros.8)
importance(rf.pros.8)

rf.pros.test.c8<-predict(rf.pros.8,newdata = test.c8)
rf.resid8<-(rf.pros.test.c8-test.c8$ca.Price)
mean(rf.resid8^2)
#[1] 0.2592646

# df9 random forest
rf.df9= select(df9, 1:11)
str(rf.df9)

split= sample.split(rf.df9$ca.Price, SplitRatio= 0.7)
train.c9= subset(rf.df9, split == TRUE)
test.c9= subset(rf.df9,split == FALSE)

set.seed(123)
rf.pros.c9<-randomForest(train.c9$ca.Price~.,data = train.c9)
summary(rf.pros.c9)

plot(rf.pros.c9)
which.min(rf.pros.c9$mse)
#[1] 362

set.seed(123)
rf.pros.9<-randomForest(train.c9$ca.Price~.,data = train.c9,ntree=362)
rf.pros.9
# % Var explained: 85.9

varImpPlot(rf.pros.9)
importance(rf.pros.9)

rf.pros.test.c9<-predict(rf.pros.9,newdata = test.c9)
rf.resid9<-(rf.pros.test.c9-test.c9$ca.Price)
mean(rf.resid9^2)
#[1] 0.05161969

#Prediction

import.data <- sample_n(final.data1,100)
cluster.prediction <-predict(bestmod, import.data)
cl.pred <-data.frame(import.data,cluster.prediction) 

price.prediction <- if (cl.pred$cluster.prediction==1) {predict(rf.pros.1, newdata = cl.pred)
  
}else if(cl.pred$cluster.prediction==2 ) {predict(rf.pros.2, newdata = cl.pred)
  
} else if(cl.pred$cluster.prediction==3 ) {predict(rf.pros.3, newdata = cl.pred)
  
} else if(cl.pred$cluster.prediction==4 ) {predict(rf.pros.4, newdata = cl.pred)
  
} else if(cl.pred$cluster.prediction==5 ) {predict(rf.pros.5, newdata = cl.pred)
  
} else if(cl.pred$cluster.prediction==6 ) {predict(rf.pros.6, newdata = cl.pred)
  
} else if(cl.pred$cluster.prediction==7 ) {predict(rf.pros.7, newdata = cl.pred)
  
} else if(cl.pred$cluster.prediction==8 ) {predict(rf.pros.8, newdata = cl.pred)
  
} else {predict(rf.pros.9, newdata = cl.pred)
}

results <-data.frame(cl.pred,price.prediction)
View(results)

error.perc <- (((results$ca.Price-results$price.prediction)/results$ca.Price)*100)
summary(error.perc)
#Median 5.066

#percentage error

error.perc <- (((results$ca.Price-results$price.prediction)/results$ca.Price)*100)
error.summary<- summary(error.perc)
error.summary
boxplot(error.perc)

error.median <- median(error.perc)
error.IQR <- IQR(error.perc)
error.min <- as.numeric(error.median-1.5*error.IQR)
error.max <- as.numeric(error.median+1.5*error.IQR)




non.outlier.error <- error.perc[(error.perc > error.min) & (error.perc < error.max)]
boxplot(non.outlier.error)
non.outlier.error
summary(non.outlier.error)

hist(error.perc)

hist(non.outlier.error, col = 'blue', border = "white")
boxplot(non.outlier.error, col = 'blue', border = "white")
