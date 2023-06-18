library(dplyr)
library(tidyr)
library(tidyverse)
library(caret)
library(ggplot2)
library(ggcorrplot)


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


###########create new variables from Features

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
