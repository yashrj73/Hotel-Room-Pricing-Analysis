# Project Title:  Hotel Room Pricing In The Indian Market
# NAME: Yashraj Shrivastava 
# EMAIL: yashrj73@gmail.com
# COLLEGE : SRM University

#Reading the data into R
Cities.df<-read.csv(paste("Cities42.csv"))

#Summary of data
summary(Cities.df)

#Formulating the probelm as: y=b0+b1*x1+b2*x2+..
#dependent variable (y)= RoomRent
#3 most significant independent variables (x):HotelCapacity(x1),StarRating(x2), Airport(x3)

#Visualisation of individual variables
#Frequency vs RoomRent
boxplot(Cities.df$RoomRent,xlab="Room Rent",ylab="Frequency")

#Frequency vs HotelCapacity
boxplot(Cities.df$HotelCapacity,xlab="HotelCapacity",ylab="Frequency")

#Frequency vs StarRating
mytable<-with(Cities.df,table(StarRating))
mytable

#Frequency vs Airport
boxplot(Cities.df$Airport,xlab="Distance from Airport",ylab="Frequency")

#Scatter plot of variable correlation pair wise:

#Pair wise correlation for all variables:
library(car)
scatterplotMatrix(~RoomRent+HotelCapacity+StarRating+Airport,data=Cities.df)

#Y versus x1
plot(RoomRent~HotelCapacity,data=Cities.df,cex=0.6,ylim=c(0,75000),col="blue")
abline(lm(Cities.df$RoomRent~Cities.df$HotelCapacity))

#Y versus x2
plot(RoomRent~StarRating,data=Cities.df,cex=0.6,col="blue",ylim=c(0,75000))
abline(lm(Cities.df$RoomRent~Cities.df$StarRating))

#Y versus x3
plot(RoomRent~Airport,data=Cities.df,cex=0.6,ylim=c(0,75000))
abline(lm(Cities.df$RoomRent~Cities.df$Airport))

#Corrgram of dataset
myvars <- c("RoomRent","HotelCapacity", "StarRating", "Airport")
cities.df<-Cities.df[myvars]
corrgram(cities.df, order=TRUE, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)

#Variance-Covariance Matrix
myvars <- c("RoomRent","HotelCapacity", "StarRating", "Airport")
cities.df<-Cities.df[myvars]
#Variance
var(cities.df)
#Co-variance
cov(cities.df)

#Hypothesis:
#H1-The rent of rooms of higher star rating hotels are higher than the rent of low star rating hotels.
#H2-The rent of rooms of hotels closer to the airport are higher than the rent of hotels far from airport. 


#Testing hypothesis using t-tests
#t-test for H1
t.test(Cities.df$RoomRent,Cities.df$StarRating)

#t-test for H2
t.test(Cities.df$RoomRent,Cities.df$Airport)

#Regression Model:
#Establishing the effect of StarRating on RoomRent using simple model:
#RoomRent= a0 + a1*StarRating + c
model1 <-lm(RoomRent~StarRating,data = Cities.df)
summary(model1)

#Establishing the effect of Airport on RoomRent using simple model:
#RoomRent= a0 + a1*Airport + c
model2 <-lm(RoomRent~Airport,data = Cities.df)
summary(model1)

#############Finding the Predictors###############
#Checking for variables with small values.
#Checking for Population, CityRank, IsMetroCity, IsTouristDestination, IsWeekend
library("leaps", lib.loc="~/R/win-library/3.4")
model <- RoomRent~Population+CityRank+IsMetroCity+IsTouristDestination+IsWeekend
leap1 <- regsubsets(model, data = Cities.df, nbest=1, really.big=T)
plot(leap1, scale="adjr2")

#Checking for IsNewYearEve, Date, StarRating, Airport
model <- RoomRent~IsNewYearEve+Date+StarRating+Airport
leap1 <- regsubsets(model, data = Cities.df, nbest=1, really.big=T)
plot(leap1, scale="adjr2")

#Checking for FreeWifi, FreeBreakfast, HotelCapacity, HasSwimmingPool
model <- RoomRent~FreeWifi+FreeBreakfast+HotelCapacity+HasSwimmingPool
leap1 <- regsubsets(model, data = Cities.df, nbest=1, really.big=T)
plot(leap1, scale="adjr2")

#Checking for variable with large values.
#Checking for CityName:
model <- lm(RoomRent~CityName, data = Cities.df)
summary(model)

#Checking for HotelName:
model <- lm(RoomRent~HotelName, data = Cities.df)
summary(model)

#Checking for HotelAddress:
model <- lm(RoomRent~HotelAddress, data = Cities.df)
summary(model)

#Checking for HotelDescription:
model <- lm(RoomRent~HotelDescription, data = Cities.df)
summary(model)

#Checking for HotelPincode:
model <- lm(RoomRent~HotelPincode, data = Cities.df)
summary(model)
            
#Establishing a regression model for rest of the independent variables:
#y=b0+b1*x1+b2*x2+..
#where y=RoomRent, x1= CityName,x2= Population,x3=IsMetroCity,x4=IsTouristDestination,x5= HotelName,x6= StarRating, x7=Airport, x8=HasSwimmingPool.
model <- lm(RoomRent~CityName+Population+IsMetroCity+IsTouristDestination+HotelName+StarRating+Airport+HasSwimmingPool,data=Cities.df)
summary(model)