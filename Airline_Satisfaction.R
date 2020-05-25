############################################################
# ------------------------------------------------------------------------------
# Descriptive Statistics 
# Variables excluded from the model:
# ------------------------------------------------------------------------------
############################################################


# ------------------------------------------------------------------------------
# Airline Name # Produced only NA's in the output
# Airline code # It made the model output to difficult to interpret because it assigned coefficients to every airline code
# Origin City, Origin State, Destination City # same problem as above
# Flight Cancelled # None of the flights were canceled so the factor is only one level
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# NA's 
# Descriptive Statistics of NAs
# Finding and analyzing where NAs exist and its value
# ------------------------------------------------------------------------------

library(readxl)
library(graphics)

SatisfactionSurvey <- read_excel("C:/Users/16023/Desktop/IST 687/SatisfactionSurvey2_2_2_2.xlsx")
sum(is.na(SatisfactionSurvey$Satisfaction))
sum(is.na(SatisfactionSurvey$'Airline Status'))
sum(is.na(SatisfactionSurvey$Age))
sum(is.na(SatisfactionSurvey$Gender))
sum(is.na(SatisfactionSurvey$'Price Sensitivity'))
sum(is.na(SatisfactionSurvey$'Year of First Flight'))
sum(is.na(SatisfactionSurvey$'No of Flights p.a.'))
sum(is.na(SatisfactionSurvey$'Type of Travel'))
sum(is.na(SatisfactionSurvey$'No. of other Loyalty Cards'))
sum(is.na(SatisfactionSurvey$'Shopping Amount at Airport'))
sum(is.na(SatisfactionSurvey$'Eating and Drinking at Airport'))
sum(is.na(SatisfactionSurvey$Class))
sum(is.na(SatisfactionSurvey$'Day of Month'))
sum(is.na(SatisfactionSurvey$'Flight date'))
sum(is.na(SatisfactionSurvey$'Airline Code'))
sum(is.na(SatisfactionSurvey$'Airline Name'))
sum(is.na(SatisfactionSurvey$'Orgin City'))
sum(is.na(SatisfactionSurvey$'Origin State'))
sum(is.na(SatisfactionSurvey$'Destination City'))
sum(is.na(SatisfactionSurvey$'Destination State'))
sum(is.na(SatisfactionSurvey$'Scheduled Departure Hour'))
sum(is.na(SatisfactionSurvey$'Departure Delay in Minutes'))
sum(is.na(SatisfactionSurvey$'Arrival Delay in Minutes'))
sum(is.na(SatisfactionSurvey$'Flight cancelled'))
sum(is.na(SatisfactionSurvey$'Flight time in minutes'))
sum(is.na(SatisfactionSurvey$'Flight Distance'))
sum(is.na(SatisfactionSurvey$'Arrival Delay greater 5 Mins'))
sum(is.na(SatisfactionSurvey$'% of Flight with other Airlines'))
View(SatisfactionSurvey)


Nasincol <- c(sum(is.na(SatisfactionSurvey$Satisfaction)),sum(is.na(SatisfactionSurvey$'Airline Status')), sum(is.na(SatisfactionSurvey$Age)), sum(is.na(SatisfactionSurvey$Gender)),  sum(is.na(SatisfactionSurvey$'Price Sensitivity')), sum(is.na(SatisfactionSurvey$'Year of First Flight')),  sum(is.na(SatisfactionSurvey$'No of Flights p.a.')), sum(is.na(SatisfactionSurvey$'Type of Travel')),  sum(is.na(SatisfactionSurvey$'No. of other Loyalty Cards')), sum(is.na(SatisfactionSurvey$'Shopping Amount at Airport')),  sum(is.na(SatisfactionSurvey$'Eating and Drinking at Airport')), sum(is.na(SatisfactionSurvey$Class)),  sum(is.na(SatisfactionSurvey$'Day of Month')), sum(is.na(SatisfactionSurvey$'Flight date')), sum(is.na(SatisfactionSurvey$'Airline Code')), sum(is.na(SatisfactionSurvey$'Airline Name')), sum(is.na(SatisfactionSurvey$'Orgin City')), sum(is.na(SatisfactionSurvey$'Origin State')), sum(is.na(SatisfactionSurvey$'Destination City')), sum(is.na(SatisfactionSurvey$'Destination State')), sum(is.na(SatisfactionSurvey$'Scheduled Departure Hour')),  sum(is.na(SatisfactionSurvey$'Departure Delay in Minutes')), sum(is.na(SatisfactionSurvey$'Arrival Delay in Minutes')),  sum(is.na(SatisfactionSurvey$'Flight cancelled')),  sum(is.na(SatisfactionSurvey$'Flight time in minutes')),  sum(is.na(SatisfactionSurvey$'Flight Distance')),  sum(is.na(SatisfactionSurvey$'Arrival Delay greater 5 Mins')), sum(is.na(SatisfactionSurvey$'% of Flight with other Airlines')))
ColumnNames <- "Column Names"
barNA <- barplot(Nasincol, main="NA's in each Column", names.arg=colnames(SatisfactionSurvey), las=2)

# ------------------------------------------------------------------------------
# Descriptive statistics by type of travel
# Data Cleansing
# ------------------------------------------------------------------------------

SatisfactionSurvey <- na.omit(SatisfactionSurvey)
SatisfactionSurvey$Satisfaction <- as.numeric(gsub(",","", SatisfactionSurvey$Satisfaction))
SatisfactionSurvey$'No of Flights p.a.' <- as.numeric(gsub(",","", SatisfactionSurvey$'No of Flights p.a.'))
SatisfactionSurvey$'Departure Delay in Minutes' <- as.numeric(gsub(",","", SatisfactionSurvey$'Departure Delay in Minutes'))
SatisfactionSurvey$'Arrival Delay in Minutes' <- as.numeric(gsub(",","", SatisfactionSurvey$'Arrival Delay in Minutes'))
SatisfactionSurvey$'Flight time in minutes' <- as.numeric(gsub(",","", SatisfactionSurvey$'Flight time in minutes'))
SatisfactionSurvey$'Flight Distance' <- as.numeric(gsub(",","", SatisfactionSurvey$'Flight Distance'))
SatisfactionSurvey$'Type of Travel' <- as.character(gsub(" ","", SatisfactionSurvey$'Type of Travel'))

# ------------------------------------------------------------------------------
# Summary 
# Sections: will contain all the Graphing/ histograms of initial Satisfaction findings
# ------------------------------------------------------------------------------


SatisfactionSurveyBusiness <- SatisfactionSurvey[SatisfactionSurvey$'Type of Travel'== 'Businesstravel' , ]
SatisfactionSurveyPersonal <- SatisfactionSurvey[SatisfactionSurvey$'Type of Travel'=='PersonalTravel',]
SatisfactionSurveyMileage <- SatisfactionSurvey[SatisfactionSurvey$'Type of Travel'=='Mileagetickets',]


colnames(SatisfactionSurvey)
length(SatisfactionSurveyBusiness)

# ------------------------------------------------------------------------------
# Summary and graph/histogram of all the data
# ------------------------------------------------------------------------------

hist(SatisfactionSurvey$Satisfaction)
summary(SatisfactionSurvey$Satisfaction)
sd(SatisfactionSurvey$Satisfaction)


# ------------------------------------------------------------------------------
# This is the data Subcategorized by Type of Travel
# ------------------------------------------------------------------------------

SatisfactionSurveyBusiness <- SatisfactionSurvey[SatisfactionSurvey$'Type of Travel'== 'Businesstravel' , ]
SatisfactionSurveyPersonal <- SatisfactionSurvey[SatisfactionSurvey$'Type of Travel'=='PersonalTravel',]
SatisfactionSurveyMileage <- SatisfactionSurvey[SatisfactionSurvey$'Type of Travel'=='Mileagetickets',]
summary(SatisfactionSurveyBusiness$Satisfaction)
summary(SatisfactionSurveyPersonal$Satisfaction)
summary(SatisfactionSurveyMileage$Satisfaction)
hist(SatisfactionSurveyBusiness$Satisfaction)
hist(SatisfactionSurveyPersonal$Satisfaction)
hist(SatisfactionSurveyMileage$Satisfaction)
sd(SatisfactionSurveyBusiness$Satisfaction)
sd(SatisfactionSurveyPersonal$Satisfaction)
sd(SatisfactionSurveyMileage$Satisfaction)


# ------------------------------------------------------------------------------
# This is the data subcategorized by Airline Status 
# ------------------------------------------------------------------------------

SatisfactionSurveyGold <- SatisfactionSurvey[SatisfactionSurvey$'Airline Status' == 'Gold',]
SatisfactionSurveyBlue <- SatisfactionSurvey[SatisfactionSurvey$'Airline Status' == 'Blue',]
SatisfactionSurveySilver <- SatisfactionSurvey[SatisfactionSurvey$'Airline Status' == 'Silver',]
SatisfactionSurveyPlatinum <- SatisfactionSurvey[SatisfactionSurvey$'Airline Status' == 'Platinum',]
hist(SatisfactionSurveyGold$Satisfaction)
hist(SatisfactionSurveyBlue$Satisfaction)
hist(SatisfactionSurveySilver$Satisfaction)
hist(SatisfactionSurveyPlatinum$Satisfaction)
summary(SatisfactionSurveyGold$Satisfaction)
summary(SatisfactionSurveyBlue$Satisfaction)
summary(SatisfactionSurveySilver$Satisfaction)
summary(SatisfactionSurveyPlatinum$Satisfaction)
sd(SatisfactionSurveyGold$Satisfaction)
sd(SatisfactionSurveyBlue$Satisfaction)
sd(SatisfactionSurveySilver$Satisfaction)
sd(SatisfactionSurveyPlatinum$Satisfaction)

# ------------------------------------------------------------------------------
# Satisfaction by State 
# Creating an initial Heat Map of initial travel for data preparation
# and Descriptive Statistical analysis
# ------------------------------------------------------------------------------

install.packages("reshape2")
library(reshape2)
satairport <- SatisfactionSurvey[ ,c(1,17)]
airportdf <- tapply(satairport$Satisfaction, satairport$'Airline Name', mean)
View(airportdf)
satcity <- SatisfactionSurvey[ , c(1,18,20)]
Ocitydf <- tapply(satcity$Satisfaction, satcity$'Orgin City' , mean)
Dcitydf <- tapply(satcity$Satisfaction, satcity$'Destination City' , mean)
Ocitydf
Dcitydf
satmap <- SatisfactionSurvey[ ,c(1,19,21)]
str(satmap)
satmap$'Origin State' <- as.factor(satmap$'Origin State')
satmap$'Destination State' <- as.factor(satmap$'Destination State')
origin <- tapply(satmap$Satisfaction, satmap$'Origin State', mean)
origin
destination <- tapply(satmap$Satisfaction, satmap$'Destination State', mean)
satbystates <- data.frame(origin, destination, origin+destination, (origin+destination)/2)
colnames(satbystates) <- c("Origin Rate", "Destination Rate", "Sum", "Average")

library(ggplot2)
install.packages("maps")


str(state.name)
head(state.name)
state.name[3]
#
dummyDF <- data.frame(state.name, stringsAsFactors=FALSE)
dummyDF$state <- tolower(dummyDF$state.name)

#
us <- map_data("state")       ## map_data is a function in maps package
us
#
map.simple <- ggplot(dummyDF, aes(map_id = state))
map.simple <- map.simple +
  geom_map(map = us, fill="white", color="black")
map.simple
map.simple <- map.simple +
  expand_limits(x = us$long, y = us$lat)
map.simple


###########################################################
# ------------------------------------------------------------------------------
# Final Project - Simple Linear Regression Model 
# ------------------------------------------------------------------------------
###########################################################

# Read in the data 
library(pacman)
p_load(readxl)

data <- read_excel("SatisfactionSurvey2_2_2_2.xlsx")

# Now search for NA's in the data 

any(is.na(data))

# Remove the NA's 

data <- na.omit(data)

# Now create the model 

lm1 <- lm(data = data, data$Satisfaction ~ data$'Airline Status' +
            data$Age +
            data$Gender +
            data$'Price Sensitivity' +
            data$'Year of First Flight' +
            data$'No of Flights p.a.' +
            data$'Type of Travel' +
            data$'No. of other Loyalty Cards' +
            data$'Shopping Amount at Airport' +
            data$'Eating and Drinking at Airport' +
            data$Class +
            data$'Day of Month' +
            data$'Flight date' +
            data$'Scheduled Departure Hour' +
            data$'Departure Delay in Minutes' +
            data$'Arrival Delay in Minutes' +
            data$'Flight time in minutes' +
            data$'Flight Distance' +
            data$'Arrival Delay greater 5 Mins' +
            data$'% of Flight with other Airlines')

summary(lm1)

plot(lm1)


############################################################
# ------------------------------------------------------------------------------
# Final Project - Ordinal Logistic Regression Model
# ------------------------------------------------------------------------------
############################################################

# here is the website I used for reference: 
# https://towardsdatascience.com/implementing-and-interpreting-ordinal-logistic-regression-1ee699274cf5

# Install some packages (MASS is the most important)

install.packages("pacman")
library(pacman)

p_load(broom, MASS, tidyverse, ggplot2, readxl)


# ------------------------------------------------------------------------------
# Ordinal Logistic Regression
# ------------------------------------------------------------------------------


# 1. Read in the data and omit the NA's: 

survey_data <- na.omit(SatisfactionSurvey2_2_2_2)

# Now lets check out some summary statistics of our model
summary(survey_data)
# the average satisfaction score turns out to be 3.384
hist(typeoftravel, satisfaction)

# check the number of observations we have

NROW(survey_data)

# we need to make the satisfaction score a factor and order it
# This basically orders the scores as 5 > 4 > 3 > 2 > 1 

satisfaction <- survey_data$Satisfaction
satisfaction <- factor(satisfaction, levels = c("1","2","2.5",
                                                "3", "3.5", "4","4.5",
                                                "5"), ordered = TRUE)

# Check the levels
levels(satisfaction)
max(satisfaction)

# lets look at a bar plot of the satisfaction levels

ggplot(survey_data, aes(satisfaction, fill = satisfaction)) +
  geom_bar() + scale_fill_hue(c = 60) + 
  theme(legend.position = 'none')
#============================================================================
# We can use categorical and continuous data in the model
# we'll define some variables before we build the model.

# flight_cancelled will be run through the factor function

flight_cancelled <- factor(survey_data$'Flight cancelled', ordered = FALSE)

# check the levels
levels(flight_cancelled)

# none of the flights were cancelled. We'll omit this variable from the model

# now we'll do arrival delay > 5 mins

arrival_delay <- factor(survey_data$'Arrival Delay greater 5 Mins', ordered = FALSE)
levels(arrival_delay)

# price sensitivity
# note: the continuous variables DO NOT need to be ran through the factor function

price_sensitivity <- survey_data$'Price Sensitivity'

# airline status
# originally I ran this through the factor function but it was unecessary

airline_status <- survey_data$'Airline Status'

# Gender

gender <- factor(survey_data$Gender, ordered = FALSE)
levels(gender)

# Type of Travel

typeoftravel <- survey_data$'Type of Travel'
typeoftravel <- factor(typeoftravel, ordered = FALSE)
levels(typeoftravel)


# Model 1
# we'll use the plor function from the MASS package
# plor = proportional linear odds regression 
#====================================================================================

model1 <- polr(satisfaction ~ airline_status +
                 survey_data$Age +
                 gender +
                 price_sensitivity +
                 survey_data$'Year of First Flight' +
                 survey_data$'No of Flights p.a.' +
                 survey_data$'% of Flight with other Airlines' +
                 typeoftravel +
                 survey_data$'No. of other Loyalty Cards' +
                 survey_data$'Shopping Amount at Airport' +
                 survey_data$'Eating and Drinking at Airport' +
                 survey_data$Class +
                 survey_data$'Day of Month' +
                 survey_data$'Scheduled Departure Hour' +
                 survey_data$'Departure Delay in Minutes' +
                 survey_data$'Arrival Delay in Minutes' +
                 survey_data$'Flight time in minutes' +
                 survey_data$'Flight Distance' +
                 arrival_delay,
               data = survey_data, method = "logistic", Hess = TRUE)


summary(model1)

# We'll use the t-value and compare it to the normal distribution to calculate
# p-values and test for statistical significance

summary_table <- coef(summary(model1))
pval <- pnorm(abs(summary_table[,"t value"]), lower.tail = F)*2
summary_table <- cbind(summary_table, "p value" = round(pval, 3))
summary_table

# Put the summary into a data frame

model1_df <- data.frame(summary_table)
model1_df

# Export the table to excel

write.csv(model1_df, 'model1_df.csv')

# Model 2
#========================================================================

# Ok, now I'll omit the variables which weren't statistically significant
# They are: 
# % of flight w/ other airlines
# no. of other loyalty cards
# eating and drinking at the airport
# Day of month
# Dep. Delay in mins
# Arrival delay in mins
# Flight time in mins
# Flight Distance

# Before I begin, let me re-name variables: 

age <- survey_data$Age
yearfirstflight <- survey_data$'Year of First Flight'
flightspa <- survey_data$'No of Flights p.a.'
dephour <- survey_data$'Scheduled Departure Hour'
shoppingamount <- survey_data$'Shopping Amount at Airport'
class <- factor(survey_data$Class)

# Now I'll rebuild the model

model2 <- polr(satisfaction ~
                 airline_status + age +
                 gender + price_sensitivity +
                 yearfirstflight +
                 flightspa +
                 typeoftravel +
                 shoppingamount +
                 class +
                 dephour + arrival_delay,
               data = survey,
               method = "logistic",
               Hess = TRUE)

# Now lets see the summary

summary(model2)

# We'll use the t-value to calculate the p-value from the model

summary_table2 <- coef(summary(model2))
pval <- pnorm(abs(summary_table2[,"t value"]), lower.tail = F)*2
summary_table2 <- cbind(summary_table2, "p value" = round(pval, 3))
summary_table2

# put it into a data frame

model2_df <- data.frame(summary_table2)
model2_df

# Now lets export the table. 

write.csv(model2_df, 'Model2.csv')

# calculating a prediction
#============================================================

# use the intercepts and coefficients to make predictions

logit_1 <- 	23.5546295609 - ((1.6624047883*1) + 
                               (35*-0.0038116973) +
                               (-0.1125294852*1) +
                               (0.0148895899*2005) +
                               (-0.0088588513*10) +
                               (-2.8623654934*1) +
                               (	0.0003610423*60) +
                               (	-0.2098222068*1) +
                               (	0.0113269368*18) +
                               (-0.9756755701*1))


logit_2 <- 26.5647146709 - ((1.6624047883*1) + 
                              (35*-0.0038116973) +
                              (-0.1125294852*1) +
                              (0.0148895899*2005) +
                              (-0.0088588513*10) +
                              (-2.8623654934*1) +
                              (	0.0003610423*60) +
                              (	-0.2098222068*1) +
                              (	0.0113269368*18) +
                              (-0.9756755701*1))

logit_3 <- 26.5650099453 - ((1.6624047883*1) + 
                              (35*-0.0038116973) +
                              (-0.1125294852*1) +
                              (0.0148895899*2005) +
                              (-0.0088588513*10) +
                              (-2.8623654934*1) +
                              (	0.0003610423*60) +
                              (	-0.2098222068*1) +
                              (	0.0113269368*18) +
                              (-0.9756755701*1))

logit_4 <- 	28.7597098017 - ((1.6624047883*1) + 
                               (35*-0.0038116973) +
                               (-0.1125294852*1) +
                               (0.0148895899*2005) +
                               (-0.0088588513*10) +
                               (-2.8623654934*1) +
                               (	0.0003610423*60) +
                               (	-0.2098222068*1) +
                               (	0.0113269368*18) +
                               (-0.9756755701*1))

logit_5 <- 28.7597841666 - ((1.6624047883*1) + 
                              (35*-0.0038116973) +
                              (-0.1125294852*1) +
                              (0.0148895899*2005) +
                              (-0.0088588513*10) +
                              (-2.8623654934*1) +
                              (	0.0003610423*60) +
                              (	-0.2098222068*1) +
                              (	0.0113269368*18) +
                              (-0.9756755701*1))

logit_6 <- 31.8373790535 - ((1.6624047883*1) + 
                              (35*-0.0038116973) +
                              (-0.1125294852*1) +
                              (0.0148895899*2005) +
                              (-0.0088588513*10) +
                              (-2.8623654934*1) +
                              (	0.0003610423*60) +
                              (	-0.2098222068*1) +
                              (	0.0113269368*18) +
                              (-0.9756755701*1))

logit_7 <- 31.8373897345 - ((1.6624047883*1) + 
                              (35*-0.0038116973) +
                              (-0.1125294852*1) +
                              (0.0148895899*2005) +
                              (-0.0088588513*10) +
                              (-2.8623654934*1) +
                              (	0.0003610423*60) +
                              (	-0.2098222068*1) +
                              (	0.0113269368*18) +
                              (-0.9756755701*1))

# now use the logits and exponentiate to determine probabilities
# these are ONLY probabilities for the individual attributes
# listed in the word document write up for this model
#============================================================

prob_1 <- exp(logit_1)/(1 + exp(logit_1))

prob_2 <- exp(logit_2)/(1 + exp(logit_2))
prob_2 <- prob_2 - prob_1

prob_3 <- exp(logit_3)/(1 + exp(logit_3))
prob_3 <- prob_3 - prob_2 - prob_1

prob_4 <- exp(logit_4)/(1 + exp(logit_4))
prob_4 <- prob_4 - prob_3 - prob_2 - prob_1

prob_5 <- exp(logit_5)/(1 + exp(logit_5))
prob_5 <- prob_5 - prob_4 - prob_3 - prob_2 - prob_1

prob_6 <- exp(logit_6)/(1 + exp(logit_6))
prob_6 <- prob_6 - prob_5 - prob_4 - prob_3 - prob_2 - prob_1

prob_7 <- exp(logit_7)/(1 + exp(logit_7))
prob_7 <- prob_7 - prob_6 - prob_5 - prob_4 - prob_3 - prob_2 - prob_1

prob_8 <- 1 - (prob_1 + prob_2 + prob_3 + prob_4 + prob_5 + prob_6 + prob_7)

# here is my barplot for type of travel
#===========================================================

ggplot(survey_data, aes(typeoftravel, fill = satisfaction)) +
  geom_bar(position = position_dodge()) + scale_fill_hue(c = 60) 


# now we're gonna determine the goodness of fit of model2.
# this is where i reference paralell slopes assumption in word
#============================================================

p_load(Hmisc)
summary(model2)

levels(satisfaction)

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=2.5' = qlogis(mean(y >= 2.5)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=3.5' = qlogis(mean(y >= 3.5)),
    'Y>=4' = qlogis(mean(y >= 4)),
    'Y>=4.5' = qlogis(mean(y >= 4.5)),
    'Y>=5' = qlogis(mean(y >= 5)))
}

(s <- with(model2_df, summary(as.numeric(satisfaction) ~ airline_status +
                                age +
                                gender + 
                                price_sensitivity +
                                yearfirstflight +
                                flightspa +
                                typeoftravel +
                                shoppingamount +
                                class +
                                dephour + 
                                arrival_delay,
                              fun = sf)))
#=============================================================
#https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
#https://towardsdatascience.com/implementing-and-interpreting-ordinal-logistic-regression-1ee699274cf5
#https://stats.idre.ucla.edu/r/faq/ologit-coefficients/





############################################################
# ------------------------------------------------------------------------------
# Final Project - Kernel-Base Support Vector Model (KSVM)
# ------------------------------------------------------------------------------
###########################################################

SatisfactionSurvey <- na.omit(SatisfactionSurvey_CoolAir)
avg <- mean(SatisfactionSurvey$Satisfaction)
install.packages("kernlab")   
library("kernlab")
library(e1071)
install.packages("neuralnet")
library(neuralnet)
install.packages("caret")
library(caret)

summary(SatisfactionSurvey)

randIndex <- sample(1:dim(SatisfactionSurvey)[1])
head(randIndex)
length(randIndex)
dim(SatisfactionSurvey)
#  
#  # In order to split data, create a 2/3 cutpoint and round the number
cutpoint2_3 <- floor(2*dim(SatisfactionSurvey)[1]/3)
# check the 2/3 cutpoint
cutpoint2_3
#  
# create train data set, which contains the first 2/3 of overall data
#  
trainData <- SatisfactionSurvey[randIndex[1:cutpoint2_3],]
dim(trainData)
head(trainData)
#  
# create test data, which contains the left 1/3 of the overall data
#  
testData <- SatisfactionSurvey[randIndex[(cutpoint2_3+1):dim(SatisfactionSurvey)[1]],]
dim(testData)   # check test data set
head(trainData)
trainData$goodsat <- ifelse(trainData$Satisfaction<avg, 0, 1)
testData$goodsat <- ifelse(testData$Satisfaction<avg, 0, 1)

trainData <- trainData[,-1]
testData <- testData[,-1]

str(trainData)
testData$goodsat <- as.factor(testData$goodsat)
modelcs <- ksvm(goodsat ~ Age + Class  + 'Price Sensitivity' + 'Airline Status' + 'Type of Travel',
                data = trainData,
                kernel="rbfdot",
                kpar = "automatic",
                C = 10,
                cross = 10,
                prob.model = TRUE)

prediction <- predict(modelcs, testData)
results <- data.frame(testData$goodsat,round(prediction))
colnames(results) <- c("Actual", "Prediction")  


str(results)
results$Prediction <- as.factor(results$Prediction)
confusionMatrix(results$Actual,results$Prediction)
#Variables I couldnt Use 'Flight cancelled'
#Variables that Lowered the accuracy 'Departure Delay in Minutes', 'Arrival Delay in Minutes', 'Flight Distance', 'No. of other Loyalty Cards'

# Failed NeuralNet Code  the only variable that was used was Age

satSurvey <- SatisfactionSurvey2_2
satSurvey <- na.omit(satSurvey)
randIndex <- sample(1:dim(satSurvey)[1])
head(randIndex)
length(randIndex)
dim(satSurvey)
#  
#  # In order to split data, create a 2/3 cutpoint and round the number
cutpoint2_3 <- floor(2*dim(satSurvey)[1]/3)
# check the 2/3 cutpoint
cutpoint2_3
#  
trainData2 <- satSurvey[randIndex[1:cutpoint2_3],]
dim(trainData2)
head(trainData2)
testData2 <- satSurvey[randIndex[(cutpoint2_3+1):dim(satSurvey)[1]],]
dim(testData2)   # check test data set
head(trainData2)
trainData2$goodsat <- ifelse(trainData2$Satisfaction<avg, 0, 1)
testData2$goodsat <- ifelse(testData2$Satisfaction<avg, 0, 1)
trainData2 <- trainData2[,-1]
testData2 <- testData2[,-1]
testData2$goodsat <- as.factor(testData2$goodsat)
trainData2$goodsat <- as.factor(trainData2$goodsat)
trainData2$'Flight Distance' <- as.numeric(trainData2$'Flight Distance')
str(trainData2)
x <- neuralnet(goodsat ~ Age, trainData2, hidden = 2, lifesign="minimal", linear.output = FALSE, threshold = .1)
plot(x)
