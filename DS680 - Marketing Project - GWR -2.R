#--------------------------------------------------

# Team: Janice Frazier and Pooja Umathe


#########################
# Initialization
#########################

#install.packages("GWmodel")
library(GWmodel)

library(maptools)

#install.packages("spgwr")

library(spgwr)

library(ggplot2)
library(ggthemes)
library(magrittr)
library(tidyverse)
library(car)

###########################
# Importing the dataset
###########################

input <- read.csv("~/DS680 - Marketing Analytics/QDOBAMerged_MatrixVersionGWR2.csv",
                  stringsAsFactors = FALSE)

#########################################################
# Model Review # 1 - Linear Regression
#########################################################

#############################
# Extract specific variables
#############################

#11/6/2019 - Recalculated StoreSales
df2 <- input %>%
  as_data_frame %>%
  select('Competitors','HouseHoldSize',
         'Diversity_veryhigh', 'Diversity_upper',
         'MedianInc_veryhigh', 'MedianInc_upper', 'Pop_medium', 'Pop_veryhigh',
         'MedianAg_under42', 'TotHouse_medium', 'TotHouse_high', 'TotHouse_upper',
         'StoreSales')

#11/6/2019 - Higher GWR score
df2 <- input %>%
  as_data_frame %>%
  select('HouseHoldSize', 'Diversity_veryhigh', 'Diversity_upper',
         'MedianInc_veryhigh', 'MedianInc_upper',
         'MedianAg_under42', 'TotHouse_medium', 'TotHouse_high', 'TotHouse_upper',
         'StoreSales')
         
###############
# Correlations
###############

#install.packages("corrplot")
library(corrplot)

corr_check <- cor(df2)

corrplot(corr_check, method="circle", order="hclust",
         tl.col="black", tl.srt=45, addCoef.col = "black",
         number.digits = 1)


#########################################################
# Model Review # 1 - Linear Regression
#########################################################

lr_mod <- lm(StoreSales ~ .,data = df2)
summary(lr_mod)

# Predicting and Plotting the results 

pred1 = predict(lr_mod, newdata = df2)

#  Actual vs Prediction
plot(df2$StoreSales, pred1)

vif(lr_mod)

AIC(lr_mod)

# Train and Test Data
#install.packages("caTools")
library(caTools)

set.seed(123)
split = sample.split(df2$StoreSales, SplitRatio = 0.70)
train = subset(df2, split == TRUE)
test = subset(df2, split == FALSE)

model <- lm(StoreSales ~ .,data = train)
summary(model)

pred1 = predict(model, newdata = test)

vif(model)


# Using varImp() function

library(caret)
varImp(lr_mod, scale=FALSE)

IMPVAR <- varImp(lr_mod, scale=FALSE)

write.csv(IMPVAR, file = "IMPVAR.csv")


# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model
model <- train(StoreSales ~ .,data = df2, method="lm", preProcess="scale", trControl=control)

# estimate variable importance
importance <- varImp(model, scale=FALSE)

# plot importance
plot(importance, main = "Linear Regression - Variable Importance - NJ")

# Residual Graphs - Verifying the Regression Assumptions
par(mfrow = c(2, 2))
plot(lr_mod)
par(mfrow = c(1,1))

##
qplot(fitted(lr_mod), resid(lr_mod)) +
  geom_hline(yintercept=0) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('Standardized Residuals by Fitted Values') +
  xlab('Fitted Values') +
  ylab('Standardized Residuals')

##
qqnorm(residuals(lr_mod), ylab="Residuals")
qqline(residuals(lr_mod))
hist(lr_mod$residuals)

#set locations
locations <- cbind(input$Latitude,input$Latitude)

###############################################################
# Model Review # 2 - Geographically Weighted Regression Model
###############################################################

#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(StoreSales ~ ., data = df2,
                        coords=locations,adapt=T)

#run the gwr model
gwr.model = gwr(StoreSales ~ ., data=df2,
                adapt=GWRbandwidth, coords=locations, hatmatrix=TRUE, se.fit=TRUE)

#calculate kernel bandwidth
#GWRbandwidth <- gwr.sel(StoreSales ~ ., data = train,
#                        coords=locations,adapt=T)

#gwr.model = gwr(StoreSales ~ ., data=train,
#                adapt=GWRbandwidth, coords=locations, hatmatrix=TRUE, se.fit=TRUE)

#gwr.model = gwr(SUM_of_IP_CLAIMS_AMOUNT ~ RankBuiltEnvironment+RankAirQuality,data = input,
#                adapt=GWRbandwidth, coords=locations, hatmatrix=TRUE, se.fit=TRUE)


#print the results of the model
gwr.model

# storage model results as dataframe 
results<-as.data.frame(gwr.model$SDF) 
head(results)

input$coefHouseHoldSize<-results$HouseHoldSize
input$coefDiversity_veryhigh<-results$Diversity_veryhigh
input$coefDiversity_upper<-results$Diversity_upper
input$coefMedianInc_veryhigh<-results$MedianInc_veryhigh
input$coefMedianInc_upper<-results$MedianInc_upper

input$coefMedianAg_under42<-results$MedianAg_under42
input$coefTotHouse_medium<-results$TotHouse_medium
input$coefTotHouse_high<-results$TotHouse_high
input$coefTotHouse_upper<-results$TotHouse_upper

input$pred<-results$pred
input$localR2<-results$localR2

write.csv(input, file = "GWR_QdobaRestaurants.csv")

