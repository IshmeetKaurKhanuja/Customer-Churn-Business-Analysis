install.packages('plyr')  
install.packages('rpart.plot') 
install.packages('caret')
install.packages('gridExtra') 
install.packages('tidyverse') 
install.packages('rsample')
install.packages('e1071') 
install.packages('GGally')
install.packages('data.table')
install.packages('DT')
install.packages('readr')
install.packages('ggplot2')
install.packages('dplyr')
install.packages('tidyr')
install.packages('corrplot')
install.packages('rms')
install.packages('MASS')
install.packages('e1071')
install.packages('ROCR')
install.packages('gplots')
install.packages('pROC')
install.packages('rpart')
install.packages('randomForest')
install.packages('ggpubr')


library(plyr)  
library(rpart.plot) 
library(caret)
library(gridExtra) 
library(tidyverse) 
library(rsample)
library(e1071) 
library(GGally)
library(data.table)
library(DT)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(rms)
library(MASS)
library(e1071)
library(ROCR)
library(gplots)
library(pROC)
library(rpart)
library(randomForest)
library(ggpubr)


getwd()


churn <- read.csv("C:/Users/HZT/Desktop/BA_Customer_Churn.csv", 
                  header=T, stringsAsFactors=T)

glimpse(churn)


#Data Preprocessing
sapply(churn, function(x) sum(is.na(x)))

churn[is.na(churn$TotalCharges),]

#Checking proportion
sum(is.na(churn$TotalCharges))/nrow(churn)

#This subset is 0.16% of our data and is quite small.
#We will remove these cases in order to accomodate our further analyses.
churn_clean <- churn[complete.cases(churn), ]

#Recode SeniorCitizen variable for ease the interpretation.
churn_clean$SeniorCitizen <- as.factor(mapvalues(churn_clean$SeniorCitizen,
                                                 from=c("0","1"),
                                                 to=c("No", "Yes")))


#No phone service to No to ease the modeling.
churn_clean$MultipleLines <- as.factor(mapvalues(churn_clean$MultipleLines, 
                                                 from=c("No phone service"),
                                                 to=c("No")))

#Varous variables are dependent on OnlineService variable, therefore we'll recode the response.
#Recode from No internet service to No.
for(i in 10:15){
  churn_clean[,i] <- as.factor(mapvalues(churn_clean[,i],
                                         from= c("No internet service"), to= c("No")))
}

#No need of customerID in modeling.
churn_clean$customerID <- NULL


#DATA VISUALIZATION FOR DESCRIPTIVE ANALYSIS

#1. DEMOGRAPHIC DATA

#Gender plot
p1 <- ggplot(churn_clean, aes(x = gender)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Senior citizen plot
p2 <- ggplot(churn_clean, aes(x = SeniorCitizen)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Partner plot
p3 <- ggplot(churn_clean, aes(x = Partner)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Dependents plot
p4 <- ggplot(churn_clean, aes(x = Dependents)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Plot demographic data within a grid
grid.arrange(p1, p2, p3, p4, ncol=2)

#PAYMENT STATUS
#Contract status plot
p5 <- ggplot(churn_clean, aes(x = Contract)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Paperless billing plot
p6 <- ggplot(churn_clean, aes(x = PaperlessBilling)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Payment method plot
p7 <- ggplot(churn_clean, aes(x = PaymentMethod)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Plot contract data within a grid
grid.arrange(p5, p6, p7, ncol=1)

#QUANTITATIVE VARIABLE

#Tenure histogram
p17 <- ggplot(data = churn_clean, aes(tenure, color = Churn))+
  geom_freqpoly(binwidth = 5, size = 1)

#Monthly charges histogram
p18 <- ggplot(data = churn_clean, aes(MonthlyCharges, color = Churn))+
  geom_freqpoly(binwidth = 5, size = 1)

#Total charges histogram
p19 <- ggplot(data = churn_clean, aes(TotalCharges, color = Churn))+
  geom_freqpoly(binwidth = 200, size = 1)

#Plot quantitative data within a grid
grid.arrange(p17, p18, p19, ncol=1)

#EXAMINE THA VARIABLE OF INTEREST - CHURN
p20 <- ggplot(churn_clean, aes(x = Churn)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
p20


#CORRELATION
churn_clean %>%
  dplyr::select (TotalCharges, MonthlyCharges, tenure) %>%
  cor() %>%
  corrplot.mixed(upper = "circle", tl.col = "black", number.cex = 0.7)



set.seed(56)
split_train_test <- createDataPartition(churn_clean$Churn,p=0.7,list=FALSE)
dtrain<- churn_clean[split_train_test,]
dtest<-  churn_clean[-split_train_test,]

# Remove Total Charges from the training dataset

dtrain <- dtrain[,-19]
dtest <- dtest[,-19]


#DECISION TREE ANALYSIS
tr_fit <- rpart(Churn ~., data = dtrain, method="class")
rpart.plot(tr_fit)

#LOGISTIC REGRESSION ANALYSIS
lr_fit <- glm(Churn ~., data = dtrain,
              family=binomial(link='logit'))
summary(lr_fit)


lr_prob1 <- predict(lr_fit, dtest, type="response")
lr_pred1 <- ifelse(lr_prob1 > 0.5,"Yes","No")
table(Predicted = lr_pred1, Actual = dtest$Churn)

lr_prob2 <- predict(lr_fit, dtrain, type="response")
lr_pred2 <- ifelse(lr_prob2 > 0.5,"Yes","No")
lr_tab1 <- table(Predicted = lr_pred2, Actual = dtrain$Churn)
lr_tab2 <- table(Predicted = lr_pred1, Actual = dtest$Churn)

# Train
confusionMatrix(
  as.factor(lr_pred2),
  as.factor(dtrain$Churn),
  positive = "Yes" 
)

# Test
confusionMatrix(
  as.factor(lr_pred1),
  as.factor(dtest$Churn),
  positive = "Yes" 
)


lr_acc <- sum(diag(lr_tab2))/sum(lr_tab2)
lr_acc

#Data Visualization Based On Models
p21 <- ggplot(churn_clean, aes(x = Contract, fill = Churn)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) +
  labs(title="Churn rate by contract status")

p21


p22 <- ggplot(churn_clean, aes(x = InternetService, fill = Churn)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) +
  labs(title="Churn rate by internet service status")

p22


p23 <- ggplot(churn_clean, aes(x = tenure, fill = Churn)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Months",
       title = "Churn rate by tenure")
p23


p24 <- ggplot(churn_clean, aes(x = TotalCharges, fill = Churn)) +
  geom_histogram(binwidth = 100) +
  labs(x = "Dollars (binwidth=100)",
       title = "Churn rate by tenure")
p24


#RANDOM FOREST ANALYSIS
#Run optimal model
rf_fit2 <- randomForest(Churn ~., data = dtrain, 
                        ntree = 75, mtry = 2, 
                        importance = TRUE, proximity = TRUE)

#Display variable importance from random tree
varImpPlot(rf_fit2, sort=T, n.var = 10, 
           main = 'Top 10 important variables')




