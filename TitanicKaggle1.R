train <- read.csv('train.csv', stringsAsFactors = F)
test  <- read.csv('test.csv', stringsAsFactors = F)


# since test doesn't have a "Survived" column, let's add it on and fill the column with 0s


test$Survived <- 0


#combining the two sets of data


combined <- rbind(train,test)


#Explore the current structure

str(combined)

#Emaberked column is missing 2 values. These blanks need to be filled.
#One wy of achieving this, is to fill the blanks with most common 
#observation

library(dplyr)

emptyFare <- combined %>% filter(PassengerId != 62 & PassengerId != 830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare

library(ggplot2)
library(scales)

ggplot(emptyFare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  xlab('Embarked') +
  ylab('Fare')

##placing the commonly appearing letter in the to the Embarked

combined$Embarked[combined$Embarked ==""] <- "C"

#test if the values have been added successiful
table(combined$Embarked)

##lets get the 'NA' from Fare column. We first find the index and then replace
##it with the median


FareIndex <- which(is.na(combined$Fare))

##The index indicates that this passenger embarked in Southampon and was in 3rd class
##We can use this information to get a more accurate median

ggplot(combined[combined$Pclass == '3' & combined$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  xlab('Fare')

##Output graph indicates passengers who embarked at the same port paid around $8
##We can find the exact amount...

FareAm <- combined %>% filter(Pclass > 2 & PassengerId != 1044)

T = median(FareAm$Fare)


##The exact amount is $8.05


combined$Fare[FareIndex] <- 8.05

colSums(combined == '')


##cleaning missing Age values... predicting the values. replacing them with predicted values
##using decision tree to grow trees
##To achieve the prediction, we need to use 'rpart' library

library(rpart)

Agefit <- rpart(Age~ + Pclass+Sex+SibSp+Parch+Fare+Embarked, 
                data = combined[!is.na(combined$Age),],
                method = 'anova')
combined$Age[is.na(combined$Age)] <- predict(Agefit,combined[is.na(combined$Age),])

##test to see if Age has been cleaned successiful

colSums(combined == '')



##Since data has been cleaned, it is now important to know which of our data
##can be can be converted to categories i.e factors
##One way of finding out is by finding the length of each variable

apply(combined,2, function(x) length(unique(x)))

##Based on the answer, we see that Survived, Pclass, Sex and Embarked should be converted 
## to factors.

combined$Survived <- as.factor(combined$Survived)
combined$Pclass <- as.factor(combined$Pclass)
combined$Sex <- as.factor(combined$Sex)
combined$Embarked <- as.factor(combined$Embarked)


##Now we have cleaned our data we can start visualizing some patterns which could 
##help in the prediction
##First, let look at the survival rate between male and female

library(ggplot2)

ggplot(combined[1:891,],aes(x = Sex,fill=factor(Survived))) +
  geom_bar() +
  ggtitle("Survival Rate Between Male and Female")+
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")  


##Let's combine Sex and Pclass and visualize their Survival rate

ggplot(combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("3D view of sex, pclass, and survival") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

##What do we see?
##If you are a female and in 1st and 2nd classes, then you have a higher survival rate
##If you are a male in 2nd and 3rd classes your surval rate is very poor


##strings are imported as factors in R. Therefore Name vector should
##converted back to "character"

combined$title <- gsub('(.*, )|(\\..*)', '', combined$Name)

combined$title[combined$title %in% c("Mme", "Mlle",'Ms')] <- "Mlle"
combined$title[combined$title %in% c('Capt', 'Don', 'Major', 'Sir','Dr','Rev')] <- 'Sir'
combined$title[combined$title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combined$title[combined$title %in% c('Col','Mr')] <- 'Mr'

combined$title <- as.factor(combined$title)

ggplot(combined[1:891,],aes(x = title,fill=factor(Survived))) +
  geom_bar() +
  ggtitle("Title V/S Survival rate")+
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived") 

### Visualize the 3-way of relationship of Title, Pclass, and Survival

ggplot(combined[1:891,], aes(x = title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("3-way relationship of Title, Pclass, and Survival") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

### Let's look at the impact of family size as factor for survival of death

combined$FamilySize <- combined$SibSp + combined$Parch + 1


combined$FSize[combined$FamilySize == 1] <- 'single'
combined$FSize[combined$FamilySize < 5 & combined$FamilySize >= 2] <- 'small'
combined$FSize[combined$FamilySize >= 5] <- 'big'


combined$FSize <- as.factor(combined$FSize)

ggplot(combined[1:891,],aes(x = FSize,fill=factor(Survived))) +
  geom_bar() +
  ggtitle("Family size vs Survival rate")+
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived") 

##Let's check Age's effect on survival rate

ggplot(combined[1:891,], aes(x = Age, fill = factor(Survived))) +
  geom_histogram(bins = 30) +
  ggtitle("Age vs Survival rate") +
  xlab("Age") +
  ylab("Total Count") +
  labs(fill = "Survived") 


##Let's perform a three-way visualization between Age vs Sex vs Survived

ggplot(combined[1:891,], aes(x = Age, fill = factor(Survived))) +
  geom_histogram(bins = 30) +
  facet_grid(~Sex) +
  ggtitle("Age vs Sex vs Survival rate") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived") 


combined$Mid_Old_Age[combined$Age >= 18] <- 'Older'
combined$Mid_Old_Age[combined$Age < 18] <- 'Young'

combined$Mid_Old_Age <- as.factor(combined$Mid_Old_Age)


##Let's perform a two-way visualization between Age vs Sex vs Survived

ggplot(combined[1:891,], aes(x = Embarked, fill = factor(Survived))) +
  geom_bar() +
  facet_grid(~Pclass) +
  ggtitle("Embarked vs Survival rate") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")


## Our model now have the following features; survival, sex,title, embarked, fsize, old_mid_age, pclass

feature = combined[1:891,c("Survived","Pclass","title","Sex","Embarked", "FSize","Mid_Old_Age")]


# We use caret to create a 70/30% split of the training data.Keeping the same propotion
# of survived vs died

set.seed(54321)

library(caret)

indexes <- createDataPartition(feature$Survived, times = 1,
                               p = 0.7,
                               list = FALSE)
                               

titanic.train <- feature[indexes,]
titanic.test <- feature[-indexes,]

prop.table(table(feature$Survived))
prop.table(table(titanic.train$Survived))
prop.table(table(titanic.test$Survived))


#----------------------------------------------------------------------------------
#                        DICISION TREE MODEL
#----------------------------------------------------------------------------------

#We can start modeling our data. First Decision Tree

set.seed(1234)

# Set up caret to perform 10-fold cross validation repeated 3 times

caret.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)


# Use caret to train a rpart decision trees using 10-fold cross 
# validation repeated 3 times and use 7 values for tuning the
# cp hyperparameter. 

rpart.cv <- train(Survived ~ ., data = titanic.train, method = "rpart",
                  trControl = caret.control, tuneLength = 30)


rpart.cv

# What is the standard deviation?
cat(paste("\nCross validation standard deviation:",  
          sd(rpart.cv$resample$Accuracy), "\n", sep = " "))


rpart.best <- rpart.cv$finalModel

##Let's use our held-up data to check the prediction power

DT_Model <- predict(rpart.cv, titanic.test)

##Checking our decision tree results... it has the accuracy of 82.71%

confusionMatrix(DT_Model,titanic.test$Survived)


test <- combined[892:1309,-2]


preds <- predict(rpart.cv, test, type = "raw")


submission <- data.frame(PassengerId = test$PassengerId,
                         Survived = preds)

# Write out a .CSV suitable for Kaggle submission
write.csv(submission, file = "MyDicisionTree.csv", row.names = FALSE)

##Our DT Model has 0.8271 accuracy which is very good.However, when we use it
##on the test data the accuracy goes down to 78.9

#-------------------------------------------------------------------------------------#
#                               RANDOM FOREST
#-------------------------------------------------------------------------------------#

set.seed(1234)

library(randomForest)

RF_Model <- randomForest(Survived~.,data = titanic.train,
                         importance = TRUE, ntree = 1000)
RF_Model


##Let's check the importance of each variable 
varImpPlot(RF_Model)


##There seem to be some redundant variables i.e, Embarked and Mid_Old_Age
RF_Model_1 <- randomForest(Survived~.,data = titanic.train[,-7],
                         importance = TRUE, ntree = 1000)
RF_Model_1


##Remove the least important variable reduces our error from 17.4% to 16.9%

##Let's now use cross validation to make sure we keep the biasness at minimal

RF.control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
RF_tuned <- train(Survived ~ ., 
                  data = titanic.train[,-7],
                  method = "rf",
                  tuneLength = 7,
                  trControl = RF.control)
RF_tuned

##mtry = 5 was used which correspond to the accuracy of 82.34%
##We used the tuned model to test our held-up titanic.test data
prediction_tuneRF <- predict(RF_tuned, titanic.test)
confusionMatrix(prediction_tuneRF,titanic.test$Survived)


##We can now use our model to predict our test dataset
Final_RF <- predict(RF_tuned, test)


##---------------------------------------------------------------------------------------#
#                       XGBOOST                                                          #
##---------------------------------------------------------------------------------------#

# We use caret to perform 10-fold cross validation repeated 3. 
# We also use a grid search for optimal model hyperparamter values.


xgb.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              search = "grid")



# We leverage a grid search of hyperparameters for xgboost. See 
# https://github.com/datasciencedojo/meetup/blob/cdb95af4d4664dcbba2a385b9151f24c0ce17607/intro_to_ml_with_r_and_caret/IntroToMachineLearning.R

tune.grid <- expand.grid(eta = c(0.05, 0.075, 0.1),
                         nrounds = c(50, 75, 100),
                         max_depth = 6:8,
                         min_child_weight = c(2.0, 2.25, 2.5),
                         colsample_bytree = c(0.3, 0.4, 0.5),
                         gamma = 0,
                         subsample = 1)

xgb.cv <- train(Survived ~ ., 
                  data = titanic.train,
                  method = "xgbTree",
                  tuneGrid = tune.grid,
                  trControl = xgb.control)
