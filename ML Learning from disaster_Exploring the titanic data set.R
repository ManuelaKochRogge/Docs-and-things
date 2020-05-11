# Machine Learning from disaster
# Applying supervised learning to the titanic data set

# Load packages
library(ggplot2) # visualization
library(ggthemes) # visualization
library(scales) # visualization
library(dplyr) # data manipulation
library(mice) # imputation
library(mlr) # benchmark algorithms
library('randomForest') # classification algorithm
library(kernlab) #classification algorithm

# Load data

train <- read.csv('train.csv', stringsAsFactors = F)
test  <- read.csv('test.csv', stringsAsFactors = F)

full  <- bind_rows(train, test) # bind training & test data
str(full)

#################################################
## Data cleaning and feature engineering

##
## Create

# The first variable **passenger name** can be split into meaningful variables 
# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Show title counts by sex
table(full$Sex, full$Title)

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)

# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])

cat(paste(nlevels(factor(full$Surname))))


# Variable Family Size
full$Fsize <- full$SibSp + full$Parch + 1

# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')

# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'


## Impute Age
# Show number of missing Age values
sum(is.na(full$Age))

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)

# Replace Age variable from the mice model.
full$Age <- mice_output$Age

# Show new number of missing Age values
sum(is.na(full$Age))


# Create the column child, and indicate whether child or adult
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

# Show counts
table(full$Child, full$Survived)

full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

# Show counts
table(full$Mother, full$Survived)

# Finish by factorizing our two new factor variables
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

# This variable appears to have a lot of missing values
full$Cabin[1:28]

# The first character is the deck. For example:
strsplit(full$Cabin[2], NULL)[[1]]

# Create a Deck variable. Get passenger deck A - F:
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))


## Complete

# exploring missing data and rectifying it through imputation. 
## Sensible value imputation
# Passengers 62 and 830 are missing Embarkment
full[c(62, 830), 'Embarked']

cat(paste(full[c(62, 830), 'Fare'][[1]][1],full[c(62, 830), 'Fare'][[1]][2],  full[c(62, 830), 'Pclass'][[1]][1], full[c(62, 830), 'Pclass'][[1]][2]))

# Get rid of our missing passenger IDs
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

# Since their fare was $80 for 1st class, they most likely embarked from 'C'
full$Embarked[c(62, 830)] <- 'C'

# Show row 1044
full[1044, ]

# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)


# Export "full" dataframe
str(full)
write.xlsx(full,"full.xlsx")

#############################################

##Exploring the data

# First, hava a look at some boxplots

# Age and Sex Distributuin
boxplot(Age~Sex,data=full, main="Age",notch=TRUE, col=(c("deeppink","darkslategrey")))

# Fare and Class Distribution
boxplot(Fare~Pclass,data=full, main="Fare",notch=TRUE, col=(c("darkorange","darkgrey", "darkviolet"))) # there seems to be an outlier
subset(full, Fare >= 500) # Outliers: 259, 680, 738, 1235
full_out<- full[-c(259,680,738,1235),  ]
boxplot(Fare~Pclass,data=full_out, main="Fare",notch=TRUE, col=(c("darkorange","darkgrey", "darkviolet")))

# Familiy Size
boxplot(full$Fsize, main="Familiy Size",notch=TRUE, col=(c("darkmagenta")))


# FNow bring in the survival variable
#relationship between Age, Sex & survival
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()

# relationship between family size & survival
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

# relationship between passenger class & survival
ggplot(full[1:891,], aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_discrete() +
  labs(x = 'PClass') +
  theme_few()

# Did children have a greater chance for survival?
ggplot(full[1:891,], aes(x = Child, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_discrete() +
  labs(x = 'Child/Adult') +
  theme_few()

##########################################################
## Data Modeling

## Split into training & test sets

# Split the data back into a train set and a test set
train <- full[1:891,]
test <- full[892:1309,]

#Select feature variables (remove chr variables etc.)
train<-train[  ,-c(1,4,8,9,11,12,14,16,20)]
test<-test[ ,-c(1,4,8,9,11,12,14,16,20)]

str(full)
str(test)


##  Benchmarking Algorithms

# Generate Pseudo-Task
# (Applying Cross Validation and Bootstrap as Resampling Methods)
# positive class = "Survived (1)"
task1 = makeClassifTask(data = train, target= "Survived", id = "CV", positive = "1") 

# Choosing algorithms for Benchmarking (making Learners)
lrn.rpart = makeLearner("classif.rpart", predict.type = "prob")
lrn.rf = makeLearner("classif.randomForest", par.vals = list(ntree = 30L), predict.type = "prob")
lrn.svm = makeLearner("classif.ksvm", predict.type = "prob")
lrn.log = makeLearner("classif.logreg", predict.type = "prob")


# Initialize 10-fold CV and a Bootstrap-Sample of 100 Iterations
rinst.cv = makeResampleInstance("CV", iters = 10L, task = task1)

## Creating Benchmark Overview
bench = benchmark(
  learners = list(lrn.rpart, lrn.rf, lrn.svm, lrn.log),
  tasks = task1,
  resamplings = rinst.cv,
  measures = list(mmce, acc, tpr, fpr, fnr, tnr,f1,auc)
)


bench
tab.bench<-getBMRAggrPerformances(bench, as.df=TRUE)
str(tab.bench)
summary(tab.bench) # LogReg with 10fold CV seems to perform best

write.xlsx(tab.bench, file = "Benchmark_learner_train.xlsx",
           sheetName = "notTuned", append= TRUE)

# Visualization with Boxplots
plotBMRBoxplots(bench)

#Plotting ROC Curve
res.log = resample(learner = lrn.log, task = task1, resampling = rd3)
res.log
pred.log<-res.log$pred

df = generateThreshVsPerfData(list(logReg = pred.log), measures = list(fpr, tpr))
plotROCCurves(df)


# Run LogReg with 10fold CV

rd3 = makeResampleDesc(method = "CV", iters = 10)
res = resample(learner = lrn.log, task = task1, resampling = rd3)
res
pred.t<-res$pred

# Analyze Performance
performance(pred.t, measures = list(mmce, acc, fpr, tpr, tnr, fnr,f1,auc))

# Find optimum threshold
d <- generateThreshVsPerfData(pred.t, measures = list(tpr, tnr, mmce))
plotThreshVsPerf(d) # Treshold between 0.3 and 0.4 seems fine, depending on the desired outcome

# Adjust threshold
# Learner anpassen
tuned.lrn.log<- setPredictThreshold(lrn.log, 0.35)
res = resample(learner = tuned.lrn.log, task = task1, resampling = rd3)
res
pred.t<-res$pred
performance(pred.t, measures = list(mmce, acc, fpr, tpr, tnr, fnr,f1,auc)) 

## Building the model using Logistic Regression

# Train the model
set.seed(754)

logreg_model<- train(tuned.lrn.log, task1)
logreg$learner.model


## Variable importance
# Get importance
importance    <- importance(logreg_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

# Make a prediction

newdata.pred = predict(logreg_model, newdata = test)
newdata.pred
titanic_pred<-data.frame(newdata.pred)

# Save the solution to a dataframe 
titanic_pred<-data.frame(newdata.pred)
test.response<-cbind(test, titanic_pred[  ,2:4] )

# Write the solution to file
write.xlsx(test.response, file = 'log_mod_test.xlsx', row.names = T)

