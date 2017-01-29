#follows a few kaggle tutorials i merged together, if memory serves
#document your code, kids

library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

#import the datasets
train <- read.csv(file="/Users/kaelenmedeiros/R directory/titanic_kaggle/train.csv")
test  <- read.csv("/Users/kaelenmedeiros/R directory/titanic_kaggle/test.csv")

#combine the two to examine the data set
full_titanic <-bind_rows(train,test)
str(full_titanic)

##feature engineering

#titles/surnames
# Grab title from passenger names
#is based off assumption that a title is likely to influence your class
full_titanic$Title <- gsub('(.*, )|(\\..*)', '', full_titanic$Name)
# Show title counts by sex
table(full_titanic$Sex, full_titanic$Title)
#makes sense to combine low counts into 'rare titles'
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
# Also reassign mlle, ms, and mme accordingly (are same as the ones being assigned to)
full_titanic$Title[full_titanic$Title == 'Mlle']        <- 'Miss' 
full_titanic$Title[full_titanic$Title == 'Ms']          <- 'Miss'
full_titanic$Title[full_titanic$Title == 'Mme']         <- 'Mrs' 
full_titanic$Title[full_titanic$Title %in% rare_title]  <- 'Rare Title'
# Show title counts by sex again
table(full_titanic$Sex, full_titanic$Title)

#grab surname from passenger name
full_titanic$Surname <- sapply(full_titanic$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])
nlevels(factor(full_titanic$Surname)) #we see 875 unique surnames from those given

#family size
# Create a family size variable including the passenger themselves
full_titanic$Fsize <- full_titanic$SibSp + full_titanic$Parch + 1
# Create a family variable 
full_titanic$Family <- paste(full_titanic$Surname, full_titanic$Fsize, sep='_')
#so now let's plot to see if there's a relationship
#between survival and family size
ggplot(full_titanic[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few() #note that it seems you had both a better chance of surviving and not based on family size
# creating a 'discretized family size variable'
full_titanic$FsizeD[full_titanic$Fsize == 1] <- 'singleton'
full_titanic$FsizeD[full_titanic$Fsize < 5 & full_titanic$Fsize > 1] <- 'small'
full_titanic$FsizeD[full_titanic$Fsize > 4] <- 'large'
# Show family size by survival using a mosaic plot
mosaicplot(table(full_titanic$FsizeD, full_titanic$Survived), main='Family Size by Survival', shade=TRUE)

#deck
full_titanic$Cabin[1:28]
strsplit(full_titanic$Cabin[2], NULL)[[1]] #many are missing
# Create a Deck variable. Get passenger deck A - F:
full_titanic$Deck<-factor(sapply(full_titanic$Cabin, function(x) strsplit(x, NULL)[[1]][1]))


##missingness and imputation

#sensible value imputation
#find embarkments
# Passengers 62 and 830 are missing Embarkment
full_titanic[c(62, 830), 'Embarked']
# Get rid of our missing passenger IDs
embark_fare <- full_titanic %>%
  filter(PassengerId != 62 & PassengerId != 830)
# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()
# Since their fare was $80 for 1st class, they most likely embarked from 'C'
full_titanic$Embarked[c(62, 830)] <- 'C'
#find a fare
# Show row 1044
full_titanic[1044, ]
#visualize those who were also third class that embarked in that place
ggplot(full_titanic[full_titanic$Pclass == '3' & full_titanic$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()
# Replace missing fare value with median fare for class/embarkment
full_titanic$Fare[1044] <- median(full_titanic[full_titanic$Pclass == '3' 
        & full_titanic$Embarked == 'S', ]$Fare, na.rm = TRUE)

#predictive value imputation
#age
# Show number of missing Age values
sum(is.na(full_titanic$Age)) #263 missing
# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')
full_titanic[factor_vars] <- lapply(full_titanic[factor_vars], function(x) as.factor(x))
# Set a random seed
set.seed(129)
# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full_titanic[, !names(full_titanic) %in% 
      c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 
# Save the complete output 
mice_output <- complete(mice_mod)
# Plot age distributions to ensure nothing has gone wrong (looks good!!)
par(mfrow=c(1,2))
hist(full_titanic$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))
# Replace Age variable from the mice model.
full_titanic$Age <- mice_output$Age
# Show new number of missing Age values
sum(is.na(full_titanic$Age))


##more feature engineering

# First we'll look at the relationship between age & survival
ggplot(full_titanic[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()
# Create the column child, and indicate whether child or adult
full_titanic$Child[full_titanic$Age < 18] <- 'Child'
full_titanic$Child[full_titanic$Age >= 18] <- 'Adult'
# Show counts
table(full_titanic$Child, full_titanic$Survived)
# Adding Mother variable
full_titanic$Mother <- 'Not Mother'
full_titanic$Mother[full_titanic$Sex == 'female' & full_titanic$Parch > 0 & full_titanic$Age > 18 
                    & full_titanic$Title != 'Miss'] <- 'Mother'
# Show counts
table(full_titanic$Mother, full_titanic$Survived)
# Finish by factorizing our two new factor variables
full_titanic$Child  <- factor(full_titanic$Child)
full_titanic$Mother <- factor(full_titanic$Mother)

#checking on missing in all variables
md.pattern(full_titanic)


##prediction

# Split the data back into a train set and a test set
train_titanic <- full_titanic[1:891,]
test_titanic <- full_titanic[892:1309,]

#build the model
# Set a random seed
set.seed(754)
# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title + 
                           FsizeD + Child + Mother,
                         data = train_titanic)
# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
# Get importance
importance    <- importance(rf_model)
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
# Predict using the test set
prediction <- predict(rf_model, test_titanic)
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test_titanic$PassengerId, Survived = prediction)
# Write the solution to file
write.csv(solution, file = '/Users/kaelenmedeiros/R directory/titanic_kaggle/rf_mod_Solution.csv', row.names = F)
