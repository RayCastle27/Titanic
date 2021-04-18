install.packages('ggplot2') 
install.packages('ggthemes')
install.packages('scales') 
install.packages('dplyr') 
install.packages('mice')
install.packages('randomForest') 

library('ggplot2') 
library('ggthemes') 
library('scales') 
library('dplyr') 
library('mice') 
library('randomForest')

test <- read.csv('C:/Kaggle/titanic/test.csv', stringsAsFactors = F)
train <- read.csv('C:/Kaggle/titanic/train.csv', stringsAsFactors = F)

predicted <- bind_rows(train, test)
glimpse(predicted)

predicted$Title <- gsub('(.*, )|(\\..*)', '', predicted$Name)

table(predicted$Sex, predicted$Title)

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

predicted$Title[predicted$Title == 'Mlle']        <- 'Miss' 
predicted$Title[predicted$Title == 'Ms']          <- 'Miss'
predicted$Title[predicted$Title == 'Mme']         <- 'Mrs' 
predicted$Title[predicted$Title %in% rare_title]  <- 'Rare Title'

table(predicted$Sex, predicted$Title)

predicted$Surname <- sapply(predicted$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])
cat(paste('We have <b>', nlevels(factor(predicted$Surname))))

predicted$Fsize <- predicted$SibSp + predicted$Parch + 1
predicted$Family <- paste(predicted$Surname, predicted$Fsize, sep='_')

ggplot(predicted[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

predicted$FsizeD[predicted$Fsize == 1] <- 'singleton'
predicted$FsizeD[predicted$Fsize < 5 & predicted$Fsize > 1] <- 'small'
predicted$FsizeD[predicted$Fsize > 4] <- 'large'

predicted$Cabin[1:28]
predicted$Deck<-factor(sapply(predicted$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

predicted[c(62, 830), 'Embarked']
predicted$Fsize <- predicted$SibSp + predicted$Parch + 1

predicted$Family <- paste(predicted$Surname, predicted$Fsize, sep='_')
ggplot(predicted[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

predicted$FsizeD[predicted$Fsize == 1] <- 'singleton'
predicted$FsizeD[predicted$Fsize < 5 & predicted$Fsize > 1] <- 'small'
predicted$FsizeD[predicted$Fsize > 4] <- 'large'

predicted$Cabin[1:28]

strsplit(predicted$Cabin[2], NULL)[[1]]

predicted$Deck<-factor(sapply(predicted$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

predicted[c(62, 830), 'Embarked']
embark_fare <- predicted %>%
  filter(PassengerId != 62 & PassengerId != 830)

ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()
predicted[1044, ]

ggplot(predicted[predicted$Pclass == '3' & predicted$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

predicted$Fare[1044] <- median(predicted[predicted$Pclass == '3' & predicted$Embarked == 'S', ]$Fare, na.rm = TRUE)

sum(is.na(predicted$Age))

factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')
set.seed(129)

mice_mod <- mice(predicted[, !names(predicted) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf')

mice_output <- complete(mice_mod)
par(mfrow=c(1,2))
hist(predicted$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

predicted$Age <- mice_output$Age
sum(is.na(predicted$Age))

ggplot(predicted[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  facet_grid(.~Sex) + 
  theme_few()

predicted$Child[predicted$Age < 18] <- 'Child'
predicted$Child[predicted$Age >= 18] <- 'Adult'

table(predicted$Child, predicted$Survived)

predicted$Mother <- 'Not Mother'
predicted$Mother[predicted$Sex == 'female' & predicted$Parch > 0 & predicted$Age > 18 & predicted$Title != 'Miss'] <- 'Mother'

table(predicted$Mother, predicted$Survived)
predicted$Child  <- factor(predicted$Child)
predicted$Mother <- factor(predicted$Mother)

train <- predicted[1:891,]
test <- predicted[892:1309,]

set.seed(754)

rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title + 
                           FsizeD + Child + Mother,
                         data = train)

plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)