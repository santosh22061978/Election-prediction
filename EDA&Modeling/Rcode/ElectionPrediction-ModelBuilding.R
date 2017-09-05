# PM Group 13
# R script for model building and analysis 

library(dplyr)
library(data.table)
library(caret)
library(plyr)
library(car)
library(psych)
library(e1071)
library(ggplot2)
library(car)
library(Boruta)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(ROCR)
library(ineq)
library(caret)
library(randomForest)
library(SDMTools)
library(pROC)
library(Hmisc)

setwd( "C:/Users/user/Documents/GL-Classes/7/grp-assignment")

GE_2009.Analysis<-read.csv("Final_Clean_GE_2009_5_STATES.csv", header=T)

str(GE_2009.Analysis)

# Removing unnecessary fields from the dataset for analysis 

GE_2009.Analysis.1 <- GE_2009.Analysis[,-c(1,2)]
GE_2009.Analysis.2 <- GE_2009.Analysis.1[,-c(4,5,7,9,12,29:32,39:45,47:50)]
GE_2009.Analysis.3 <- GE_2009.Analysis.2[,-c(5:7,9:11,15,16)]

GE_2009.Analysis.3 <- plyr::rename(GE_2009.Analysis.3, c(Sex.Ratio..per.1000.="Sex.Ratio"))

GE_2009.Analysis.3$Winner <- as.factor(GE_2009.Analysis.3$Winner)
# Continuous variable analysis
#Candidate.Age,POLL.PERCENTAGE,Sex.Ratio, Average.Literacy , MuslimPercent, HinduPercent
# 

ggplot(data = GE_2009.Analysis.3, aes(x="Candidate Age",y=Candidate.Age,fill=Winner)) + 
  geom_boxplot()

ggplot(data = GE_2009.Analysis.3, aes(x="Poll Percentage",y=POLL.PERCENTAGE,fill=Winner)) + 
  geom_boxplot()

ggplot(data = GE_2009.Analysis.3, aes(x="Sex Ratio",y=Sex.Ratio,fill=Winner)) + 
  geom_boxplot()


ggplot(data = GE_2009.Analysis.3, aes(x="Average Literacy",y=Average.Literacy,fill=Winner)) + 
  geom_boxplot()


ggplot(data = GE_2009.Analysis.3, aes(x="Muslim Percent",y=MuslimPercent,fill=Winner)) + 
  geom_boxplot()


ggplot(data = GE_2009.Analysis.3, aes(x="Hindu Percent",y=HinduPercent,fill=Winner)) + 
  geom_boxplot()

# Distributions

ggplot(data=GE_2009.Analysis.3,aes(x=Candidate.Age)) + 
  geom_bar(alpha=0.75,fill="tomato",color="black") +
  ggtitle("Candidate Age Distribution") 

ggplot(data=GE_2009.Analysis.3,aes(x=POLL.PERCENTAGE)) + 
  geom_bar(alpha=0.75,fill="tomato",color="black") +
  ggtitle("Poll Percentage")

ggplot(data=GE_2009.Analysis.3,aes(x=Sex.Ratio)) + 
  geom_bar(alpha=0.75,fill="tomato",color="black") +
  ggtitle("Sex Ratio")
ggplot(data=GE_2009.Analysis.3,aes(x=Average.Literacy)) + 
  geom_bar(alpha=0.75,fill="tomato",color="black") +
  ggtitle("Average Literacy")


ggplot(data=GE_2009.Analysis.3,aes(x=MuslimPercent)) + 
  geom_bar(alpha=0.75,fill="tomato",color="black") +
  ggtitle("Muslim Percentage")
ggplot(data=GE_2009.Analysis.3,aes(x=HinduPercent)) + 
  geom_bar(alpha=0.75,fill="tomato",color="black") +
  ggtitle("Hindu Percentage")


# Categorical variable analysis
# Converting non factor categorical variables to factors
# 
GE_2009.Analysis.3$Incumbency <- as.factor(GE_2009.Analysis.3$Incumbency)
GE_2009.Analysis.3$Women <- as.factor(GE_2009.Analysis.3$Women)
GE_2009.Analysis.3$Criminal.Case <- as.factor(GE_2009.Analysis.3$Criminal.Case)
GE_2009.Analysis.3$SeriousCrime <- as.factor(GE_2009.Analysis.3$SeriousCrime)
GE_2009.Analysis.3$IsGraduate <- as.factor(GE_2009.Analysis.3$IsGraduate)
GE_2009.Analysis.3$HaveCriminalCharges <- as.factor(GE_2009.Analysis.3$HaveCriminalCharges)
GE_2009.Analysis.3$IsCrorepati <- as.factor(GE_2009.Analysis.3$IsCrorepati)
GE_2009.Analysis.3$Asset.Rank <- as.factor(GE_2009.Analysis.3$Asset.Rank)
GE_2009.Analysis.3$Rich <- as.factor(GE_2009.Analysis.3$Rich)
GE_2009.Analysis.3$National <- as.factor(GE_2009.Analysis.3$National)
GE_2009.Analysis.3$Regional <- as.factor(GE_2009.Analysis.3$Regional)
GE_2009.Analysis.3$Is_SC_ST <- as.factor(GE_2009.Analysis.3$Is_SC_ST)


# Chi Sq table
# Stacked bar plot for each variable stacked with Winner


chisq.test(table(GE_2009.Analysis.3$Incumbency,GE_2009.Analysis.3$Winner))
ggplot(GE_2009.Analysis.3,aes(Incumbency,fill=Winner))+geom_bar()+labs(title="Stacked Bar Chart",x="Incumbency",y="Count")+theme_bw()

chisq.test(table(GE_2009.Analysis.3$Women,GE_2009.Analysis.3$Winner))
ggplot(GE_2009.Analysis.3,aes(Women,fill=Winner))+geom_bar()+labs(title="Stacked Bar Chart",x="Women",y="Count")+theme_bw()


chisq.test(table(GE_2009.Analysis.3$Criminal.Case,GE_2009.Analysis.3$Winner))
ggplot(GE_2009.Analysis.3,aes(Criminal.Case,fill=Winner))+geom_bar()+labs(title="Stacked Bar Chart",x="Criminal.Case",y="Count")+theme_bw()

chisq.test(table(GE_2009.Analysis.3$SeriousCrime,GE_2009.Analysis.3$Winner))
ggplot(GE_2009.Analysis.3,aes(SeriousCrime,fill=Winner))+geom_bar()+labs(title="Stacked Bar Chart",x="SeriousCrime",y="Count")+theme_bw()

chisq.test(table(GE_2009.Analysis.3$IsGraduate,GE_2009.Analysis.3$Winner))
ggplot(GE_2009.Analysis.3,aes(IsGraduate,fill=Winner))+geom_bar()+labs(title="Stacked Bar Chart",x="IsGraduate",y="Count")+theme_bw()

chisq.test(table(GE_2009.Analysis.3$HaveCriminalCharges,GE_2009.Analysis.3$Winner))
ggplot(GE_2009.Analysis.3,aes(HaveCriminalCharges,fill=Winner))+geom_bar()+labs(title="Stacked Bar Chart",x="HaveCriminalCharges",y="Count")+theme_bw()

chisq.test(table(GE_2009.Analysis.3$IsCrorepati,GE_2009.Analysis.3$Winner))
ggplot(GE_2009.Analysis.3,aes(IsCrorepati,fill=Winner))+geom_bar()+labs(title="Stacked Bar Chart",x="IsCrorepati",y="Count")+theme_bw()

chisq.test(table(GE_2009.Analysis.3$Asset.Rank,GE_2009.Analysis.3$Winner))
ggplot(GE_2009.Analysis.3,aes(Asset.Rank,fill=Winner))+geom_bar()+labs(title="Stacked Bar Chart",x="Asset.Rank",y="Count")+theme_bw()

chisq.test(table(GE_2009.Analysis.3$Rich,GE_2009.Analysis.3$Winner))
ggplot(GE_2009.Analysis.3,aes(Rich,fill=Winner))+geom_bar()+labs(title="Stacked Bar Chart",x="Rich",y="Count")+theme_bw()

chisq.test(table(GE_2009.Analysis.3$National,GE_2009.Analysis.3$Winner))
ggplot(GE_2009.Analysis.3,aes(National,fill=Winner))+geom_bar()+labs(title="Stacked Bar Chart",x="National",y="Count")+theme_bw()

chisq.test(table(GE_2009.Analysis.3$Regional,GE_2009.Analysis.3$Winner))
ggplot(GE_2009.Analysis.3,aes(Regional,fill=Winner))+geom_bar()+labs(title="Stacked Bar Chart",x="Regional",y="Count")+theme_bw()

chisq.test(table(GE_2009.Analysis.3$Is_SC_ST,GE_2009.Analysis.3$Winner))
ggplot(GE_2009.Analysis.3,aes(Is_SC_ST,fill=Winner))+geom_bar()+labs(title="Stacked Bar Chart",x="Is_SC_ST",y="Count")+theme_bw()


# remove IS_SC and IS_ST variables

GE_2009.Analysis.3 <- GE_2009.Analysis.3[,-c(24,25)]
GE_2009.Analysis.3 <- GE_2009.Analysis.3[,-c(15)]

# Scaling the continuous variables 

# Scaling the train variables 



unscalled_list <- subset(GE_2009.Analysis.3, select = c("Candidate.Age", "POLL.PERCENTAGE","Sex.Ratio","Average.Literacy","MuslimPercent","HinduPercent"))
maxs <- apply(unscalled_list,2,max)
mins <- apply(unscalled_list,2,min)

scaled_data <- as.data.frame(scale(unscalled_list,center = mins,scale = maxs-mins))
str(scaled_data)


GE_2009.Analysis.4 <- subset(GE_2009.Analysis.3, select = -c(Candidate.Age, POLL.PERCENTAGE,Sex.Ratio,Average.Literacy,MuslimPercent,HinduPercent))
GE_2009.Analysis.4 <- cbind(GE_2009.Analysis.4,scaled_data)



## Training and Testing sample creation 
set.seed(123)
n <- nrow(GE_2009.Analysis.4)
shuffledDf <- GE_2009.Analysis.4[sample(n),]
train <- shuffledDf[1:round(n*0.7),]
test <- shuffledDf[(round(n*0.7)+1):n,]

str(train)

####feature selection

set.seed(123)
boruta.train <- Boruta(Winner~., data = train[,-c(1,2,3)], doTrace = 2)
print(boruta.train)

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

## Logistic regression modell

train.LR <- train
test.LR <- test

# Convert Rich variable into dummy variable
Rich.matrix <- model.matrix(~ Rich - 1, data = train.LR)
train.LR <- data.frame(train.LR, Rich.matrix)
train.LR <- subset(train.LR, select = -c(Rich))

Rich.matrix <- model.matrix(~ Rich - 1, data = test.LR)
test.LR <- data.frame(test.LR, Rich.matrix)
test.LR <- subset(test.LR, select = -c(Rich))

# Initially use all the variables to build the logit model

logit.all <- Winner ~ Incumbency+Women+SeriousCrime+IsGraduate+HaveCriminalCharges+IsCrorepati+National+Regional+Is_SC_ST+Candidate.Age+POLL.PERCENTAGE+Sex.Ratio+Average.Literacy+MuslimPercent+HinduPercent+RichNR+RichR1+RichR2+RichR3

model.LR.all <- glm(logit.all   , train.LR, family = binomial)
summary(model.LR.all)


misClassTable.LR.all = data.frame(Target = train.LR$Winner, Prediction = predict.glm(model.LR.all, type="response") )
misClassTable.LR.all$Classification = ifelse(misClassTable.LR.all$Prediction>0.5,1,0)
with(misClassTable.LR.all, table(Target, Classification))
confusionMatrix(misClassTable.LR.all$Target, misClassTable.LR.all$Classification)

# New model with only important and significant variables
logit.imp <- Winner ~ Incumbency+Women+IsGraduate+National+Regional+RichR1+RichR2+MuslimPercent+HinduPercent
model.LR.imp <- glm(logit.imp, train.LR, family = binomial)
summary(model.LR.imp)


misClassTable.LR.imp = data.frame(Target = train.LR$Winner, Prediction = predict.glm(model.LR.imp, type="response") )
misClassTable.LR.imp$Classification = ifelse(misClassTable.LR.imp$Prediction>0.5,1,0)
with(misClassTable.LR.imp, table(Target, Classification))
confusionMatrix(misClassTable.LR.imp$Target, misClassTable.LR.imp$Classification)

# compare both the models
library(stats)
AIC(model.LR.imp)
AIC(model.LR.all)

# 10-fold validation Logistic Regression model

library(caret)
library(scales)
train_control<- trainControl(method="cv", number=10, savePredictions = TRUE)
LR.model.10fold <- train(logit.imp, data=train.LR, trControl=train_control, method="glm")
summary(LR.model.10fold)
confusionMatrix(LR.model.10fold$pred$obs, LR.model.10fold$pred$pred)

## deciling code
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}

misClassTable.LR.imp$Target<- as.character(misClassTable.LR.imp$Target)
misClassTable.LR.imp$Target[misClassTable.LR.imp$Target == "0"] <- 0
misClassTable.LR.imp$Target[misClassTable.LR.imp$Target== "1"] <- 1

misClassTable.LR.imp$Target <- as.numeric(misClassTable.LR.imp$Target)
misClassTable.LR.imp$deciles <- decile(misClassTable$Prediction)


tmp_DT = data.table(misClassTable.LR.imp)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);


rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)


pred <- prediction(misClassTable.LR.imp$Prediction, misClassTable.LR.imp$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)


gini = ineq(misClassTable$Prediction, type="Gini")

auc
KS
gini


#### Model evaluation in testing dataset 

misClassTable.LR.imp.test = data.frame(Target = test.LR$Winner, Prediction = predict.glm(model.LR.imp, newdata= test.LR, type="response") )
misClassTable.LR.imp.test$Classification = ifelse(misClassTable.LR.imp.test$Prediction>0.5,1,0)
with(misClassTable.LR.imp.test, table(Target, Classification))
confusionMatrix(misClassTable.LR.imp.test$Target, misClassTable.LR.imp.test$Classification)

misClassTable.LR.imp.test$Target<- as.character(misClassTable.LR.imp.test$Target)
misClassTable.LR.imp.test$Target[misClassTable.LR.imp.test$Target == "0"] <- 0
misClassTable.LR.imp.test$Target[misClassTable.LR.imp.test$Target== "1"] <- 1

misClassTable.LR.imp.test$Target <- as.numeric(misClassTable.LR.imp.test$Target)
misClassTable.LR.imp.test$deciles <- decile(misClassTable.LR.imp.test$Prediction)


tmp_DT = data.table(misClassTable.LR.imp.test)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);


rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)


pred <- prediction(misClassTable.LR.imp.test$Prediction, misClassTable.LR.imp.test$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)


gini = ineq(misClassTable$Prediction, type="Gini")

auc
KS
gini



## Building the RANDOM FOREST model


train.RF <- train
test.RF <- test

train.RF <- train.RF[,-c(1,2,3)]
test.RF <- test.RF[,-c(1,2,3)]

RF <- randomForest( as.factor(Winner) ~ ., data = train.RF, 
                    ntree=500, mtry =4, nodesize = 12,
                    importance=TRUE)

print(RF)
plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest train")
RF$err.rate

tRF <- tuneRF(x = train.RF[,-c(3)], 
              y=as.factor(train.RF$Winner),
              mtryStart = 5, 
              ntreeTry=65, 
              stepFactor = 1.5, 
              improve = 0.001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = FALSE,
              nodesize = 12, 
              importance=TRUE
)

best.m <- tRF[tRF[, 2] == min(tRF[, 2]), 1]
print(tRF)
print(best.m)

set.seed(71)

RF <- randomForest(as.factor(Winner) ~ ., data = train.RF, 
                   ntree=65, mtry = best.m, nodesize = 12,
                   importance=TRUE)
print(RF)

importance(RF)
varImpPlot(RF)

train.RF$predict.class <- predict(RF,train.RF, type="class")
train.RF$predict.score <- predict(RF, train.RF, type="prob")
head(train.RF)


#Confusion Matrix
confusionMatrix(train.RF$predict.class,train.RF$Winner)


# Predicting the testing dataset 
test.RF$predict.class <- predict(RF, test.RF, type="class")
test.RF$predict.score <- predict(RF, test.RF,type="prob")
View(test.RF)


confusionMatrix(test.RF$predict.class,test.RF$Winner)
