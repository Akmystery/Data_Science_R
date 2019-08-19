#Data Exploration

marketData <- read.csv("assign2_MarketingData.csv", header = TRUE)
predData <- read.csv('assign2_MarketingPred.csv', header = TRUE)


# Basic exploration
dim(marketData)      # dimension of the dataset
names(marketData)    # labels of the columns/variables
str(marketData)      # structure of the dataset
head(marketData)     # first few (6) rows of the data
tail(marketData)     # last few (6) rows of the data
summary(marketData)  # summary statistics for all variables

#Reordering

marketData$Marital <- ordered(marketData$Marital, 
                             levels = c("single", "married", "divorced","unknown"))
marketData$Default <- ordered(marketData$Default, 
                              levels = c("yes", "no","unknown"))
marketData$Housing <- ordered(marketData$Housing, 
                              levels = c("yes", "no","unknown"))
marketData$Loan <- ordered(marketData$Loan, 
                              levels = c("yes", "no","unknown"))
marketData$PrevOutcome <- ordered(marketData$PrevOutcome, 
                              levels = c("success", "failure","nonexistent"))
marketData$Subscribed <- ordered(marketData$Subscribed, 
                           levels = c("yes", "no"))
marketData$Day <- ordered(marketData$Day, 
                                 levels = c("mon", "tue","wed","thu","fri"))


#marketData$Education <- ordered(marketData$Education, 
#levels = c("basic4y", " basic6y", "basic9y","highschool","university","professional","(Other)"))
#marketData$Month <- ordered(marketData$Month, 
#levels = c("apr", "may","jun","jul","aug","nov"))

#histogram
#Campaign, PrevDays, Previous
hist(marketData$Campaign,col='lightgreen')
hist(marketData$PrevDays,col='lightgreen')
hist(marketData$Previous,col='lightgreen')

campaign <- as.factor(marketData$Campaign)
barplot(table(campaign), col = 'lightgreen')

prevdays <- as.factor(marketData$PrevDays)
barplot(table(prevdays), col = 'lightgreen')

previous <- as.factor(marketData$Previous)
barplot(table(previous), col = 'lightgreen')

previous
#Start Plotting
plot(marketData$Subscribed)


mosaicplot(table(marketData$Job, marketData$Subscribed), main="", ylab="Subscribed", xlab="Job") #matter a lot
mosaicplot(table(marketData$Marital, marketData$Subscribed), main="", ylab="Subscribed", xlab="Marital") #does not matter
mosaicplot(table(marketData$Education, marketData$Subscribed), main="", ylab="Subscribed", xlab="Education") #no
mosaicplot(table(marketData$Default, marketData$Subscribed), main="", ylab="Subscribed", xlab="Default") #yes. a bit
mosaicplot(table(marketData$Housing, marketData$Subscribed), main="", ylab="Subscribed", xlab="Housing") #no
mosaicplot(table(marketData$Loan, marketData$Subscribed), main="", ylab="Subscribed", xlab="Loan") #no
mosaicplot(table(marketData$Contact, marketData$Subscribed), main="", ylab="Subscribed", xlab="Contact") #yes a bit
mosaicplot(table(marketData$Month, marketData$Subscribed), main="", ylab="Subscribed", xlab="Month") #yes a lot
mosaicplot(table(marketData$Day, marketData$Subscribed), main="", ylab="Subscribed", xlab="Day") #no
mosaicplot(table(marketData$PrevOutcome, marketData$Subscribed), main="", ylab="Subscribed", xlab="PrevOutcome") #yes
mosaicplot(table(marketData$Previous, marketData$Subscribed), main="", ylab="Subscribed", xlab="Previous") #yes a lot
help(boxplot)
boxplot(Age ~ Subscribed, data=marketData,xlab="Subscribed", ylab="Age", pch = 19)
boxplot(Duration~Subscribed,data=marketData, xlab="Subscribed", ylab="Duration", pch = 19) # yes
boxplot(Campaign~Subscribed,data=marketData, xlab="Subscribed", ylab="Campaign", pch = 19) #yes
boxplot(PrevDays~Subscribed,data=marketData, xlab="Subscribed", ylab="PrevDays", pch = 19) #no weired graph
boxplot(EmpVarRate~Subscribed,data=marketData, xlab="Subscribed", ylab="EmpVarRate", pch = 19) #no
boxplot(ConPriceIndex~Subscribed,data=marketData, xlab="Subscribed", ylab="ConPriceIndex", pch = 19) #no
boxplot(ConConfIndex~Subscribed,data=marketData, xlab="Subscribed", ylab="ConConfIndex", pch = 19) # a little bit
boxplot(EuriborRate~Subscribed,data=marketData, xlab="Subscribed", ylab="EuriborRate", pch = 19) # no
boxplot(NumEmp~Subscribed,data=marketData, xlab="Subscribed", ylab="NumEmp", pch = 19) # no

library(ROSE)
newData <- ROSE(Subscribed~.,data=marketData)$data #With ROSE
plot(newData$Subscribed)

summary(marketData$Subscribed)
summary(newData$Subscribed)


train <- sample(nrow(newData), 0.8*nrow(newData), replace = FALSE)
marketTrain <- newData[train,]
marketValid <- newData[-train,]
summary(marketTrain)
summary(marketValid)

library("tree")
library(randomForest)


treeFit <- tree(Subscribed ~ Age+Job+Marital+Education+Default+Housing+Loan+Contact, data = marketTrain)
plot(treeFit)
text(treeFit, pretty = FALSE)
summary(treeFit)
treeFit

predTrain <- predict(treeFit, marketTrain, type = "class")  # prediction on train set
mean(predTrain == marketTrain$Subscribed)                       # classification accuracy
predValid <- predict(treeFit, marketValid, type = "class")  # prediction on validation set
mean(predValid == marketValid$Subscribed)                       # classification accuracy

trials <- 8               # Number of trials for mtry
nTree <- 500              # Fixing the value of ntree

oobError <- double(trials)
for (mt in 1:trials) {
  rfFit <- randomForest(Subscribed ~ Age+Job+Marital+Education+Default+Housing+Loan+Contact, data = marketTrain, ntree = nTree, mtry = mt)
  oobError[mt] <- rfFit$err.rate[nTree,1]   # OOB MSE after fitting all trees
  cat("Experiment completed with mtry = ", mt, "\n")
}
plot(1:trials, oobError, pch=19, type="b",
     xlab = "Value of mtry", ylab = "Out-Of-Bag Error")



rfFit <- randomForest(Subscribed ~ Age+Job+Marital+Education+Default+Housing+Loan+Contact, data = marketTrain, 
                      ntree = 500, mtry = 3,
                      na.action = na.action(marketTrain),
                      importance = TRUE)                
rfFit

predTrain <- predict(rfFit, marketTrain, type = "class")  # prediction on train set
mean(predTrain == marketTrain$Subscribed)                       # classification accuracy
predValid <- predict(rfFit, marketValid, type = "class")  # prediction on validation set
mean(predValid == marketValid$Subscribed)  

importance(rfFit)        # importance of the variables in the model (values)
varImpPlot(rfFit)        # importance of the variables in the model (visual)

str(predData)
str(newData)

levels(predData$Education) <- levels(marketTrain$Education)
levels(predData$Default) <- levels(marketTrain$Default)
prediction <- predict(rfFit,predData,type="class")
summary(prediction)

###################
treeFit <- tree(Subscribed ~ Age+Job+Marital+Education+Default+Housing+Loan+Contact+Month+Day+Duration+Campaign+PrevDays+Previous+PrevOutcome, data = marketTrain)
plot(treeFit)
text(treeFit, pretty = FALSE)
summary(treeFit)
treeFit

predTrain <- predict(treeFit, marketTrain, type = "class")  # prediction on train set
mean(predTrain == marketTrain$Subscribed)                       # classification accuracy
predValid <- predict(treeFit, marketValid, type = "class")  # prediction on validation set
mean(predValid == marketValid$Subscribed)                       # classification accuracy

trials <- 15               # Number of trials for mtry
nTree <- 500              # Fixing the value of ntree

oobError <- double(trials)
for (mt in 1:trials) {
  rfFit <- randomForest(Subscribed ~ Age+Job+Marital+Education+Default+Housing+Loan+Contact+Month+Day+Duration+Campaign+PrevDays+Previous+PrevOutcome, data = marketTrain, ntree = nTree, mtry = mt)
  oobError[mt] <- rfFit$err.rate[nTree,1]   # OOB MSE after fitting all trees
  cat("Experiment completed with mtry = ", mt, "\n")
}
plot(1:trials, oobError, pch=19, type="b",
     xlab = "Value of mtry", ylab = "Out-Of-Bag Error")



rfFit <- randomForest(Subscribed ~ Age+Job+Marital+Education+Default+Housing+Loan+Contact+Month+Day+Duration+Campaign+PrevDays+Previous+PrevOutcome, data = marketTrain, 
                      ntree = 500, mtry = 2,
                      na.action = na.action(marketTrain),
                      importance = TRUE)                
rfFit

predTrain <- predict(rfFit, marketTrain, type = "class")  # prediction on train set
mean(predTrain == marketTrain$Subscribed)                       # classification accuracy
predValid <- predict(rfFit, marketValid, type = "class")  # prediction on validation set
mean(predValid == marketValid$Subscribed)  

importance(rfFit)        # importance of the variables in the model (values)
varImpPlot(rfFit)        # importance of the variables in the model (visual)

prediction <- predict(rfFit,predData,type="class")
summary(prediction)


#############

treeFit <- tree(Subscribed ~ ., data = marketTrain)
plot(treeFit)
text(treeFit, pretty = FALSE)
summary(treeFit)
treeFit

predTrain <- predict(treeFit, marketTrain, type = "class")  # prediction on train set
mean(predTrain == marketTrain$Subscribed)                       # classification accuracy
predValid <- predict(treeFit, marketValid, type = "class")  # prediction on validation set
mean(predValid == marketValid$Subscribed)                       # classification accuracy

trials <- 20              # Number of trials for mtry
nTree <- 500              # Fixing the value of ntree

oobError <- double(trials)
for (mt in 1:trials) {
  rfFit <- randomForest(Subscribed ~ ., data = marketTrain, ntree = nTree, mtry = mt)
  oobError[mt] <- rfFit$err.rate[nTree,1]   # OOB MSE after fitting all trees
  cat("Experiment completed with mtry = ", mt, "\n")
}
plot(1:trials, oobError, pch=19, type="b",
     xlab = "Value of mtry", ylab = "Out-Of-Bag Error")



rfFit <- randomForest(Subscribed ~ ., data = marketTrain, 
                      ntree = 500, mtry = 3,
                      na.action = na.action(marketTrain),
                      importance = TRUE)                
rfFit

predTrain <- predict(rfFit, marketTrain, type = "class")  # prediction on train set
mean(predTrain == marketTrain$Subscribed)                       # classification accuracy
predValid <- predict(rfFit, marketValid, type = "class")  # prediction on validation set
mean(predValid == marketValid$Subscribed)  

importance(rfFit)        # importance of the variables in the model (values)
varImpPlot(rfFit)        # importance of the variables in the model (visual)

prediction <- predict(rfFit,predData,type="class")
summary(prediction)
