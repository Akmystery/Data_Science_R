retailData <- read.csv("assign3_RetailData.csv", header = TRUE)

retailData$CustomerID <- as.factor(retailData$CustomerID)

retailData <- na.omit(retailData)

retailData <- retailData[!(retailData$Quantity <= 0),]

retailData <- retailData[!(retailData$UnitPrice <= 0),] #397884

retailData <- retailData[!(retailData$Quantity > 12),] #331960

summary(retailData)

Nazi <- subset(retailData, Country == "Germany")

Nazimatrix <- xtabs(Quantity ~ CustomerID + StockCode, data = Nazi)

Nazimatrix <- Nazimatrix[rowSums(Nazimatrix) > 0,]

Nazimatrix <- Nazimatrix[,colSums(Nazimatrix) > 0]

library(Matrix)
Nazimatrix <- Matrix(Nazimatrix, sparse = TRUE) 


library(mclust)
emFit <- Mclust(Nazimatrix)
summary(emFit)


hiercFit <- hclust(dist(Nazimatrix, method = "euclidean"),    # choice of distance
                   method="ward.D") # choice of linkage
hiercFit
plot(hiercFit, main="Minimum Variance", xlab="", ylab="", sub="", cex =.5)

cutree(hiercFit, k = 2)
clustLabels <- cutree(hiercFit, k = 2)

hiercClust1 = Nazi [clustLabels == 1,]
hiercClust2 = Nazi[clustLabels == 2,]


summary(hiercClust1,10)
summary(hiercClust2,10)

France <- subset(retailData, Country == "France")

summary(France)

Francematrix <- xtabs(Quantity ~ CustomerID + StockCode, data = France)
#Francematrix <- xtabs(Quantity*UnitPrice ~ CustomerID + StockCode, data = France)

Francematrix <- Francematrix[rowSums(Francematrix)>0,]

Francematrix <- Francematrix[,colSums(Francematrix) > 0]

library(Matrix)
Francematrix <- Matrix(Francematrix, sparse = TRUE) 
str(Francematrix)

kMin <- 1
kMax <- 20
withinSS <- double(kMax - kMin + 1)
betweenSS <- double(kMax - kMin + 1)

for (K in kMin:kMax) {
  kMeansFit <- kmeans(Francematrix, centers = K, nstart = 20)
  withinSS[K] <- sum(kMeansFit$withinss)
  betweenSS[K] <- kMeansFit$betweenss
}

plot(kMin:kMax, withinSS, pch=19, type="b", col="red",
     xlab = "Value of K", ylab = "Sum of Squares (Within and Between)")
points(kMin:kMax, betweenSS, pch=19, type="b", col="green")

K <- 3
kMeansFit <- kmeans(Francematrix, centers = K, nstart = 20)
kMeansFit$withinss
clustLabels <- kMeansFit$cluster

kMeansClust1 = France[clustLabels == 1,]
kMeansClust2 = France[clustLabels == 2,]
kMeansClust3 = France[clustLabels == 3,]

summary(kMeansClust1)
summary(kMeansClust2)
summary(kMeansClust3)

retailmatrix <- xtabs(Quantity ~ CustomerID + StockCode, data = retailData, sparse = TRUE)

library(mclust)
emFit <- Mclust(retailData)
clustLabels <- emFit$classification
summary(emFit)

emClust1 = retailData[clustLabels == 1,]
emClust2 = retailData[clustLabels == 2,]
emClust3 = retailData[clustLabels == 3,]
emClust4 = retailData[clustLabels == 4,]
emClust5 = retailData[clustLabels == 5,]
emClust6 = retailData[clustLabels == 6,]
emClust7 = retailData[clustLabels == 7,]
emClust8 = retailData[clustLabels == 8,]
emClust9 = retailData[clustLabels == 9,]

summary(emClust1,10)
summary(emClust2,10)
summary(emClust3,30) 
summary(emClust4,10)
summary(emClust5,10)
summary(emClust6,10)
summary(emClust7,10)
summary(emClust8,10)
summary(emClust9,10)
