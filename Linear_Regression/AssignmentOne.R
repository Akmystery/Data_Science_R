energyData <- read.csv("assign1_EnergyData.csv", header = TRUE)

str(energyData)

dim(energyData)

names(energyData)

head(energyData)

tail(energyData)

summary(energyData)


#histogram

hist(energyData$Compactness,col = 'lightgreen') #numeric
hist(energyData$SurfaceArea,col = 'lightgreen') #numeric
hist(energyData$WallArea,col = 'lightgreen') #numeric
hist(energyData$RoofArea,col = 'lightgreen') #numeric There is a big gap

hist(energyData$Height,col = 'lightgreen') #category
hist(energyData$Orientation,col = 'lightgreen') #category
hist(energyData$GlazingArea,col = 'lightgreen') #category
hist(energyData$GAreaDist,col = 'lightgreen') #category

hist(energyData$HeatingLoad,col = 'lightgreen') #target
hist(energyData$CoolingLoad,col = 'lightgreen') #target

orientation <- as.factor(energyData$Orientation)
barplot(table(orientation), col = 'lightgreen')

gArea <- as.factor(energyData$GlazingArea)
barplot(table(gArea), col = 'lightgreen')

gAreaDist <- as.factor(energyData$GAreaDist)
barplot(table(gAreaDist), col = 'lightgreen')

height <- as.factor(energyData$Height)
barplot(table(height), col = 'lightgreen')

#boxplot
boxplot(energyData$Compactness, horizontal = TRUE, col = 'steelblue')
boxplot(energyData$SurfaceArea, horizontal = TRUE, col = 'steelblue')
boxplot(energyData$WallArea, horizontal = TRUE, col = 'steelblue')
boxplot(energyData$RoofArea, horizontal = TRUE, col = 'steelblue')
boxplot(energyData$HeatingLoad, horizontal = TRUE, col = 'steelblue')
boxplot(energyData$CoolingLoad, horizontal = TRUE, col = 'steelblue')

boxplot(energyData$Height, horizontal = TRUE, col = 'steelblue')
boxplot(energyData$Orientation, horizontal = TRUE, col = 'steelblue')
boxplot(energyData$GlazingArea, horizontal = TRUE, col = 'steelblue')
boxplot(energyData$GAreaDist, horizontal = TRUE, col = 'steelblue')

#correlation
cor(energyData)
library(corrplot)
corrplot.mixed(cor(energyData))

pairs(energyData, pch = 19, col = "blue")

#There is a non-linear relationship between compactness and surface area

#Start Fitting for heating load
lmHeatFit1 <- lm(HeatingLoad ~ (.- CoolingLoad), data = energyData)
summary(lmHeatFit1)

lmHeatFit2 <- update(lmHeatFit1, ~ . - RoofArea, data = energyData)
summary(lmHeatFit2)

lmHeatFit3 <- update(lmHeatFit2, ~ . - Orientation, data = energyData)
summary(lmHeatFit3)

lmHeatFit4 <- update(lmHeatFit3, ~ . + SurfaceArea:Compactness, data = energyData)
summary(lmHeatFit4)

lmHeatFit5 <- update(lmHeatFit4, ~ . + I(Compactness^2), data = energyData)
summary(lmHeatFit5)

lmHeatFit6 <- update(lmHeatFit5, ~ . - WallArea, data = energyData)
summary(lmHeatFit6)
#Line is not that flat
summary(lmHeatFit5)
plot(lmHeatFit5)

cd <- cooks.distance(lmHeatFit5)
energyData.clean <- energyData[abs(cd) < 4/nrow(energyData), ] #cleaning the outlier? That's it
nrow(energyData.clean)

formula(lmHeatFit5)
lmHeatFit <- lm(formula(lmHeatFit5), data = energyData.clean)

summary(lmHeatFit)
plot(lmHeatFit)

#Start Fitting for Cooling Load
lmCoolFit1 <- lm(CoolingLoad ~ (.- HeatingLoad), data = energyData)
summary(lmCoolFit1)

lmCoolFit2 <- update(lmCoolFit1, ~ . - RoofArea, data = energyData)
summary(lmCoolFit2)

lmCoolFit3 <- update(lmCoolFit2, ~ . - (Orientation + GAreaDist), data = energyData)
summary(lmCoolFit3)

lmCoolFit4 <- update(lmCoolFit3, ~ . + Compactness:SurfaceArea, data = energyData)
summary(lmCoolFit4)

lmCoolFit5 <- update(lmCoolFit4, ~ . + I(SurfaceArea^2), data = energyData)
summary(lmCoolFit5)

lmCoolFit6 <- update(lmCoolFit5, ~ . + I(Compactness^3), data = energyData)
summary(lmCoolFit6)

summary(lmCoolFit6)
plot(lmCoolFit6)

cd <- cooks.distance(lmCoolFit6)
energyCoolData.clean <- energyData[abs(cd) < 4/nrow(energyData), ] #cleaning the outlier? That's it
nrow(energyData.clean)

formula(lmCoolFit6)
lmCoolFit <- lm(formula(lmCoolFit6), data = energyCoolData.clean)

summary(lmCoolFit)
plot(lmCoolFit)

#Prediction

newData <- read.csv("assign1_EnergyPred.csv", header = TRUE)

Heatprediction <- predict(lmHeatFit, newdata = newData)
Heatprediction

CoolPrediction <- predict(lmCoolFit, newdata = newData)

