energyData <- read.csv("assign1_EnergyData.csv", header = TRUE)

str(energyData)

dim(energyData)

names(energyData)

head(energyData)

tail(energyData)

summary(energyData)


#histogram

hist(energyData$Compactness,breaks = seq(0.6, 1.0, by = 0.04),col = 'lightgreen') #numeric
hist(energyData$SurfaceArea,breaks = seq(500, 850, by = 30),col = 'lightgreen') #numeric
hist(energyData$WallArea,breaks = seq(200, 450, by = 35),col = 'lightgreen') #numeric
hist(energyData$RoofArea,col = 'lightgreen') #category There is a big gap

hist(energyData$Height,col = 'lightgreen') #category
hist(energyData$Orientation,col = 'lightgreen') #category
hist(energyData$GlazingArea,col = 'lightgreen') #category
hist(energyData$GAreaDist,col = 'lightgreen') #category

hist(energyData$HeatingLoad,col = 'lightgreen') #target
hist(energyData$CoolingLoad,col = 'lightgreen') #target

roofArea <- as.factor(energyData$RoofArea)
barplot(table(roofArea), col = 'lightgreen')

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


boxplot(energyData$Height, horizontal = TRUE, col = 'steelblue')
boxplot(energyData$Orientation, horizontal = TRUE, col = 'steelblue')
boxplot(energyData$GlazingArea, horizontal = TRUE, col = 'steelblue')
boxplot(energyData$GAreaDist, horizontal = TRUE, col = 'steelblue')

boxplot(energyData$HeatingLoad, horizontal = TRUE, col = 'steelblue')
boxplot(energyData$CoolingLoad, horizontal = TRUE, col = 'steelblue')

#correlation
cor(energyData)
library(corrplot)
corrplot.mixed(cor(energyData))
pairs(energyData, pch = 19, col = "blue")
#There is a non-linear relationship between compactness and surface area

#Start Fitting for heating load
lmHeatFit1 <- lm(HeatingLoad ~ (.- CoolingLoad), data = energyData)
summary(lmHeatFit1)

lmHeatFit2 <- update(lmHeatFit1, ~ . - (RoofArea + Orientation), data = energyData)
summary(lmHeatFit2)

lmHeatFit3 <- update(lmHeatFit2, ~ . + SurfaceArea:Height, data = energyData)
summary(lmHeatFit3)

lmHeatFit4 <- update(lmHeatFit3, ~ . - WallArea, data = energyData)
summary(lmHeatFit4)


#Line is not that flat
summary(lmHeatFit4)
plot(lmHeatFit4)

cd <- cooks.distance(lmHeatFit4)

energyData.clean <- energyData[abs(cd) < 4/nrow(energyData), ] #cleaning the outlier? That's it
nrow(energyData.clean)

formula(lmHeatFit4)
lmHeatFit <- lm(formula(lmHeatFit4), data = energyData.clean)
summary(lmHeatFit)

plot(lmHeatFit)

#Start Fitting for Cooling Load
lmCoolFit1 <- lm(CoolingLoad ~ (.- HeatingLoad), data = energyData)
summary(lmCoolFit1)

lmCoolFit2 <- update(lmCoolFit1, ~ . - RoofArea, data = energyData)
summary(lmCoolFit2)

lmCoolFit3 <- update(lmCoolFit2, ~ . - (Orientation + GAreaDist), data = energyData)
summary(lmCoolFit3)

lmCoolFit4 <- update(lmCoolFit3, ~ . + WallArea:SurfaceArea, data = energyData)
summary(lmCoolFit4)

summary(lmCoolFit4)
plot(lmCoolFit4)

cd <- cooks.distance(lmCoolFit4)
energyCoolData.clean <- energyData[abs(cd) < 4/nrow(energyData), ] #cleaning the outlier? That's it
nrow(energyCoolData.clean)

formula(lmCoolFit4)
lmCoolFit <- lm(formula(lmCoolFit4), data = energyCoolData.clean)

summary(lmCoolFit)
plot(lmCoolFit)

#Prediction

newData <- read.csv("assign1_EnergyPred.csv", header = TRUE)

Heatprediction <- predict(lmHeatFit, newdata = newData)
Heatconfint <- predict(lmHeatFit, newdata = newData, interval = "confidence", level = 0.95)
Heatpredint <- predict(lmHeatFit, newdata = newData, interval = "prediction", level = 0.95)
Heatprediction
Heatconfint
Heatpredint

CoolPrediction <- predict(lmCoolFit, newdata = newData)
Coolconfint <- predict(lmCoolFit, newdata = newData, interval = "confidence", level = 0.95)
Coolpredint <- predict(lmCoolFit, newdata = newData, interval = "prediction", level = 0.95)
Coolconfint
Coolpredint

