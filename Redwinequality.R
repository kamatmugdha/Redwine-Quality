install.packages("ggplot2")
library(ggplot2)
library(moments)
library(caret)
library(tidyverse)
library(cluster)
library(factoextra)
library(corrplot)
library(gridExtra)
library(dplyr)
library(magrittr)
library(e1071)  
library(glmnet)
library(ggthemes)

redwine <- read.csv("redwine.csv")
dim(redwine)
head(redwine)
str(redwine)
summary(redwine)

#Exploratory Data Analysis
# Separate dataset into different components to use in EDA
drop <- c("quality")
Xred_train = redwine[,!(names(redwine) %in% drop)]
yred <- subset(redwine, select = c("quality"))

# Duplicated Rows
sum(ifelse(duplicated(redwine) == TRUE,1,0)) #240 duplicated records in red

# In our case, duplicated rows are acceptable, as wines with all the same physiochemical values should have the same quality rating.
# Missing or Null Values
sum(is.na(redwine))

#Dependent variable
summary(redwine$quality)

ggplot(redwine, aes(quality)) + geom_boxplot() # Visualize the distribution of quality

hist(redwine$quality) # Visualize the distribution of quality rating, 5 and 6 are the most common

#Distribution of red wine quality ratings
ggplot(redwine,aes(x=quality))+geom_bar(stat = "count",position = "dodge")+
  scale_x_continuous(breaks = seq(3,8,1))+
  ggtitle("Distribution of Red Wine Quality Ratings")+
  theme_classic()

#Independent variables
# Using Correlation
correlation_matrix = cor(Xred_train, yred)
correlation_matrix = correlation_matrix[order(correlation_matrix, decreasing = TRUE),]
head(correlation_matrix,5)

cor.table.r = cor(redwine)
corrplot(cor.table.r)

# Density vs. Alcohol
ggplot(redwine, aes(x=density, y=alcohol)) + geom_point() + geom_smooth(method=lm, se=FALSE)
# Fixed.acidity vs. citric.acid

ggplot(redwine, aes(x=fixed.acidity, y=citric.acid)) + geom_point() + 
  geom_smooth(method=lm, se=FALSE)
# The predominant fixed acids found in wines are tartaric, malic, citric, and succinic. As citric acid is a component of fixed acid, their correlation are extremely high.

# Fixed.acidity vs. density
ggplot(redwine, aes(x=fixed.acidity, y=density)) + geom_point() + 
  geom_smooth(method=lm, se=FALSE)
# Another high correlation is between density and fixed.acidity. The four prime acids found in fixed acid all have higher density than water. Consequently, the higher the acid concentration in a given compound relative to water, the more dense it becomes, all else being equal.

# Fixed.acidity vs. pH
ggplot(redwine, aes(x=fixed.acidity, y=pH)) + geom_point() + 
  geom_smooth(method=lm, se=FALSE)
# There is a negative correlation between fixed.acidity and pH. This makes sense as the higher acidity in wine will lead to a lower pH level. Fundamentally speaking, all wines lie on the acidic side of the pH spectrum, most range from 2.5 to about 4.5 pH. In our case, we have pH ranging from 3.0 to 4.0.

#Clustering the Quality of Wine

# Convert the dataset into scale to use the k-means function
Xred_km <- scale(redwine)
head(Xred_km)

# To visualize the current dataset and see if there's any cluster, we can measure the distance between each datapoint and graph them.
library(factoextra)
distance <- get_dist(Xred_km)
fviz_dist(distance, show_labels = FALSE, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Set center to 3, based on EDA
kmc3 <- kmeans(Xred_km, centers = 3, nstart = 25)
kmc3

# Let's Visualize the result
fviz_cluster(kmc3, data = Xred_km)

# Let's try different cluster settings to make sure. This block creates a set of clustering objects based on the number of clusters we designated in the center variable.
kmc2 <- kmeans(Xred_km, centers = 2, nstart = 25)
kmc4 <- kmeans(Xred_km, centers = 4, nstart = 25)
kmc5 <- kmeans(Xred_km, centers = 5, nstart = 25)

# Plots to compare
p1 <- fviz_cluster(kmc2, geom = "point", data = Xred_km) + ggtitle("k = 2")
p2 <- fviz_cluster(kmc3, geom = "point", data = Xred_km) + ggtitle("k = 3")
p3 <- fviz_cluster(kmc4, geom = "point", data = Xred_km) + ggtitle("k = 4")
p4 <- fviz_cluster(kmc5, geom = "point", data = Xred_km) + ggtitle("k = 5")

# Assign these plots to a grid
library(grid)
library(gridExtra)

grid.arrange(p1, p2, p3, p4, nrow = 2)
grid.arrange(p1,p2)

# Determine Optimal Number of Clusters
# 1. Elbow Method
set.seed(123)
fviz_nbclust(Xred_km, kmeans, method = "wss")

# 2. Silhouette Method
fviz_nbclust(Xred_km, kmeans, method = "silhouette")

# Let's proceed with 2 clusters, as it is within the range of the optimal number of clusters.
set.seed(123)
finalclusters <- kmeans(Xred_km, 2, nstart = 25)
print(finalclusters)

# Visualize the final result
fviz_cluster(finalclusters, data = Xred_km)


# K-Means Clustering
# create a X_clustering dataset to keep the integrity of the original Xred dataset.
km_final <- redwine %>%
  mutate(cluster = finalclusters$cluster)
head(km_final)

library(car)
#calculate the VIF for each predictor variable in the model
model <- lm(quality ~. , data = redwine)
vif(model)     


#Create a variable indicating if a wine is good or bad
drop <- c("quality","cluster")
redwine1 = km_final[,!(names(km_final) %in% drop)]

redwine1$good.wine<-ifelse(redwine$quality>6,1,0)

# Create a test and train dataset by splitting X4red to 70:30
redwine1$good.wine = factor(redwine1$good.wine)

#Distribution of good/bad red wines
ggplot(redwine1,aes(x=good.wine,fill=good.wine))+geom_bar(stat = "count",position = "dodge")+
  ggtitle("Distribution of Good/Bad Red Wines")+
  theme_classic()

#Alcohol and Wine Quality
ggplot(redwine1,aes(x=alcohol,fill=good.wine))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(alcohol[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(alcohol[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  xlab(label = "Alcohol Level")+
  ggtitle("Distribution of Alcohol Levels")+
  theme_classic()

#Sulphates and Wine Quality
ggplot(redwine1,aes(x=sulphates,fill=good.wine))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(sulphates[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(sulphates[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  xlab(label = "Sulphates Level")+
  ggtitle("Distribution of Sulphates Levels")+
  theme_classic()

#Training Testing
# Create a test and train dataset by splitting X4red to 70:30
set.seed(1234)
splitindex <- createDataPartition(redwine1$good.wine, p=0.7, list=FALSE, 
                                  times=1)
X4red_train <- redwine1[splitindex,]
table(X4red_train$good.wine)
X4red_test <- redwine1[-splitindex,]
table(X4red_test$good.wine)



# Oversampling the imbalanced dataset
set.seed(234)
X4red_train <- upSample(x = X4red_train[, -12],
                        y = X4red_train$good.wine)
# upSample function changes the name of our target variable from type to Class. Consequently, Class will be our target variable. The number of samples within each class is now balanced with each class having 924 samples.
table(X4red_train$Class)

names(X4red_test)[12] <- "Class"
table(X4red_test$Class)

mylogit <- glm(Class ~. , data = X4red_train, family = "binomial"(link='logit'))

summary(mylogit)


# Make prediction on the test dataset
library(InformationValue)

pred1 <- (predict(mylogit, X4red_test, type = 'response'))
head(pred1)

#y_pred = predict(mylogit, newdata = X4red_test)

optCutOff <- optimalCutoff(X4red_test$Class, pred1)[1]
optCutOff

#Confusion Matrix
confusionMatrix(X4red_test$Class, pred1, threshold = optCutOff)


# Fit the model on the train dataset using default method
# for oversampling
library(randomForest)
model2 <- randomForest(Class ~ ., data = X4red_train) ## default for classification mtry is sqrt(p)
model2 # shows out of bag error rate as 1.66% 


# Predicting the Test set results
y_pred = predict(model2, newdata = X4red_test)

#Visualizing the feature importance
#varImpPlot(model2)
#varImportance = model2$importance

importance    <- importance(model2)

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
            hjust=0, vjust=0.55, size = 4, colour = 'white') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_classic()  

#Confusion Matrix
confusionMatrix(X4red_test$Class, y_pred,positive = "1")

confusion_mtx = table(X4red_test$Class, y_pred)
confusion_mtx


