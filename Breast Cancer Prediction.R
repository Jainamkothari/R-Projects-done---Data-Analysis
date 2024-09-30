if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(funModeling)) install.packages("funModeling", repos = "http://cran.us.r-project.org")

library(funModeling)
library(corrplot)
library(tidyverse)

#################################################
#  Breast Cancer Project Code 
################################################

#### Data Loading ####
# Wisconsin Breast Cancer Diagnostic Dataset
# Loading the csv data file

data <- read.csv("data.csv")
data$diagnosis <- as.factor(data$diagnosis)

# the 33 column is not right on looking at file

data[,33] <- NULL

# General Data Info
str(data)
summary(data)

## We have 569 observations with 32 variables. 
head(data)

# Check for missing values
map_int(data, function(.x) sum(is.na(.x)))
## no missing values

# Check proportion of data
prop.table(table(data$diagnosis))

# Distribution of the  Diagnosis Column
options(repr.plot.width=4, repr.plot.height=4)
ggplot(data, aes(x=diagnosis))+geom_bar(fill="blue",alpha=0.5)+theme_bw()+labs(title="Distribution of Diagnosis")

# Plotting Numerical Data
plot_num(data %>% select(-id), bins=10) 

# Correlation plot
correlationMatrix <- cor(data[,3:ncol(data)])
corrplot(correlationMatrix, order = "hclust", tl.cex = 1, addrect = 8)

# Find attributes that are highly corrected (ideally >0.90)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.9)
# print indexes of highly correlated attributes
print(highlyCorrelated)

# Remove correlated variables
data2 <- data %>%select(-highlyCorrelated)
# Number of columns after removing correlated variables
ncol(data2)
### Model creation

# Creation of the partition 80% and 20%
set.seed(1815) #provare 1234
data3 <- cbind (diagnosis=data$diagnosis, data2)
data_sampling_index <- createDataPartition(data$diagnosis, times=1, p=0.8, list = FALSE)
train_data <- data3[data_sampling_index, ]
test_data <- data3[-data_sampling_index, ]

fitControl <- trainControl(method="cv",    #Control the computational nuances of the train function
                           number = 15,    #Either the number of folds or number of re sampling iterations
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

### K Nearest Neighbor (KNN) Model
# Creation of K Nearest Neighbor (KNN) Model
model_knn <- train(diagnosis~.,
                   train_data,
                   method="knn",
                   metric="ROC",
                   preProcess = c('center', 'scale'),
                   tuneLength=, #The tuneLength parameter tells the algorithm to try different default values for the main parameter
                   #In this case we used 10 default values
                   trControl=fitControl)
# Prediction
prediction_knn <- predict(model_knn, test_data)

# Confusion matrix        
confusionmatrix_knn <- confusionMatrix(prediction_knn, test_data$diagnosis, positive = "M")
confusionmatrix_knn

# Plot of top important variables
plot(varImp(model_knn), top=10, main="Top variables - KNN")


