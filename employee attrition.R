# Load necessary libraries
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")

#### Including the Require Pacakges

library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(reshape2)

### Data set Loading and Cleaning
# Load the dataset

employee_prediction <- read.csv('HR_Employee_Attrition-1.csv')

# Data Inspection
summary(employee_prediction)
# Check for missing values
colSums(is.na(employee_prediction)) 

# Data types and structure
str(employee_prediction)        
# Check if any NA values exist
any(is.na(employee_prediction)) 

attrition_count <- table(employee_prediction$Attrition)

print(attrition_count)

# Pie chart for Attrition
pie(attrition_count, labels = c('No', 'Yes'), explode = c(0.2, 0))

# Countplot for Attrition

ggplot(employee_prediction, aes(x = Attrition)) +
  geom_bar(fill = "blue", alpha = 0.5) +
  theme_bw() +
  labs(title = "Count of Attrition")

# Check for duplicates
duplicates <- duplicated(employee_prediction)
sum(duplicates)  # Count duplicates

### EDA

# Countplot of Age by Attrition
ggplot(employee_prediction, aes(x = Age, fill = Attrition)) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Age by Attrition")

# Print unique values for categorical columns
for (column in names(employee_prediction)) {
  if (is.character(employee_prediction[[column]])) {
    cat(column, " : ", unique(employee_prediction[[column]]), "\n")
    print(table(employee_prediction[[column]]))
    cat("_________________________________________________________\n")
  }
}

# Countplot by BusinessTravel
ggplot(employee_prediction, aes(x = BusinessTravel, fill = Attrition)) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Attrition by Business Travel") +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(0.9), vjust=-0.25)

# Countplot by Department
ggplot(employee_prediction, aes(x = Department, fill = Attrition)) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Attrition by Department") +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(0.9), vjust=-0.25)

# Countplot by Education
ggplot(employee_prediction, aes(x = Education, fill = Attrition)) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Attrition by Education") +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(0.9), vjust=-0.25)

# Countplot by Gender
ggplot(employee_prediction, aes(x = Gender, fill = Attrition)) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Attrition by Gender") +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(0.9), vjust=-0.25)

# Countplot by JobRole
ggplot(employee_prediction, aes(x = JobRole, fill = Attrition)) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Attrition by Job Role") +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(0.9), vjust=-0.25)

# Countplot by OverTime
ggplot(employee_prediction, aes(x = OverTime, fill = Attrition)) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Attrition by OverTime") +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(0.9), vjust=-0.25)

# Remove unneeded columns
employee_prediction <- employee_prediction %>%
  select(-EmployeeNumber, -StandardHours, -EmployeeCount, -Over18)

# Correlation matrix
correlation_matrix <- cor(select(employee_prediction, where(is.numeric)))
ggplot(melt(correlation_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Heatmap")

# Transform non-numeric columns into numerical columns as needed by our model
employee_prediction[] <- lapply(employee_prediction, function(x) {
  if (is.character(x)) as.numeric(as.factor(x)) else x
})

# Create a new column for Age in years
employee_prediction$Age_Years <- employee_prediction$Age

# Remove the first column called age
employee_prediction <- select(employee_prediction, -Age)

##### MODEL BUILIDING

# Split the data into independent (X) and dependent (Y) variables
X <- employee_prediction[, -1]  # Independent variables (all except first column)
Y <- employee_prediction[, 1]    # Dependent variable (Attrition)

# Split the dataset into 75% Training set and 25% Testing set
set.seed(0)
train_index <- createDataPartition(Y, p = 0.75, list = FALSE)
X_train <- X[train_index, ]
Y_train <- Y[train_index]
X_test <- X[-train_index, ]
Y_test <- Y[-train_index]

# Use Random Forest Classification algorithm

forest <- randomForest(x = X_train, y = as.factor(Y_train), ntree = 10, importance = TRUE)

# Get the accuracy on the training data
train_accuracy <- predict(forest, X_train)
train_score <- mean(train_accuracy == Y_train)
print(paste("Random Forest Training Score:", train_score))

# Show the confusion matrix and accuracy for the model on the test data
test_predictions <- predict(forest, X_test)
cm <- confusionMatrix(test_predictions, as.factor(Y_test))
print(cm)

