
library(dplyr)
library(Amelia)
library(ggplot2)
library(caret)

#importing the data set
churn = read.csv("Telco-Customer-Churn.csv")

#checking the head of the data set
head(churn)

# checking the str of the data set
str(churn)

names(churn)

# Data Pre-processing 
glimpse(churn)
sapply(churn, function(x) sum(is.na(x)))

missmap(obj = churn, legend = FALSE, col = c("red", 'black'))

churn = churn[complete.cases(churn),]

#let's recheck the missing values
sapply(churn, function(x) sum(is.na(x)))

#### Exploration Data Analysis

# let's take a copy of our data set
churn_df = data.frame(churn)



churn_df = churn_df %>% mutate_if(is.character, as.factor)
churn_df$SeniorCitizen = as.factor(churn_df$SeniorCitizen)


glimpse(churn_df)
#Statring from gender tendacy to churn

ggplot(data = churn_df, aes(x = gender)) +
  geom_bar(aes(fill = (Churn), alpha = 0.5,  position = "dodge"))


churn_df %>% 
  group_by(gender,Churn) %>% 
  summarise(n=n())

#Senior Citizens
ggplot(data  = churn_df, aes(x = SeniorCitizen)) +
  geom_bar(aes(fill = Churn, alpha = 0.5), position = "dodge")

#churn_df %>% group_by(SeniorCitizen, Churn) %>% summarise(n = n(), average = mean(TotalCharges), maximum = max(TotalCharges))
churn_df %>% group_by(SeniorCitizen, Churn) %>% summarise(n = n())

churn_df %>% group_by(SeniorCitizen, Churn) %>% summarise(n = n()) %>% mutate(Percentage = n / sum(n))

#Partener
ggplot(data = churn_df, aes(x = Partner)) +
  geom_bar(aes(fill = Churn), alpha = 0.5, position = "dodge")
churn_df %>% group_by(Partner, Churn) %>% summarise(n = n())
churn_df %>% group_by(Partner, Churn) %>% summarise(n = n()) %>% mutate(percent = n/sum(n))

# dependent
ggplot(data = churn_df, aes(x = Dependents)) +
  geom_bar(aes(fill = Churn), alpha = 0.5, position = "dodge")
churn_df %>% group_by(Dependents, Churn) %>% summarise(n = n()) %>% mutate(percent = n/sum(n))
# Senior Citizens 
ggplot(churn_df, aes(x = SeniorCitizen, y = TotalCharges)) + 
geom_boxplot(aes(fill=Churn)) +
scale_y_continuous(breaks = seq(min(churn_df$TotalCharges), max(churn_df$TotalCharges), by = 400))
     
# Gender 
ggplot(churn_df, aes(x = gender, y = TotalCharges)) + 
  geom_boxplot(aes(fill=Churn)) +
  scale_y_continuous(breaks = seq(min(churn_df$TotalCharges), max(churn_df$TotalCharges), by = 400))

# Partner 
ggplot(churn_df, aes(x = Partner, y = TotalCharges)) + 
geom_boxplot(aes(fill=Churn)) +
scale_y_continuous(breaks = seq(min(churn_df$TotalCharges), max(churn_df$TotalCharges), by = 400))
     
# Dependents
ggplot(churn_df, aes(x = Dependents, y = TotalCharges)) + 
geom_boxplot(aes(fill=Churn)) +
scale_y_continuous(breaks = seq(min(churn_df$TotalCharges), max(churn_df$TotalCharges), by = 400))


# Total charges and tenure of people without a partner
churn_df %>% 
  select(Partner, Churn, TotalCharges, tenure) %>% 
  filter(Partner == "No", Churn == "Yes") %>% 
  summarise(n = n(),
            total = sum(TotalCharges),
            avg_tenure = sum(tenure)/n)

###Model building using Logistic Regression

head(churn_df)

# removing customerID
churn_df = select(churn_df, -customerID)

head(churn_df)


set.seed(1234)
inTrain = createDataPartition(y = churn_df$Churn, list = FALSE, p = 0.75)

train = churn_df[inTrain,]
test = churn_df[-inTrain,]

dim(train)

dim(test)

# creating the model at first
classifier = glm(formula =train$Churn ~., family = binomial(link = "logit"), data = train)
summary(classifier)

#making prediction
y_pred = predict(object = classifier, newdata = test, type = "response")

head(y_pred)

y_pred = ifelse(y_pred > 0.5, "Yes", "No")

y_pred = as.factor(y_pred)
head(y_pred)
confusionMatrix(y_pred, test$Churn, positive = "Yes")

plot(varImp(model_knn), top=10, main="Top variables - Linear Regression")

