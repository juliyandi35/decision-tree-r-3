# Decision Tree
library(caret)
library(DescTools)
library(rpart)
library(rpart.plot)
library(readxl)

# Load data
Dataset <- read_excel('Dataset.xlsx')
Dataset <- data.frame(Dataset)
names(Dataset)
Dataset <- Dataset[,c(1,2,3,4,6,15)]

PlotMiss(Dataset)
str(Dataset)

for(i in 1:ncol(Dataset)){
  if (is.character(Dataset[, i]) == TRUE){
    Dataset[,i] <- as.factor(Dataset[,i])
  }
}
Dataset$income <- as.factor(Dataset$income)
str(Dataset)

# set sampling seed
set.seed(123)
# specify 80/20 split
data_sample <- sample(c(TRUE, FALSE), nrow(Dataset), replace = T, prob = c(0.8, 0.2))

# subset data points into train and test sets
train <- Dataset[data_sample, ]
test <- Dataset[!data_sample, ]

Dataset.rpart <- rpart(formula = income ~ ., # Y ~ all other variables in dataframe
                       data = train, # include only relevant variables
                       method = "class") # classification
Dataset.rpart

prp(x = Dataset.rpart, # rpart object
    extra = 2) # include proportion of correct predictions

base.trpreds <- predict(object = Dataset.rpart, # DT model
                        newdata = train,type = "class")  # training data

DT_train_conf <- confusionMatrix(data = base.trpreds, # predictions
                                 reference = train$income, # actual
                                 positive = "1",
                                 mode = "everything")
DT_train_conf

base.tepreds <- predict(object = Dataset.rpart, # DT model
                        newdata = test, # testing data
                        type = "class")

DT_test_conf <- confusionMatrix(data = base.tepreds, # predictions
                                reference = test$income, # actual
                                positive = "1",
                                mode = "everything")
DT_test_conf

cbind(Training = DT_train_conf$overall,
      Testing = DT_test_conf$overall)
