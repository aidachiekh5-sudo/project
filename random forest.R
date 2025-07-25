#splitting data into training and testing datasets 
install.packages("caTools")  # only first time
library(caTools)
set.seed(123)
# sample.split takes the target variable and returns TRUE/FALSE for train/test
split <- sample.split(b2b_data4$CHURN_LABEL, SplitRatio = 0.7)
# Create train and test datasets
train <- subset(b2b_data4, split == TRUE)
test <- subset(b2b_data4, split == FALSE)
# Check proportions to confirm stratification
prop.table(table(b2b_data4$CHURN_LABEL))
prop.table(table(train$CHURN_LABEL))
prop.table(table(test$CHURN_LABEL))


#encoding categorical variables 
#Ordinal encode PLAN and SEGMENT (convert to integer)
train$PLAN <- as.integer(factor(train$PLAN, 
                                levels = c("Pro", "Growth", "Enterprise"), 
                                ordered = TRUE))

test$PLAN <- as.integer(factor(test$PLAN, 
                               levels = c("Pro", "Growth", "Enterprise"), 
                               ordered = TRUE))

train$SEGMENT<- as.integer(factor(train$SEGMENT, 
                                  levels = c("SMB", "Mid-Market", "Enterprise"), 
                                  ordered = TRUE))

test$SEGMENT <- as.integer(factor(test$SEGMENT, 
                                  levels = c("SMB", "Mid-Market", "Enterprise"), 
                                  ordered = TRUE))

#One-hot encode COUNTRY and INDUSTRY
if (!require(fastDummies)) install.packages("fastDummies")
library(fastDummies)
# For train set
train <- dummy_cols(train, select_columns = c("COUNTRY", "INDUSTRY"), remove_first_dummy = TRUE)
# For test set
test <- dummy_cols(test, select_columns = c("COUNTRY", "INDUSTRY"), remove_first_dummy = TRUE)

#remove unecessary columns from train and test
train <- train[, !(names(train) %in% c("CUSTOMER_ID", "CUSTOMER_STATUS", "PREDICTED_CHURN","CHURN_PROBABILITY"))]
test <- test[, !(names(test) %in% c("CUSTOMER_ID", "CUSTOMER_STATUS", "PREDICTED_CHURN","CHURN_PROBABILITY"))]

# Separate features and target for training set
train_x <- train[, !(names(train) %in% c("CHURN_LABEL"))]
train_y <- train$CHURN_LABEL
# Separate features and target for test set (optional, used for prediction or evaluation)
test_x <- test[, !(names(test) %in% c("CHURN_LABEL"))]
test_y <- test$CHURN_LABEL

#Model Random forest 
train_y <- as.factor(train_y)
test_y <- as.factor(test_y)
library(randomForest)
library(caret)
model_rf <- randomForest(x = train_x, y = train_y, ntree = 100, importance = TRUE)
print(model_rf)
varImpPlot(model_rf)
pred_classes <- predict(model_rf, test_x)
conf_matrix <- confusionMatrix(pred_classes, test_y)
print(conf_matrix)

