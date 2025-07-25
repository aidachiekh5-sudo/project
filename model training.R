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

#Decision Tree

# Check structure
str(train)
str(test)
# Remove original categorical columns to avoid duplication
train_nocat <- train[, !(names(train) %in% c("COUNTRY", "INDUSTRY"))]
test_nocat <- test[, !(names(test) %in% c("COUNTRY", "INDUSTRY"))]

sum(is.na(train_nocat))  # Should be 0
sum(is.na(test_nocat))   # Should be 0

intersect(c("PLAN", "SEGMENT", "COUNTRY", "INDUSTRY"), names(train_nocat))  # Should be removed if one-hot encoded

str(train_nocat)
str(test_nocat)

# Match levels of SEGMENT and PLAN in test_nocat to those in train_nocat
test_nocat$SEGMENT <- factor(test_nocat$SEGMENT, levels = levels(train_nocat$SEGMENT))
test_nocat$PLAN <- factor(test_nocat$PLAN, levels = levels(train_nocat$PLAN))
str(train_nocat$SEGMENT)
str(test_nocat$SEGMENT)
str(train_nocat$PLAN)
str(test_nocat$PLAN)

# levels must align
levels(train_nocat$CHURN_LABEL)
levels(test_nocat$CHURN_LABEL)

str(train_nocat$CHURN_LABEL)  # should be Factor with levels "0", "1"
str(test_nocat$CHURN_LABEL)  # should be Factor with levels "0", "1"

#difference between the column names
setdiff(names(train_nocat), names(test_nocat))  # Should only return CHURN_LABEL

#training the decision tree model
# Load required libraries
library(rpart)
library(rpart.plot)
library(caret)

# 1. Make sure target variable is a factor
train_nocat$CHURN_LABEL <- as.factor(train_nocat$CHURN_LABEL)

# 2. Train the decision tree model
model_dt <- rpart(CHURN_LABEL ~ ., 
                  data = train_nocat, 
                  method = "class", 
                  control = rpart.control(cp = 0.01))  # Adjust cp if needed

# 3. Visualize the decision tree
rpart.plot(model_dt, type = 2, extra = 104, fallen.leaves = TRUE, cex = 0.7)

# Make sure levels match
test_nocat$CHURN_LABEL <- factor(test_nocat$CHURN_LABEL, levels = levels(train_nocat$CHURN_LABEL))

# Remove the target column from test features for prediction
test_features <- test_nocat[, setdiff(names(test_nocat), "CHURN_LABEL")]

# Predict on test data (features only)
pred_test <- predict(model_dt, test_features, type = "class")

# Evaluate predictions using the true target labels
confusionMatrix(pred_test, test_nocat$CHURN_LABEL)


