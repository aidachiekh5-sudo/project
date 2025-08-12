# Apply one-hot encoding to 'COUNTRY' and 'INDUSTRY' in b2b_data4_updated
b2b_data4_updated <- fastDummies::dummy_cols(
  b2b_data4_updated,
  select_columns = c("COUNTRY", "INDUSTRY"),
  remove_first_dummy = FALSE,
  remove_selected_columns = FALSE
)

# Ordinal encoding of PLAN and SEGMENT in b2b_data4_updated
b2b_data4_updated$PLAN <- as.integer(factor(b2b_data4_updated$PLAN, 
                                            levels = c("Pro", "Growth", "Enterprise"), 
                                            ordered = TRUE))

b2b_data4_updated$SEGMENT <- as.integer(factor(b2b_data4_updated$SEGMENT, 
                                               levels = c("SMB", "Mid-Market", "Enterprise"), 
                                               ordered = TRUE))

#remove unecessary columns from train and test
b2b_data4_updated <- b2b_data4_updated[, !(names(b2b_data4_updated) %in% c("CUSTOMER_ID", "CUSTOMER_STATUS", "PREDICTED_CHURN","CHURN_PROBABILITY",'COUNTRY',"INDUSTRY"))]


# Convert CHURN_LABEL to factor
b2b_data4_updated$CHURN_LABEL <- as.factor(b2b_data4_updated$CHURN_LABEL)

library(caTools)

# Step 2: Split (70% train, 30% test) while preserving class ratio
set.seed(42)
split <- sample.split(b2b_data4_updated$CHURN_LABEL, SplitRatio = 0.7)

# Step 3: Create train and test sets
train_dataRF <- subset(b2b_data4_updated, split == TRUE)
test_dataRF  <- subset(b2b_data4_updated, split == FALSE)

print(prop.table(table(b2b_data4_updated$CHURN_LABEL)))
print(prop.table(table(train_dataRF$CHURN_LABEL)))
print(prop.table(table(test_dataRF$CHURN_LABEL)))

# Step 4: Separate features and labels
X_trainRF <- subset(train_dataRF, select = -CHURN_LABEL)
y_trainRF<- train_dataRF$CHURN_LABEL

X_testRF  <- subset(test_dataRF, select = -CHURN_LABEL)
y_testRF  <- test_dataRF$CHURN_LABEL


library(smotefamily)

# Convert y_trainRF to numeric vector (required by smotefamily)
y_trainRF_vec <- as.numeric(as.character(y_trainRF))

# Apply SMOTE (NOTE: convert X_trainRF to data.frame, NOT matrix)
smote_output <- SMOTE(data.frame(X_trainRF), y_trainRF_vec, K = 5, dup_size = 2)

# Combine SMOTE result
X_trainRF_balanced <- smote_output$data[, -ncol(smote_output$data)]
y_trainRF_balanced <- smote_output$data[,  ncol(smote_output$data)]

# Optional: Convert y_trainRF_balanced back to factor
y_trainRF_balanced <- as.factor(y_trainRF_balanced)

prop.table(table(y_trainRF_balanced))
table(y_trainRF_balanced)

# Clean column names: replace spaces and slashes with ".", then remove consecutive dots
clean_colnames <- function(df) {
  colnames(df) <- gsub("[ /]", ".", colnames(df))     # Replace space or slash with dot
  colnames(df) <- gsub("\\.+", ".", colnames(df))     # Replace multiple dots with a single dot
  return(df)
}

# Step 1: Combine the SMOTE-balanced training data
train_rf_data <- data.frame(X_trainRF_balanced)
train_rf_data$CHURN_LABEL <- as.factor(y_trainRF_balanced)

# Apply to all 3 datasets
train_rf_data <- clean_colnames(train_rf_data)
X_trainRF_balanced <- clean_colnames(X_trainRF_balanced)
X_testRF <- clean_colnames(X_testRF)

# Check if columns now match
setdiff(colnames(X_trainRF_balanced), colnames(X_testRF))  # should return character(0)
setdiff(colnames(X_testRF), colnames(X_trainRF_balanced))  # should return character(0)


# Step 2: Load randomForest package
library(randomForest)

# Step 3: Train the Random Forest model
set.seed(42)
rf_model <- randomForest(CHURN_LABEL ~ ., 
                         data = train_rf_data, 
                         ntree = 500,                       # Number of trees
                         mtry = floor(sqrt(ncol(X_trainRF_balanced))), # Features per split
                         importance = TRUE,                 # Get variable importance
                         na.action = na.omit)               # Handle NAs



# Step 4: Evaluate model on test data
rf_predictions <- predict(rf_model, newdata = X_testRF)

# Step 5: Confusion matrix and accuracy
library(caret)
conf_matrix <- table(Predicted = rf_predictions, Actual = y_testRF)
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", round(accuracy, 4), "\n")

# Step 6: Variable importance plot
varImpPlot(rf_model)


