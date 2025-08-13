rf_model_worst5 <- randomForest(CHURN_LABEL ~ ., 
                                data = train_rf_data, 
                                ntree = 20,                       # Number of trees
                                mtry = floor(sqrt(ncol(X_trainRF_balanced))), # Features per split
                                importance = TRUE,                 # Get variable importance
                                na.action = na.omit)               # Handle NAs

# --- Predictions ---
rf_predictions5 <- predict(rf_model_worst5, newdata = X_testRF)
rf_probabilities5 <- predict(rf_model_worst5, newdata = X_testRF, type = "prob")[, "1"]

# --- Confusion Matrix with Full Stats ---
library(caret)
cm5 <- confusionMatrix(rf_predictions5, y_testRF, positive = "1")  
print(cm5)

# --- Extract and Print Evaluation Metrics ---
kappa5 <- cm5$overall['Kappa']
precision5 <- cm5$byClass['Pos Pred Value']
recall5 <- cm5$byClass['Sensitivity']
f1_score5 <- 2 * (precision5 * recall5) / (precision5 + recall5)

cat("\n--- Evaluation Metrics (rf_model_worst5) ---\n")
cat("Precision: ", round(precision5, 4), "\n")
cat("Recall (Sensitivity): ", round(recall5, 4), "\n")
cat("F1 Score: ", round(f1_score5, 4), "\n")
cat("Kappa: ", round(kappa5, 4), "\n")

# --- Show a sample of predictions and probabilities ---
cat("\n--- Sample Predictions and Probabilities ---\n")
sample_output5 <- data.frame(
  Actual = y_testRF[1:10],
  Predicted = rf_predictions5[1:10],
  Probability = round(rf_probabilities5[1:10], 4)
)
print(sample_output5)

