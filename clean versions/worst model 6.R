library(randomForest)
rf_model_worst6 <- randomForest(CHURN_LABEL ~ ., 
                                data = train_rf_data, 
                                ntree = 500,                       # Number of trees
                                mtry = floor(sqrt(ncol(X_trainRF_balanced))), # Features per split
                                importance = TRUE,                 # Get variable importance
                                na.action = na.omit)               # Handle NAs

#metrics
rf_predictions6 <- predict(rf_model_worst6, newdata = X_testRF)
rf_probabilities6 <- predict(rf_model_worst6, newdata = X_testRF, type = "prob")[, "1"]
# --- Confusion Matrix with Full Stats ---
library(caret)
cm6 <- confusionMatrix(rf_predictions6, y_testRF, positive = "1")  
print(cm6)

# --- Extract and Print Evaluation Metrics ---
kappa6 <- cm6$overall['Kappa']
precision6 <- cm6$byClass['Pos Pred Value']
recall6 <- cm6$byClass['Sensitivity']
f1_score6 <- 2 * (precision6 * recall6) / (precision6 + recall6)

cat("\n--- Evaluation Metrics (rf_model_worst6) ---\n")
cat("Precision: ", round(precision6, 4), "\n")
cat("Recall (Sensitivity): ", round(recall6, 4), "\n")
cat("F1 Score: ", round(f1_score6, 4), "\n")
cat("Kappa: ", round(kappa6, 4), "\n")

# --- Show a sample of predictions and probabilities ---
cat("\n--- Sample Predictions and Probabilities ---\n")
sample_output6 <- data.frame(
  Actual = y_testRF[1:10],
  Predicted = rf_predictions6[1:10],
  Probability = round(rf_probabilities6[1:10], 4)
)
print(sample_output6)