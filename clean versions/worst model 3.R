rf_model_worst3 <- randomForest(CHURN_LABEL ~ ., 
                                data = train_rf_data, 
                                ntree = 20,                        # Too few trees: weak ensemble
                                mtry = ncol(X_trainRF_balanced),  # Use all features at each split (less randomness)
                                importance = FALSE,                # No variable importance tracking
                                na.action = na.omit)               # Remove NAs safely

# --- Predictions ---
rf_predictions3 <- predict(rf_model_worst3, newdata = X_testRF)
rf_probabilities3 <- predict(rf_model_worst3, newdata = X_testRF, type = "prob")[, "1"]

# --- Confusion Matrix with Full Stats ---
library(caret)
cm3 <- confusionMatrix(rf_predictions3, y_testRF, positive = "1")  
print(cm3)

# --- Extract and Print Evaluation Metrics ---
kappa3 <- cm3$overall['Kappa']
precision3 <- cm3$byClass['Pos Pred Value']
recall3 <- cm3$byClass['Sensitivity']
f1_score3 <- 2 * (precision3 * recall3) / (precision3 + recall3)

cat("\n--- Evaluation Metrics (rf_model_worst3) ---\n")
cat("Precision: ", round(precision3, 4), "\n")
cat("Recall (Sensitivity): ", round(recall3, 4), "\n")
cat("F1 Score: ", round(f1_score3, 4), "\n")
cat("Kappa: ", round(kappa3, 4), "\n")

# --- Show a sample of predictions and probabilities ---
cat("\n--- Sample Predictions and Probabilities ---\n")
sample_output3 <- data.frame(
  Actual = y_testRF[1:10],
  Predicted = rf_predictions3[1:10],
  Probability = round(rf_probabilities3[1:10], 4)
)
print(sample_output3)
