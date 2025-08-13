rf_model_worst1 <- randomForest(CHURN_LABEL ~ ., 
                                data = train_rf_data, 
                                ntree = 20,                   # Very few trees
                                mtry = 1,                    # Only one feature considered per split (too restrictive)
                                importance = FALSE,           # No importance tracking
                                na.action = na.omit)

# --- Predictions ---
rf_predictions1 <- predict(rf_model_worst1, newdata = X_testRF)
rf_probabilities1 <- predict(rf_model_worst1, newdata = X_testRF, type = "prob")[, "1"]

# --- Confusion Matrix with Full Stats ---
library(caret)
cm1 <- confusionMatrix(rf_predictions1, y_testRF, positive = "1")  
print(cm1)

# --- Extract and Print Evaluation Metrics ---
kappa1 <- cm1$overall['Kappa']
precision1 <- cm1$byClass['Pos Pred Value']
recall1 <- cm1$byClass['Sensitivity']
f1_score1 <- 2 * (precision1 * recall1) / (precision1 + recall1)

cat("\n--- Evaluation Metrics (rf_model_worst1) ---\n")
cat("Precision: ", round(precision1, 4), "\n")
cat("Recall (Sensitivity): ", round(recall1, 4), "\n")
cat("F1 Score: ", round(f1_score1, 4), "\n")
cat("Kappa: ", round(kappa1, 4), "\n")

# --- Show a sample of predictions and probabilities ---
cat("\n--- Sample Predictions and Probabilities ---\n")
sample_output1 <- data.frame(
  Actual = y_testRF[1:10],
  Predicted = rf_predictions1[1:10],
  Probability = round(rf_probabilities1[1:10], 4)
)
print(sample_output1)
