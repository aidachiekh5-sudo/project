rf_model_worst2 <- randomForest(CHURN_LABEL ~ ., 
                                data = train_rf_data, 
                                ntree = 20,                   # Very few trees
                                mtry = 2,                    # Only one feature considered per split (too restrictive)
                                importance = FALSE,           # No importance tracking
                                na.action = na.omit)

# --- Predictions ---
rf_predictions2 <- predict(rf_model_worst2, newdata = X_testRF)
rf_probabilities2 <- predict(rf_model_worst2, newdata = X_testRF, type = "prob")[, "1"]

# --- Confusion Matrix with Full Stats ---
library(caret)
cm2 <- confusionMatrix(rf_predictions2, y_testRF, positive = "1")  
print(cm2)

# --- Extract and Print Evaluation Metrics ---
kappa2 <- cm2$overall['Kappa']
precision2 <- cm2$byClass['Pos Pred Value']
recall2 <- cm2$byClass['Sensitivity']
f1_score2 <- 2 * (precision2 * recall2) / (precision2 + recall2)

cat("\n--- Evaluation Metrics (rf_model_worst2) ---\n")
cat("Precision: ", round(precision2, 4), "\n")
cat("Recall (Sensitivity): ", round(recall2, 4), "\n")
cat("F1 Score: ", round(f1_score2, 4), "\n")
cat("Kappa: ", round(kappa2, 4), "\n")

# --- Show a sample of predictions and probabilities ---
cat("\n--- Sample Predictions and Probabilities ---\n")
sample_output2 <- data.frame(
  Actual = y_testRF[1:10],
  Predicted = rf_predictions2[1:10],
  Probability = round(rf_probabilities2[1:10], 4)
)
print(sample_output2)
