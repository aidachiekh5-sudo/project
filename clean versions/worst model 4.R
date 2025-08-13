rf_model_worst4 <- randomForest(CHURN_LABEL ~ ., 
                                data = train_rf_data, 
                                ntree = 40,                       # Number of trees
                                mtry = floor(sqrt(ncol(X_trainRF_balanced))), # Features per split
                                importance = TRUE,                 # Get variable importance
                                na.action = na.omit)               # Handle NAs

# --- Predictions ---
rf_predictions4 <- predict(rf_model_worst4, newdata = X_testRF)
rf_probabilities4 <- predict(rf_model_worst4, newdata = X_testRF, type = "prob")[, "1"]

# --- Confusion Matrix with Full Stats ---
library(caret)
cm4 <- confusionMatrix(rf_predictions4, y_testRF, positive = "1")  
print(cm4)

# --- Extract and Print Evaluation Metrics ---
kappa4 <- cm4$overall['Kappa']
precision4 <- cm4$byClass['Pos Pred Value']
recall4 <- cm4$byClass['Sensitivity']
f1_score4 <- 2 * (precision4 * recall4) / (precision4 + recall4)

cat("\n--- Evaluation Metrics (rf_model_worst4) ---\n")
cat("Precision: ", round(precision4, 4), "\n")
cat("Recall (Sensitivity): ", round(recall4, 4), "\n")
cat("F1 Score: ", round(f1_score4, 4), "\n")
cat("Kappa: ", round(kappa4, 4), "\n")

# --- Show a sample of predictions and probabilities ---
cat("\n--- Sample Predictions and Probabilities ---\n")
sample_output4 <- data.frame(
  Actual = y_testRF[1:10],
  Predicted = rf_predictions4[1:10],
  Probability = round(rf_probabilities4[1:10], 4)
)
print(sample_output4)
