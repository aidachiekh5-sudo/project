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





Confusion Matrix and Statistics

          Reference
Prediction    0    1
         0 2109  284
         1    0  607
                                          
               Accuracy : 0.9053          
                 95% CI : (0.8943, 0.9156)
    No Information Rate : 0.703           
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.7503          
                                          
 Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.6813          
            Specificity : 1.0000          
         Pos Pred Value : 1.0000          
         Neg Pred Value : 0.8813          
             Prevalence : 0.2970          
         Detection Rate : 0.2023          
   Detection Prevalence : 0.2023          
      Balanced Accuracy : 0.8406          
                                          
       'Positive' Class : 1               
                                          
> 
> # --- Extract and Print Evaluation Metrics ---
> kappa2 <- cm2$overall['Kappa']
> precision2 <- cm2$byClass['Pos Pred Value']
> recall2 <- cm2$byClass['Sensitivity']
> f1_score2 <- 2 * (precision2 * recall2) / (precision2 + recall2)
> 
> cat("\n--- Evaluation Metrics (rf_model_worst2) ---\n")

--- Evaluation Metrics (rf_model_worst2) ---
> cat("Precision: ", round(precision2, 4), "\n")
Precision:  1 
> cat("Recall (Sensitivity): ", round(recall2, 4), "\n")
Recall (Sensitivity):  0.6813 
> cat("F1 Score: ", round(f1_score2, 4), "\n")
F1 Score:  0.8104 
> cat("Kappa: ", round(kappa2, 4), "\n")
Kappa:  0.7503 
> 
> # --- Show a sample of predictions and probabilities ---
> cat("\n--- Sample Predictions and Probabilities ---\n")

--- Sample Predictions and Probabilities ---
> sample_output2 <- data.frame(
+   Actual = y_testRF[1:10],
+   Predicted = rf_predictions2[1:10],
+   Probability = round(rf_probabilities2[1:10], 4)
+ )
> print(sample_output2)
   Actual Predicted Probability
1       0         0        0.00
2       0         0        0.15
5       0         0        0.00
8       0         0        0.05
10      1         0        0.45
12      1         0        0.50
15      0         0        0.00
17      1         1        0.65
18      0         0        0.05
20      1         1        0.75
> 
