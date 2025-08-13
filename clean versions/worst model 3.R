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




Confusion Matrix and Statistics

          Reference
Prediction    0    1
         0 2087  167
         1   22  724
                                          
               Accuracy : 0.937           
                 95% CI : (0.9277, 0.9454)
    No Information Rate : 0.703           
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.8417          
                                          
 Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.8126          
            Specificity : 0.9896          
         Pos Pred Value : 0.9705          
         Neg Pred Value : 0.9259          
             Prevalence : 0.2970          
         Detection Rate : 0.2413          
   Detection Prevalence : 0.2487          
      Balanced Accuracy : 0.9011          
                                          
       'Positive' Class : 1               
                                          
> 
> # --- Extract and Print Evaluation Metrics ---
> kappa3 <- cm3$overall['Kappa']
> precision3 <- cm3$byClass['Pos Pred Value']
> recall3 <- cm3$byClass['Sensitivity']
> f1_score3 <- 2 * (precision3 * recall3) / (precision3 + recall3)
> 
> cat("\n--- Evaluation Metrics (rf_model_worst3) ---\n")

--- Evaluation Metrics (rf_model_worst3) ---
> cat("Precision: ", round(precision3, 4), "\n")
Precision:  0.9705 
> cat("Recall (Sensitivity): ", round(recall3, 4), "\n")
Recall (Sensitivity):  0.8126 
> cat("F1 Score: ", round(f1_score3, 4), "\n")
F1 Score:  0.8845 
> cat("Kappa: ", round(kappa3, 4), "\n")
Kappa:  0.8417 
> 
> # --- Show a sample of predictions and probabilities ---
> cat("\n--- Sample Predictions and Probabilities ---\n")

--- Sample Predictions and Probabilities ---
> sample_output3 <- data.frame(
+   Actual = y_testRF[1:10],
+   Predicted = rf_predictions3[1:10],
+   Probability = round(rf_probabilities3[1:10], 4)
+ )
> print(sample_output3)
   Actual Predicted Probability
1       0         0        0.00
2       0         0        0.00
5       0         0        0.00
8       0         0        0.00
10      1         1        1.00
12      1         1        1.00
15      0         0        0.05
17      1         1        1.00
18      0         0        0.00
20      1         1        1.00
> 

