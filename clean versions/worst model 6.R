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




Confusion Matrix and Statistics

          Reference
Prediction    0    1
         0 2109  168
         1    0  723
                                         
               Accuracy : 0.944          
                 95% CI : (0.9352, 0.952)
    No Information Rate : 0.703          
    P-Value [Acc > NIR] : < 2.2e-16      
                                         
                  Kappa : 0.8582         
                                         
 Mcnemar's Test P-Value : < 2.2e-16      
                                         
            Sensitivity : 0.8114         
            Specificity : 1.0000         
         Pos Pred Value : 1.0000         
         Neg Pred Value : 0.9262         
             Prevalence : 0.2970         
         Detection Rate : 0.2410         
   Detection Prevalence : 0.2410         
      Balanced Accuracy : 0.9057         
                                         
       'Positive' Class : 1              
                                         
> 
> # --- Extract and Print Evaluation Metrics ---
> kappa6 <- cm6$overall['Kappa']
> precision6 <- cm6$byClass['Pos Pred Value']
> recall6 <- cm6$byClass['Sensitivity']
> f1_score6 <- 2 * (precision6 * recall6) / (precision6 + recall6)
> 
> cat("\n--- Evaluation Metrics (rf_model_worst6) ---\n")

--- Evaluation Metrics (rf_model_worst6) ---
> cat("Precision: ", round(precision6, 4), "\n")
Precision:  1 
> cat("Recall (Sensitivity): ", round(recall6, 4), "\n")
Recall (Sensitivity):  0.8114 
> cat("F1 Score: ", round(f1_score6, 4), "\n")
F1 Score:  0.8959 
> cat("Kappa: ", round(kappa6, 4), "\n")
Kappa:  0.8582 
> 
> # --- Show a sample of predictions and probabilities ---
> cat("\n--- Sample Predictions and Probabilities ---\n")

--- Sample Predictions and Probabilities ---
> sample_output6 <- data.frame(
+   Actual = y_testRF[1:10],
+   Predicted = rf_predictions6[1:10],
+   Probability = round(rf_probabilities6[1:10], 4)
+ )
> print(sample_output6)
   Actual Predicted Probability
1       0         0       0.008
2       0         0       0.010
5       0         0       0.004
8       0         0       0.012
10      1         1       0.976
12      1         1       0.936
15      0         0       0.048
17      1         1       0.962
18      0         0       0.006
20      1         1       0.964
> 
