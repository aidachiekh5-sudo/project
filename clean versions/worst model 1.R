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




Confusion Matrix and Statistics

          Reference
Prediction    0    1
         0 2005  498
         1  104  393
                                          
               Accuracy : 0.7993          
                 95% CI : (0.7845, 0.8135)
    No Information Rate : 0.703           
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.4491          
                                          
 Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.4411          
            Specificity : 0.9507          
         Pos Pred Value : 0.7907          
         Neg Pred Value : 0.8010          
             Prevalence : 0.2970          
         Detection Rate : 0.1310          
   Detection Prevalence : 0.1657          
      Balanced Accuracy : 0.6959          
                                          
       'Positive' Class : 1               
                                          
> 
> # --- Extract and Print Evaluation Metrics ---
> kappa1 <- cm1$overall['Kappa']
> precision1 <- cm1$byClass['Pos Pred Value']
> recall1 <- cm1$byClass['Sensitivity']
> f1_score1 <- 2 * (precision1 * recall1) / (precision1 + recall1)
> 
> cat("\n--- Evaluation Metrics (rf_model_worst1) ---\n")

--- Evaluation Metrics (rf_model_worst1) ---
> cat("Precision: ", round(precision1, 4), "\n")
Precision:  0.7907 
> cat("Recall (Sensitivity): ", round(recall1, 4), "\n")
Recall (Sensitivity):  0.4411 
> cat("F1 Score: ", round(f1_score1, 4), "\n")
F1 Score:  0.5663 
> cat("Kappa: ", round(kappa1, 4), "\n")
Kappa:  0.4491 
> 
> # --- Show a sample of predictions and probabilities ---
> cat("\n--- Sample Predictions and Probabilities ---\n")

--- Sample Predictions and Probabilities ---
> sample_output1 <- data.frame(
+   Actual = y_testRF[1:10],
+   Predicted = rf_predictions1[1:10],
+   Probability = round(rf_probabilities1[1:10], 4)
+ )
> print(sample_output1)
   Actual Predicted Probability
1       0         0        0.25
2       0         0        0.35
5       0         0        0.15
8       0         0        0.15
10      1         1        0.55
12      1         0        0.20
15      0         0        0.25
17      1         0        0.45
18      0         0        0.20
20      1         1        0.75
> 
