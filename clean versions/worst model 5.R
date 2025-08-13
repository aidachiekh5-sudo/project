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



Confusion Matrix and Statistics

          Reference
Prediction    0    1
         0 2103  167
         1    6  724
                                          
               Accuracy : 0.9423          
                 95% CI : (0.9334, 0.9504)
    No Information Rate : 0.703           
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.8543          
                                          
 Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.8126          
            Specificity : 0.9972          
         Pos Pred Value : 0.9918          
         Neg Pred Value : 0.9264          
             Prevalence : 0.2970          
         Detection Rate : 0.2413          
   Detection Prevalence : 0.2433          
      Balanced Accuracy : 0.9049          
                                          
       'Positive' Class : 1               
                                          
> 
> # --- Extract and Print Evaluation Metrics ---
> kappa5 <- cm5$overall['Kappa']
> precision5 <- cm5$byClass['Pos Pred Value']
> recall5 <- cm5$byClass['Sensitivity']
> f1_score5 <- 2 * (precision5 * recall5) / (precision5 + recall5)
> 
> cat("\n--- Evaluation Metrics (rf_model_worst5) ---\n")

--- Evaluation Metrics (rf_model_worst5) ---
> cat("Precision: ", round(precision5, 4), "\n")
Precision:  0.9918 
> cat("Recall (Sensitivity): ", round(recall5, 4), "\n")
Recall (Sensitivity):  0.8126 
> cat("F1 Score: ", round(f1_score5, 4), "\n")
F1 Score:  0.8933 
> cat("Kappa: ", round(kappa5, 4), "\n")
Kappa:  0.8543 
> 
> # --- Show a sample of predictions and probabilities ---
> cat("\n--- Sample Predictions and Probabilities ---\n")

--- Sample Predictions and Probabilities ---
> sample_output5 <- data.frame(
+   Actual = y_testRF[1:10],
+   Predicted = rf_predictions5[1:10],
+   Probability = round(rf_probabilities5[1:10], 4)
+ )
> print(sample_output5)
   Actual Predicted Probability
1       0         0        0.00
2       0         0        0.00
5       0         0        0.00
8       0         0        0.00
10      1         1        0.95
12      1         1        0.95
15      0         0        0.10
17      1         1        0.95
18      0         0        0.00
20      1         1        0.95
> 
