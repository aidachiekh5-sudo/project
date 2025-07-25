#Categorical vs churn 

#SEGMENT
# Chi-Square Test of Independence:
# Contingency table
Contingency_tbl <- table(b2b_data4$SEGMENT, b2b_data4$CHURN_LABEL)
print(Contingency_tbl)
# Chi-square test
test <- chisq.test(Contingency_tbl)
print(test)
print(test$statistic) #X_squared
# knowing our X_alpha
test$parameter # degrees of freedom
X_alpha <-qchisq(1 - 0.05, df = test$parameter)  # α = 0.05
# test conslusions
if (test$statistic > X_alpha) {
  print("Reject H₀: there is a significant relationship.")
} else {
  print("Fail to reject H₀: no significant relationship.")
}
# Cramér’s V (Strength of Association)
install.packages("rcompanion")
install.packages("TH.data")
library(rcompanion)
cv <- cramerV(Contingency_tbl)
print(cv)


# check multicollinearity
library(rcompanion)
cramerV(table(b2b_data4$SEGMENT, b2b_data4$PLAN)) #Check for high association using Cramér’s V

#Country
# Chi-Square Test of Independence:
# Contingency table
Contingency_tbl <- table(b2b_data4$COUNTRY, b2b_data4$CHURN_LABEL)
print(Contingency_tbl)
# Chi-square test
test <- chisq.test(Contingency_tbl)
print(test)
print(test$statistic) #X_squared
# knowing our X_alpha
test$parameter # degrees of freedom
X_alpha <-qchisq(1 - 0.05, df = test$parameter)  # α = 0.05
print(X_alpha)
# test conslusions
if (test$statistic > X_alpha) {
  print("Reject H₀: there is a significant relationship.")
} else {
  print("Fail to reject H₀: no significant relationship.")
}
# Cramér’s V (Strength of Association)
library(rcompanion)
cv <- cramerV(Contingency_tbl)
print(cv)
# check multicollinearity
library(rcompanion)
cramerV(table(b2b_data4$COUNTRY, b2b_data4$PLAN)) #Check for high association using Cramér’s V

#Industry
# Chi-Square Test of Independence:
# Contingency table
Contingency_tbl <- table(b2b_data4$INDUSTRY, b2b_data4$CHURN_LABEL)
print(Contingency_tbl)
# Chi-square test
test <- chisq.test(Contingency_tbl)
print(test)
print(test$statistic) #X_squared
# knowing our X_alpha
test$parameter # degrees of freedom
X_alpha <-qchisq(1 - 0.05, df = test$parameter)  # α = 0.05
print(X_alpha)
# test conslusions
if (test$statistic > X_alpha) {
  print("Reject H₀: there is a significant relationship.")
} else {
  print("Fail to reject H₀: no significant relationship.")
}
# Cramér’s V (Strength of Association)
library(rcompanion)
cv <- cramerV(Contingency_tbl)
print(cv)
# check multicollinearity
library(rcompanion)
cramerV(table(b2b_data4$INDUSTRY, b2b_data4$PLAN)) #Check for high association using Cramér’s V

#PLAN
# Chi-Square Test of Independence:
# Contingency table
Contingency_tbl <- table(b2b_data4$PLAN, b2b_data4$CHURN_LABEL)
print(Contingency_tbl)
# Chi-square test
test <- chisq.test(Contingency_tbl)
print(test)
print(test$statistic) #X_squared
# knowing our X_alpha
test$parameter # degrees of freedom
X_alpha <-qchisq(1 - 0.05, df = test$parameter)  # α = 0.05
print(X_alpha)
# test conslusions
if (test$statistic > X_alpha) {
  print("Reject H₀: there is a significant relationship.")
} else {
  print("Fail to reject H₀: no significant relationship.")
}
# Cramér’s V (Strength of Association)
install.packages("rcompanion")
install.packages("TH.data")
library(rcompanion)
cv <- cramerV(Contingency_tbl)
print(cv)


#Categorical vs categorical
install.packages(c("lsr", "purrr", "tibble"))   # cramersV lives in lsr
library(lsr)
library(dplyr)
library(purrr)
library(tibble)
# Make sure the variables are factors
cat_vars <- c("COUNTRY", "PLAN", "SEGMENT", "INDUSTRY")
b2b_data4[cat_vars] <- lapply(b2b_data4[cat_vars], as.factor)
# ---------- FUNCTION TO RUN ONE TEST ----------
run_chisq <- function(df, var1, var2) {
  tbl  <- table(df[[var1]], df[[var2]])
  test <- suppressWarnings(chisq.test(tbl))   # suppress warning on small expected counts
  v     <- cramersV(tbl)                      # effect-size
  
  tibble(
    Var1        = var1,
    Var2        = var2,
    ChiSq       = round(test$statistic, 2),
    df          = test$parameter,
    p_value     = signif(test$p.value, 3),
    Cramers_V   = round(v, 3)
  )
}
# ---------- ALL-AT-ONCE LOOP ----------
results <-
  combn(cat_vars, 2, simplify = FALSE) |>
  map_dfr(~ run_chisq(b2b_data4, .x[1], .x[2])) |>
  arrange(p_value)
print(results)


#Continuous vs churn 
#Average monthly logins
#Average tickets raised
b2b_data4$CHURN_LABEL <- as.numeric(as.character(b2b_data4$CHURN_LABEL))
vars <- b2b_data4[, c("AVG_MONTHLY_LOGINS", "AVG_TICKETS_RAISED", "CHURN_LABEL")] # Select the three variables
spearman_corr <- cor(vars, use = "complete.obs", method = "spearman")  # Compute Spearman correlation matrix
print(spearman_corr) # Print the result

#Average feature usage 
install.packages("psych")
library(psych)
b2b_data4$CHURN_LABEL <- as.numeric(as.character(b2b_data4$CHURN_LABEL)) # Make sure CHURN_LABEL is numeric (0 and 1)
cor_test_result <- cor.test(b2b_data4$AVG_FEATURE_USAGE, b2b_data4$CHURN_LABEL, method = "pearson") # Run the point-biserial correlation test
cor_test_result

#Discrete vs churn 
b2b_data4$CHURN_LABEL <- as.numeric(as.character(b2b_data4$CHURN_LABEL)) # Make sure churn is numeric
vars <- b2b_data4[, c("MRR", "SUBSCRIPTION_DURATION", "CHURN_LABEL")] # Select variables
cor(vars, use = "complete.obs", method = "spearman") # Spearman correlation

#MRR vs plan/segment
# Step 1: Create ordered factors for Plan and Segment
b2b_data4$Plan_encoded <- factor(b2b_data4$PLAN, levels = c("Pro", "Growth", "Enterprise"), ordered = TRUE)
b2b_data4$Segment_encoded <- factor(b2b_data4$SEGMENT, levels = c("SMB", "Mid-Market", "Enterprise"), ordered = TRUE)
as.integer(b2b_data4$Plan_encoded) #check how they're encoded
# Step 2: Convert ordered factors to numeric
b2b_data4$Plan_num <- as.numeric(b2b_data4$Plan_encoded)
b2b_data4$Segment_num <- as.numeric(b2b_data4$Segment_encoded)
# Step 3: Calculate Spearman correlation with MRR (replace 'MRR' with your exact MRR variable name)
cor_plan <- cor.test(b2b_data4$Plan_num, b2b_data4$MRR, method = "spearman")
cor_segment <- cor.test(b2b_data4$Segment_num, b2b_data4$MRR, method = "spearman")
# Step 4: Print results
print(cor_plan)
print(cor_segment)

#plan vs avg feature usage
# Step 3: Calculate Spearman correlation with AVG_FEATURE_USAGE
cor_plan <- cor.test(b2b_data4$Plan_num, b2b_data4$AVG_FEATURE_USAGE, method = "spearman")
# Step 4: Print results
print(cor_plan)

#MRR vs subscription duration
cor(b2b_data4$MRR, b2b_data4$SUBSCRIPTION_DURATION, method = "spearman")

#industry vs average feature usage
install.packages("fastDummies") 
library(fastDummies)
b2b_data4 <- dummy_cols(b2b_data4, select_columns = "INDUSTRY", remove_first_dummy = TRUE)

# Then get correlation for each industry
sapply(b2b_data4[, grepl("INDUSTRY_", names(b2b_data4))], function(col) {
  cor(col, b2b_data4$AVG_FEATURE_USAGE, method = "spearman")
})


#HEATMAPS
# Load required libraries
library(tidyverse)
library(fastDummies)
library(reshape2)

# Step 1: Encode PLAN and SEGMENT as ordinal
b2b_data4$Plan_encoded <- factor(b2b_data4$PLAN, levels = c("Pro", "Growth", "Enterprise"), ordered = TRUE)
b2b_data4$Segment_encoded <- factor(b2b_data4$SEGMENT, levels = c("SMB", "Mid-Market", "Enterprise"), ordered = TRUE)
b2b_data4$Plan_num <- as.numeric(b2b_data4$Plan_encoded)
b2b_data4$Segment_num <- as.numeric(b2b_data4$Segment_encoded)

# Step 2: Encode INDUSTRY as dummy variables
b2b_data4 <- fastDummies::dummy_cols(b2b_data4, select_columns = "INDUSTRY", remove_first_dummy = TRUE)

# Step 3: Select all relevant columns for correlation
cor_data <- b2b_data4 %>%
  dplyr::select(
    AVG_MONTHLY_LOGINS,
    AVG_TICKETS_RAISED,
    AVG_FEATURE_USAGE,
    CHURN_LABEL,
    MRR,
    SUBSCRIPTION_DURATION,
    Plan_num,
    Segment_num,
    dplyr::starts_with("INDUSTRY_")
  )

# Step 4: Compute Spearman correlation matrix
corr_matrix <- cor(cor_data, use = "complete.obs", method = "spearman")

# Step 5: Melt the matrix to long format for ggplot
melted_corr <- melt(corr_matrix, varnames = c("Variable1", "Variable2"), value.name = "Correlation")

# Step 6: Plot heatmap using ggplot2
ggplot(melted_corr, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Spearman\nCorrelation"
  ) +
  geom_text(aes(label = round(Correlation, 2)), size = 3.2, color = "black") +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.grid = element_blank()
  ) +
  coord_fixed()
