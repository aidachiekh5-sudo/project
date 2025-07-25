#dusplicate train nd to clear it up
train_copy <- train
train_copy <- train_copy[ , !(names(train_copy) %in% c("segment", "duration_quartile"))]

#see the data types
str(train_copy)

#Categorical features: SEGMENT
table(train_copy$SEGMENT)
prop.table(table(train_copy$SEGMENT))

#Plotting the distribution of the segment(bar chart)
library(ggplot2)
library(dplyr)

train_copy %>%
  count(SEGMENT) %>%                         
  mutate(perc = n / sum(n)) %>%
  ggplot(aes(x = SEGMENT, y = perc)) +        
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Distribution by Entreprise Type", x = "Entreprise Type", y = "Percentage")

#Cardinality check
library(dplyr)
n_distinct(train_copy$SEGMENT)

#Churn rate per segment category
library(dplyr)

train_copy %>%
  group_by(SEGMENT, CHURN_LABEL) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(SEGMENT) %>%
  mutate(perc = count / sum(count)) %>%
  filter(CHURN_LABEL == 1)  # Churn rate per segment

# Chi-Square Test of Independence:
# Contingency table
Contingency_tbl <- table(train_copy$SEGMENT, train_copy$CHURN_LABEL)
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
cramerV(table(train_copy$SEGMENT, train_copy$PLAN)) #Check for high association using Cramér’s V






# Categorical features: COUNTRY
table(train_copy$COUNTRY)
prop.table(table(train_copy$COUNTRY))

#Plotting the distribution of the segment(bar chart)
library(ggplot2)
library(dplyr)
library(scales)

train_copy %>%
  count(COUNTRY) %>%                         
  mutate(perc = n / sum(n)) %>%
  ggplot(aes(x = reorder(COUNTRY, -perc), y = perc)) +  # 👈 reorder by descending percentage
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Distribution by Country", x = "Country", y = "Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 👈 optional: rotate labels

#Cardinality check
unique(train_copy$COUNTRY)

#Churn rate per segment category
library(dplyr)

train_copy %>%
  group_by(COUNTRY, CHURN_LABEL) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(COUNTRY) %>%
  mutate(perc = count / sum(count)) %>%
  filter(CHURN_LABEL == 1)  # Churn rate per segment

# Chi-Square Test of Independence:
# Contingency table
Contingency_tbl <- table(train_copy$COUNTRY, train_copy$CHURN_LABEL)
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


# check multicollinearity
library(rcompanion)
cramerV(table(train_copy$COUNTRY, train_copy$PLAN)) #Check for high association using Cramér’s V




# Categorical features: INDUSTRY
table(train_copy$INDUSTRY)
prop.table(table(train_copy$INDUSTRY))

#Plotting the distribution of the segment(bar chart)
library(ggplot2)
library(dplyr)
library(scales)

train_copy %>%
  count(INDUSTRY) %>%                         
  mutate(perc = n / sum(n)) %>%
  ggplot(aes(x = reorder(INDUSTRY, -perc), y = perc)) +  # 👈 reorder by descending percentage
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Distribution by Industry", x = "Industry", y = "Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 👈 optional: rotate labels

#Cardinality check
unique(train_copy$INDUSTRY)

#Churn rate per segment category
library(dplyr)

train_copy %>%
  group_by(INDUSTRY, CHURN_LABEL) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(INDUSTRY) %>%
  mutate(perc = count / sum(count)) %>%
  filter(CHURN_LABEL == 1)  # Churn rate per segment

# Chi-Square Test of Independence:
# Contingency table
Contingency_tbl <- table(train_copy$INDUSTRY, train_copy$CHURN_LABEL)
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


# check multicollinearity
library(rcompanion)
cramerV(table(train_copy$INDUSTRY, train_copy$PLAN)) #Check for high association using Cramér’s V



# Categorical features: PLAN
table(train_copy$PLAN)
prop.table(table(train_copy$PLAN))

#Plotting the distribution of the segment(pie chart)
library(ggplot2)
library(dplyr)
library(scales)
# Prepare the data
plan_dist <- train_copy %>%
  filter(!is.na(PLAN)) %>%
  count(PLAN) %>%
  mutate(perc = n / sum(n),
         label = paste0(PLAN, ": ", percent(perc)))
# Plot pie chart
ggplot(plan_dist, aes(x = "", y = perc, fill = PLAN)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(title = "PLAN Distribution") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


#Cardinality check
unique(train_copy$PLAN)

#Churn rate per segment category
library(dplyr)

train_copy %>%
  group_by(PLAN, CHURN_LABEL) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(PLAN) %>%
  mutate(perc = count / sum(count)) %>%
  filter(CHURN_LABEL == 1)  # Churn rate per segment

# Chi-Square Test of Independence:
# Contingency table
Contingency_tbl <- table(train_copy$PLAN, train_copy$CHURN_LABEL)
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


# plot plan vs churn
library(dplyr)
library(ggplot2)
library(scales)

# Summarize churn rate by plan
churn_by_plan <- train %>%
  mutate(CHURN_LABEL = as.numeric(as.character(CHURN_LABEL))) %>%
  group_by(PLAN) %>%
  summarise(
    churn_rate = mean(CHURN_LABEL, na.rm = TRUE),
    count = n()
  ) %>%
  filter(count > 10) %>%
  arrange(desc(churn_rate))

# 🔥 Plot with precise y-axis + exact % labels
ggplot(churn_by_plan, aes(x = reorder(PLAN, -churn_rate), y = churn_rate, fill = PLAN)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = sprintf("%.1f%%", churn_rate * 100)),
    vjust = -0.5, size = 3
  ) +
  scale_y_continuous(
    limits = c(0, max(churn_by_plan$churn_rate) + 0.05),
    breaks = seq(0, 1, by = 0.05),
    labels = percent_format(accuracy = 0.1)
  ) +
  labs(
    title = "Churn Rate by Plan",
    x = "Plan",
    y = "Churn Rate (%)",
    fill = "Plan"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )



# check multicollinearity
library(rcompanion)
cramerV(table(train_copy$PLAN, train_copy$COUNTRY)) #Check for high association using Cramér’s V

# Categorical variables association
# ---------- SET-UP ----------
# If you haven't already:
install.packages(c("lsr", "dplyr", "purrr", "tibble"))   # cramersV lives in lsr
library(lsr)
library(dplyr)
library(purrr)
library(tibble)

# Make sure the variables are factors
cat_vars <- c("COUNTRY", "PLAN", "SEGMENT", "INDUSTRY")
train_copy[cat_vars] <- lapply(train_copy[cat_vars], as.factor)

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
  map_dfr(~ run_chisq(train_copy, .x[1], .x[2])) |>
  arrange(p_value)

print(results)

#association HEATMAP
# Load required packages
library(lsr)       # for cramersV()
library(reshape2)  # for melt()
library(ggplot2)

# Your data frame and categorical variables
cat_vars <- c("COUNTRY", "PLAN", "SEGMENT", "INDUSTRY")

# Make sure the variables are factors
train_copy[cat_vars] <- lapply(train_copy[cat_vars], as.factor)

# Compute Cramér's V matrix function
compute_cramers_matrix <- function(df, vars) {
  n <- length(vars)
  mat <- matrix(NA, nrow = n, ncol = n,
                dimnames = list(vars, vars))
  
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) {
        mat[i, j] <- 1
      } else {
        tbl <- table(df[[vars[i]]], df[[vars[j]]])
        mat[i, j] <- cramersV(tbl)
      }
    }
  }
  return(mat)
}

# Calculate the matrix
cramers_matrix <- compute_cramers_matrix(train_copy, cat_vars)

# Convert to long format for plotting
cramers_df <- melt(cramers_matrix, varnames = c("Var1", "Var2"), value.name = "CramersV")

# Remove diagonal (optional)
cramers_df <- cramers_df[cramers_df$Var1 != cramers_df$Var2, ]

# Order factors for clean plotting
cramers_df$Var1 <- factor(cramers_df$Var1, levels = cat_vars)
cramers_df$Var2 <- factor(cramers_df$Var2, levels = cat_vars)

# Plot heatmap
ggplot(cramers_df, aes(x = Var1, y = Var2, fill = CramersV)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "white", mid = "yellow", high = "red",
                       midpoint = 0.2, limit = c(0,1), space = "Lab",
                       name = "Cramér's V") +
  geom_text(aes(label = round(CramersV, 2)), color = "black", size = 5) +
  theme_minimal() +
  coord_fixed() +
  labs(title = "Heatmap of Categorical Variable Associations (Cramér's V)",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#Numerical features: MRR
summary(train_copy$MRR)

#plots: histogram
library(ggplot2)

ggplot(train_copy, aes(x = MRR)) +
  geom_histogram(breaks = seq(0, 10000, by = 500),
                 fill = "lightblue",
                 color = "white",
                 closed = "left") +   # equivalent to right = FALSE
  labs(title = "MRR Distribution",
       x = "MRR (USD)",
       y = "Count") +
  theme_minimal()

#Inspect the HISTOGRAM output:
MRR <- train_copy$MRR  # assign the numeric vector

hist_info <- hist(MRR)  # creates histogram and stores info
print(hist_info$breaks)  # prints the bins chosen by R


# Density plot 
library(ggplot2)

ggplot(train_copy, aes(x = MRR)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  labs(
    title = "MRR Distribution",
    x = "MRR(USD)",
    y = "Density"
  ) +
  theme_minimal()
#plots: boxplot
boxplot(train_copy$MRR,
        main = "Overall MRR Distribution",
        ylab = "Monthly Recurring Revenue (MRR)",
        col = "lightblue",
        outline = TRUE)  # TRUE shows outliers


t.test(MRR ~ CHURN_LABEL, data = train_copy) #can't  run t tests cuz my data is not normal


#Other Statistical Calculations
#C.1. Central Tendency & Dispersion

mean_val <- mean(train_copy$MRR, na.rm = TRUE)  # 'na.rm' excludes missing values
print(mean_val)
median_val <- median(train_copy$MRR, na.rm = TRUE)
print(median_val)
sd(train_copy$MRR, na.rm = TRUE)  # Remove NA values if needed
tapply(train_copy$MRR, train_copy$CHURN_LABEL, sd, na.rm = TRUE)  # std per grp churners vs non churners
IQR(train_copy$MRR)
tapply(train_copy$MRR, train_copy$CHURN_LABEL, IQR)

#C.2. Shape Metrics (Skewness & Kurtosis)
# install.packages("e1071")

library(e1071)

# Skewness
skewness(train_copy$MRR)

# Kurtosis
kurtosis(train_copy$MRR)

#C.3. Outlier Detection
# Calculate Q1 and Q3
Q1 <- quantile(train_copy$MRR, 0.25)
Q3 <- quantile(train_copy$MRR, 0.75)
IQR_value <- IQR(train_copy$MRR)
print(Q1)
print(Q3)
print(IQR_value)  
# Find outliers
outliers <- train_copy[train_copy$MRR < (Q1 - 1.5 * IQR_value) | train_copy$MRR > (Q3 + 1.5 * IQR_value), ]
# Number of outliers
nrow(outliers)

# Calculate Z-scores
z_scores <- scale(train_copy$MRR)
# Find outliers where absolute Z-score > 3
outliers_z <- train_copy[abs(z_scores) > 3, ]
# See how many outliers
nrow(outliers_z)
# View outlier values
outliers_z$MRR


sum(is.na(train_copy$MRR))
mean(is.na(train_copy$MRR)) * 100
colSums(is.na(train_copy))

#C.1. Central Tendency & Dispersion of churners and non churners
mean_cvsn<- tapply(train_copy$MRR, train_copy$CHURN_LABEL, mean, na.rm = TRUE)
print(mean_cvsn)

median_cvsn<- tapply(train_copy$MRR, train_copy$CHURN_LABEL, median, na.rm = TRUE)
print(median_cvsn)

std_cvsn<- tapply(train_copy$MRR, train_copy$CHURN_LABEL, sd, na.rm = TRUE)
print(std_cvsn)

IQR_cvsn<-tapply(train_copy$MRR, train_copy$CHURN_LABEL, IQR)
print(IQR_cvsn)

IQR(train_copy$MRR, na.rm = TRUE)

#C.2. Shape Metrics (Skewness & Kurtosis)
library(e1071)                 # Every session
install.packages("moments")    # Optional alternative
library(moments)
tapply(train_copy$MRR, train_copy$CHURN_LABEL, skewness, na.rm = TRUE) # For skewness

tapply(train_copy$MRR, train_copy$CHURN_LABEL, kurtosis, na.rm = TRUE) # For kurtosis

#C.3. Outlier Detection
# Calculate Q1 and Q3
tapply(train_copy$MRR, train_copy$CHURN_LABEL, function(x) quantile(x, 0.25, na.rm = TRUE))  # Q1
tapply(train_copy$MRR, train_copy$CHURN_LABEL, function(x) quantile(x, 0.75, na.rm = TRUE))  # Q3
tapply(train_copy$MRR, train_copy$CHURN_LABEL, IQR, na.rm = TRUE)                           # IQR

# Correlation Analysis

#Numerical features: SUBSCRIPTION_DURATION
# Plot histogram
library(ggplot2)

p<-ggplot(train_copy, aes(x = SUBSCRIPTION_DURATION)) +
  geom_histogram(binwidth = 50, fill = "#69b3a2", color = "white") +
  labs(title = "Subscription Duration Distribution",
       x = "Subscription Duration (Days)",
       y = "Number of Subscribers") +
  theme_minimal()

# Density plot 
library(ggplot2)

ggplot(train_copy, aes(x = SUBSCRIPTION_DURATION)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  labs(
    title = "Subscription Duration Distribution",
    x = "Subscription Duration (Days)",
    y = "Density"
  ) +
  theme_minimal()


#plots: boxplot

#option 1 
boxplot(train_copy$SUBSCRIPTION_DURATION,
        main = "Subscription Duration Distribution",
        ylab = "Subscription Duration (Days)",
        col = "lightblue",
        outline = TRUE)  # TRUE shows outliers

#option 2
install.packages("tidyverse")
library(tidyverse)
train_copy %>%
  ggplot(aes(y = SUBSCRIPTION_DURATION)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Subscription Duration",
       y = "Subscription Duration (Days)") +
  theme_minimal()



#Other Statistical Calculations
#C.1. Central Tendency & Dispersion

mean_val <- mean(train_copy$SUBSCRIPTION_DURATION, na.rm = TRUE)  # 'na.rm' excludes missing values
print(mean_val)
mean_cvsn1<- tapply(train_copy$SUBSCRIPTION_DURATION, train_copy$CHURN_LABEL, mean, na.rm = TRUE)
print(mean_cvsn1)

median_val <- median(train_copy$SUBSCRIPTION_DURATION, na.rm = TRUE)
print(median_val)
median_cvsn1<- tapply(train_copy$SUBSCRIPTION_DURATION, train_copy$CHURN_LABEL, median, na.rm = TRUE)
print(median_cvsn1)

sd(train_copy$SUBSCRIPTION_DURATION, na.rm = TRUE)  # Remove NA values if needed
tapply(train_copy$SUBSCRIPTION_DURATION, train_copy$CHURN_LABEL, sd, na.rm = TRUE)  # std per grp churners vs non churners


IQR(train_copy$AVG_MONTHLY_LOGINS)
tapply(train_copy$AVG_MONTHLY_LOGINS, train_copy$CHURN_LABEL, IQR)

#C.2. Shape Metrics (Skewness & Kurtosis)
# install.packages("e1071")

library(e1071)

# Skewness
skewness(train_copy$SUBSCRIPTION_DURATION)

# Kurtosis
kurtosis(train_copy$SUBSCRIPTION_DURATION)

#C.3. Outlier Detection
# Calculate Q1 and Q3
Q1 <- quantile(train_copy$SUBSCRIPTION_DURATION, 0.25)
Q3 <- quantile(train_copy$SUBSCRIPTION_DURATION, 0.75)
IQR_value <- IQR(train_copy$SUBSCRIPTION_DURATION)
print(Q1)
print(Q3)
print(IQR_value)  

# Find outliers
outliers <- train_copy[train_copy$SUBSCRIPTION_DURATION < (Q1 - 1.5 * IQR_value) | train_copy$SUBSCRIPTION_DURATION > (Q3 + 1.5 * IQR_value), ]
# Number of outliers
nrow(outliers)

sum(is.na(train_copy$SUBSCRIPTION_DURATION))
mean(is.na(train_copy$SUBSCRIPTION_DURATION)) * 100
colSums(is.na(train_copy))

#C.1. Central Tendency & Dispersion of churners and non churners
mean_cvsn<- tapply(train_copy$SUBSCRIPTION_DURATION, train_copy$CHURN_LABEL, mean, na.rm = TRUE)
print(mean_cvsn)

median_cvsn<- tapply(train_copy$SUBSCRIPTION_DURATION, train_copy$CHURN_LABEL, median, na.rm = TRUE)
print(median_cvsn)

std_cvsn<- tapply(train_copy$SUBSCRIPTION_DURATION, train_copy$CHURN_LABEL, sd, na.rm = TRUE)
print(std_cvsn)

IQR_cvsn<-tapply(train_copy$SUBSCRIPTION_DURATION, train_copy$CHURN_LABEL, IQR)
print(IQR_cvsn)

IQR(train_copy$SUBSCRIPTION_DURATION, na.rm = TRUE)

#C.2. Shape Metrics (Skewness & Kurtosis)
install.packages("moments")    # Optional alternative
library(e1071)                 # Every session
library(moments)
tapply(train_copy$SUBSCRIPTION_DURATION, train_copy$CHURN_LABEL, skewness, na.rm = TRUE) # For skewness

tapply(train_copy$SUBSCRIPTION_DURATION, train_copy$CHURN_LABEL, kurtosis, na.rm = TRUE) # For kurtosis

#C.3. Outlier Detection
# Calculate Q1 and Q3
tapply(train_copy$SUBSCRIPTION_DURATION, train_copy$CHURN_LABEL, function(x) quantile(x, 0.25, na.rm = TRUE))  # Q1
tapply(train_copy$SUBSCRIPTION_DURATION, train_copy$CHURN_LABEL, function(x) quantile(x, 0.75, na.rm = TRUE))  # Q3
tapply(train_copy$SUBSCRIPTION_DURATION, train_copy$CHURN_LABEL, IQR, na.rm = TRUE)                           # IQR

#C.5. Group-wise Statistics by Churn.
boxplot(SUBSCRIPTION_DURATION ~ CHURN_LABEL,
        data = train_copy,
        main = "Subscription duration by Churn Status",
        xlab = "Churn Status",
        ylab = "Subscription Duration (Days)",
        col = c("lightblue", "salmon"),
        outline = TRUE)  # keep TRUE to show outliers

#plots: density plot
library(ggplot2)
ggplot(train_copy, aes(x = SUBSCRIPTION_DURATION, fill = CHURN_LABEL)) +
  geom_density(alpha = 0.4) +
  labs(title = "Subscription Duration Distribution by Churn Status", x = "Subscription duration (days)", y = "Density") +
  theme_minimal()

# Correlation Analysis




#Numerical features: Average monthly logins 

summary(train_copy$AVG_MONTHLY_LOGINS)

#plots: histogram 
library(ggplot2)

ggplot(train_copy, aes(x = AVG_MONTHLY_LOGINS)) +
  geom_histogram(binwidth = 75,
                 fill = "#a8dadc",   # light pastel blue
                 color = "white",    # white border for smooth separation
                 alpha = 0.9) +
  labs(title = "Frequency Histogram of Average Monthly Logins",
       x = "Average Monthly Logins",
       y = "Count") +
  xlim(100, 1500) +
  theme_minimal(base_size = 14)

#plots: Kernel density plot
library(ggplot2)

ggplot(train_copy, aes(x = AVG_MONTHLY_LOGINS)) +
  geom_density(fill = "#a8dadc", color = "#1d3557", alpha = 0.7, size = 1) +
  labs(title = "Kernel Density Plot of Average Monthly Logins",
       x = "Average Monthly Logins",
       y = "Density") +
  xlim(100, 1500) +
  theme_minimal(base_size = 14)

#plots: boxplot
boxplot(train_copy$AVG_MONTHLY_LOGINS,
        main = "Overall Average Monthly Logins Distribution",
        ylab = "Average Monthly Logins",
        col = "lightblue",
        outline = TRUE,
        col.out = "red",   # red dots for outliers
        pch = 21,          # solid circle
        cex = 1.2)         # slightly bigger dots


# Q-Q Plot for normality
#option 1
qqnorm(train_copy$AVG_MONTHLY_LOGINS, main = "Q-Q Plot: AVG_MONTHLY_LOGINS")
qqline(train_copy$AVG_MONTHLY_LOGINS, col = "red", lwd = 2)


#option 2
# Load the required package
library(ggplot2)

# Create a Q-Q plot
ggplot(train_copy, aes(sample = AVG_MONTHLY_LOGINS)) +
  stat_qq(color = "#1d3557", size = 2, alpha = 0.6) +
  stat_qq_line(color = "#e63946", size = 1.2, linetype = "dashed") +
  labs(
    title = "Q-Q Plot of AVG_MONTHLY_LOGINS",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#d3d3d3", size = 0.3)
  )

# Create ECDF plot 
plot(ecdf(train_copy$AVG_MONTHLY_LOGINS),
     main = "ECDF of AVG_MONTHLY_LOGINS",
     xlab = "Average Monthly Logins",
     ylab = "ECDF",
     col = "blue",
     lwd = 2)

# Normality Tests
# Shapiro-Wilk Test
sum(is.na(train_copy$AVG_MONTHLY_LOGINS))
# Perform the Shapiro-Wilk Test on the variable
shapiro_result <- shapiro.test(train_copy$AVG_MONTHLY_LOGINS)
# Display the result
print(shapiro_result)
# interpretation
if (shapiro_result$p.value > 0.05) {
  cat("✅ Data is likely normally distributed (p-value =", shapiro_result$p.value, ")\n")
} else {
  cat("❌ Data is likely NOT normally distributed (p-value =", shapiro_result$p.value, ")\n")
}


# Kolmogorov-Smirnov Test
# Extract your variable
data <- train_copy$AVG_MONTHLY_LOGINS
data <- na.omit(data) # Remove NA values
mean_val <- mean(data) # Estimate sample mean and SD
sd_val <- sd(data)
# Perform the K-S test against normal distribution with estimated parameters
ks_result <- ks.test(data, "pnorm", mean = mean_val, sd = sd_val)
print(ks_result) # Show result
# interpretation
if (ks_result$p.value > 0.05) {
  cat("✅ Data is likely normally distributed (p-value =", ks_result$p.value, ")\n")
} else {
  cat("❌ Data is likely NOT normally distributed (p-value =", ks_result$p.value, ")\n")
}

# Anderson-Darling Test
install.packages("nortest")  # Only run once
library(nortest)
data <- na.omit(train_copy$AVG_MONTHLY_LOGINS) # Prepare data
ad_result <- ad.test(data) # Run Anderson-Darling Test
print(ad_result) # View the result
# Optional interpretation
if (ad_result$p.value > 0.05) {
  cat("✅ Data is likely normally distributed (p-value =", ad_result$p.value, ")\n")
} else {
  cat("❌ Data is likely NOT normally distributed (p-value =", ad_result$p.value, ")\n")
}


#Other Statistical Calculations
#C.1. Central Tendency & Dispersion

mean_val <- mean(train_copy$AVG_MONTHLY_LOGINS, na.rm = TRUE)  # 'na.rm' excludes missing values
print(mean_val)
mean_cvsn1<- tapply(train_copy$AVG_MONTHLY_LOGINS, train_copy$CHURN_LABEL, mean, na.rm = TRUE)
print(mean_cvsn1)

median_val <- median(train_copy$AVG_MONTHLY_LOGINS, na.rm = TRUE)
print(median_val)
median_cvsn1<- tapply(train_copy$AVG_MONTHLY_LOGINS, train_copy$CHURN_LABEL, median, na.rm = TRUE)
print(median_cvsn1)

sd(train_copy$AVG_MONTHLY_LOGINS, na.rm = TRUE)  # Remove NA values if needed
tapply(train_copy$AVG_MONTHLY_LOGINS, train_copy$CHURN_LABEL, sd, na.rm = TRUE)  # std per grp churners vs non churners


IQR(train_copy$AVG_MONTHLY_LOGINS)
tapply(train_copy$AVG_MONTHLY_LOGINS, train_copy$CHURN_LABEL, IQR)

#C.2. Shape Metrics (Skewness & Kurtosis)
# install.packages("e1071")

library(e1071)

# Skewness
skewness(train_copy$AVG_MONTHLY_LOGINS)

# Kurtosis
kurtosis(train_copy$AVG_MONTHLY_LOGINS)

#C.3. Outlier Detection
# Calculate Q1 and Q3
Q1 <- quantile(train_copy$AVG_MONTHLY_LOGINS, 0.25)
Q3 <- quantile(train_copy$AVG_MONTHLY_LOGINS, 0.75)
IQR_value <- IQR(train_copy$AVG_MONTHLY_LOGINS)
print(Q1)
print(Q3)
print(IQR_value)  

# Find outliers
outliers <- train_copy[train_copy$AVG_MONTHLY_LOGINS < (Q1 - 1.5 * IQR_value) | train_copy$AVG_MONTHLY_LOGINS > (Q3 + 1.5 * IQR_value), ]
# Number of outliers
nrow(outliers)

sum(is.na(train_copy$AVG_MONTHLY_LOGINS))
mean(is.na(train_copy$AVG_MONTHLY_LOGINS)) * 100
colSums(is.na(train_copy))

#C.1. Central Tendency & Dispersion of churners and non churners
mean_cvsn<- tapply(train_copy$AVG_MONTHLY_LOGINS, train_copy$CHURN_LABEL, mean, na.rm = TRUE)
print(mean_cvsn)

median_cvsn<- tapply(train_copy$AVG_MONTHLY_LOGINS, train_copy$CHURN_LABEL, median, na.rm = TRUE)
print(median_cvsn)

std_cvsn<- tapply(train_copy$AVG_MONTHLY_LOGINS, train_copy$CHURN_LABEL, sd, na.rm = TRUE)
print(std_cvsn)

IQR_cvsn<-tapply(train_copy$AVG_MONTHLY_LOGINS, train_copy$CHURN_LABEL, IQR)
print(IQR_cvsn)

IQR(train_copy$AVG_MONTHLY_LOGINS, na.rm = TRUE)

#C.2. Shape Metrics (Skewness & Kurtosis)
install.packages("moments")    # Optional alternative
library(e1071)                 # Every session
library(moments)
tapply(train_copy$AVG_MONTHLY_LOGINS, train_copy$CHURN_LABEL, skewness, na.rm = TRUE) # For skewness

tapply(train_copy$AVG_MONTHLY_LOGINS, train_copy$CHURN_LABEL, kurtosis, na.rm = TRUE) # For kurtosis

#C.5. Group-wise Statistics by Churn.
boxplot(AVG_MONTHLY_LOGINS ~ CHURN_LABEL,
        data = train_copy,
        main = "Average Monthly Logins by Churn Status",
        xlab = "Churn Status",
        ylab = "Average Monthly Logins",
        col = c("lightblue", "salmon"),
        outline = TRUE, # keep TRUE to show outliers
        col.out = "red",   # red dots for outliers
        pch = 21,          # solid circle
        cex = 1.2)         # slightly bigger dots
)  

#plots: density plot
library(ggplot2)

ggplot(train_copy, aes(x = AVG_MONTHLY_LOGINS, fill = CHURN_LABEL)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Average Monthly Logins Distribution by Churn Status",
    x = "Average Monthly Logins",
    y = "Density",
    fill = "Churn Status"
  ) +
  theme_minimal()


#plots: violon plot
library(ggplot2)

ggplot(train_copy, aes(x = CHURN_LABEL, y = AVG_MONTHLY_LOGINS, fill = CHURN_LABEL)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  # Adds a small boxplot inside
  labs(
    title = "Violin Plot of Average Monthly Logins by Churn Status",
    x = "Churn Status",
    y = "Average Monthly Logins"
  ) +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  theme_minimal()

# Normality (Shapiro-Wilk test for each group): didnt do it go back to it later on
shapiro.test(train_copy$AVG_MONTHLY_LOGINS[train_copy$CHURN_LABEL == "Yes"])
shapiro.test(train_copy$AVG_MONTHLY_LOGINS[train_copy$CHURN_LABEL == "No"])
# Variance test (equal variances)
var.test(AVG_MONTHLY_LOGINS ~ CHURN_LABEL, data = train_copy)
# t-test (automatically uses Welch’s t-test if variances are unequal)
t.test(AVG_MONTHLY_LOGINS ~ CHURN_LABEL, data = train_copy)

# Correlation Analysis





#Numerical features: Average tickets raised 

summary(train_copy$AVG_TICKETS_RAISED)

#plots: histogram 
library(ggplot2)

ggplot(train_copy, aes(x = AVG_TICKETS_RAISED)) +
  geom_histogram(binwidth = 50,
                 fill = "#a8dadc",
                 color = "white",
                 alpha = 0.9) +
  labs(
    title = "Distribution of Average Tickets Raised per Company",
    x = "Average Tickets Raised",
    y = "Number of Companies"
  ) +
  xlim(50, 800) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )


#plots: Kernel density plot
ggplot(train_copy, aes(x = AVG_TICKETS_RAISED)) +
  geom_density(fill = "#a8dadc", color = "#1d3557", alpha = 0.7, size = 1, adjust = 0.5) +
  labs(
    title = "Distribution of Average Tickets Raised per Company",
    x = "Average Tickets Raised",
    y = "Density"
  ) +
  xlim(50, 800) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )



#plots: boxplot
boxplot(train_copy$AVG_TICKETS_RAISED,
        main = "Overall Distribution of Average Tickets Raised per Company",
        ylab = "Average Tickets Raised",
        col = "lightblue",
        outline = TRUE,
        col.out = "red",   # red dots for outliers
        pch = 21,          # solid circle
        cex = 1.2)         # slightly bigger dots




# Q-Q Plot for normality
#option 1
qqnorm(train_copy$AVG_TICKETS_RAISED, main = "Q-Q Plot: AVG_TICKETS_RAISED")
qqline(train_copy$AVG_TICKETS_RAISED, col = "red", lwd = 2)


#option 2
# Load the required package
library(ggplot2)

# Create a Q-Q plot
ggplot(train_copy, aes(sample = AVG_TICKETS_RAISED)) +
  stat_qq(color = "#1d3557", size = 2, alpha = 0.6) +
  stat_qq_line(color = "#e63946", size = 1.2, linetype = "dashed") +
  labs(
    title = "Q-Q Plot of AVG_TICKETS_RAISED",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#d3d3d3", size = 0.3)
  )

# Create ECDF plot 
plot(ecdf(train_copy$AVG_TICKETS_RAISED),
     main = "ECDF of AVG_TICKETS_RAISED",
     xlab = "Average Tickets Raised",
     ylab = "ECDF",
     col = "blue",
     lwd = 2)

# Normality Tests
# Shapiro-Wilk Test
sum(is.na(train_copy$AVG_TICKETS_RAISED))
# Perform the Shapiro-Wilk Test on the variable
shapiro_result <- shapiro.test(train_copy$AVG_TICKETS_RAISED)
# Display the result
print(shapiro_result)
# interpretation
if (shapiro_result$p.value > 0.05) {
  cat("✅ Data is likely normally distributed (p-value =", shapiro_result$p.value, ")\n")
} else {
  cat("❌ Data is likely NOT normally distributed (p-value =", shapiro_result$p.value, ")\n")
}


# Kolmogorov-Smirnov Test
# Extract your variable
data <- train_copy$AVG_TICKETS_RAISED
data <- na.omit(data) # Remove NA values
mean_val <- mean(data) # Estimate sample mean and SD
sd_val <- sd(data)
# Perform the K-S test against normal distribution with estimated parameters
ks_result <- ks.test(data, "pnorm", mean = mean_val, sd = sd_val)
print(ks_result) # Show result
# interpretation
if (ks_result$p.value > 0.05) {
  cat("✅ Data is likely normally distributed (p-value =", ks_result$p.value, ")\n")
} else {
  cat("❌ Data is likely NOT normally distributed (p-value =", ks_result$p.value, ")\n")
}

# Anderson-Darling Test
install.packages("nortest")  # Only run once
library(nortest)
data <- na.omit(train_copy$AVG_TICKETS_RAISED) # Prepare data
ad_result <- ad.test(data) # Run Anderson-Darling Test
print(ad_result) # View the result
# Optional interpretation
if (ad_result$p.value > 0.05) {
  cat("✅ Data is likely normally distributed (p-value =", ad_result$p.value, ")\n")
} else {
  cat("❌ Data is likely NOT normally distributed (p-value =", ad_result$p.value, ")\n")
}


#Other Statistical Calculations
#C.1. Central Tendency & Dispersion

mean_val <- mean(train_copy$AVG_TICKETS_RAISED, na.rm = TRUE)  # 'na.rm' excludes missing values
print(mean_val)
mean_cvsn1<- tapply(train_copy$AVG_TICKETS_RAISED, train_copy$CHURN_LABEL, mean, na.rm = TRUE)
print(mean_cvsn1)

median_val <- median(train_copy$AVG_TICKETS_RAISED, na.rm = TRUE)
print(median_val)
median_cvsn1<- tapply(train_copy$AVG_TICKETS_RAISED, train_copy$CHURN_LABEL, median, na.rm = TRUE)
print(median_cvsn1)

sd(train_copy$AVG_TICKETS_RAISED, na.rm = TRUE)  # Remove NA values if needed
tapply(train_copy$AVG_TICKETS_RAISED, train_copy$CHURN_LABEL, sd, na.rm = TRUE)  # std per grp churners vs non churners


IQR(train_copy$AVG_TICKETS_RAISED)
tapply(train_copy$AVG_TICKETS_RAISED, train_copy$CHURN_LABEL, IQR)

#C.2. Shape Metrics (Skewness & Kurtosis)
# install.packages("e1071")

library(e1071)

# Skewness
skewness(train_copy$AVG_TICKETS_RAISED)

# Kurtosis
kurtosis(train_copy$AVG_TICKETS_RAISED)

#C.3. Outlier Detection
# Calculate Q1 and Q3
Q1 <- quantile(train_copy$AVG_TICKETS_RAISED, 0.25)
Q3 <- quantile(train_copy$AVG_TICKETS_RAISED, 0.75)
IQR_value <- IQR(train_copy$AVG_TICKETS_RAISED)
print(Q1)
print(Q3)
print(IQR_value)  

# Find outliers
outliers <- train_copy[train_copy$AVG_TICKETS_RAISED < (Q1 - 1.5 * IQR_value) | train_copy$AVG_TICKETS_RAISED > (Q3 + 1.5 * IQR_value), ]
# Number of outliers
nrow(outliers)

sum(is.na(train_copy$AVG_TICKETS_RAISED))
mean(is.na(train_copy$AVG_TICKETS_RAISED)) * 100
colSums(is.na(train_copy))

#C.1. Central Tendency & Dispersion of churners and non churners
mean_cvsn<- tapply(train_copy$AVG_TICKETS_RAISED, train_copy$CHURN_LABEL, mean, na.rm = TRUE)
print(mean_cvsn)

median_cvsn<- tapply(train_copy$AVG_TICKETS_RAISED, train_copy$CHURN_LABEL, median, na.rm = TRUE)
print(median_cvsn)

std_cvsn<- tapply(train_copy$AVG_TICKETS_RAISED, train_copy$CHURN_LABEL, sd, na.rm = TRUE)
print(std_cvsn)

IQR_cvsn<-tapply(train_copy$AVG_TICKETS_RAISED, train_copy$CHURN_LABEL, IQR)
print(IQR_cvsn)

IQR(train_copy$AVG_TICKETS_RAISED, na.rm = TRUE)

#C.2. Shape Metrics (Skewness & Kurtosis)
install.packages("moments")    # Optional alternative
library(e1071)                 # Every session
library(moments)
tapply(train_copy$AVG_TICKETS_RAISED, train_copy$CHURN_LABEL, skewness, na.rm = TRUE) # For skewness

tapply(train_copy$AVG_TICKETS_RAISED, train_copy$CHURN_LABEL, kurtosis, na.rm = TRUE) # For kurtosis

#C.5. Group-wise Statistics by Churn.
boxplot(AVG_TICKETS_RAISED ~ CHURN_LABEL,
        data = train_copy,
        main = "Average Tickets Raised by Churn Status",
        xlab = "Churn Status",
        ylab = "Average Tickets Raised",
        col = c("lightblue", "salmon"),
        outline = TRUE, # keep TRUE to show outliers
        col.out = "red",   # red dots for outliers
        pch = 21,          # solid circle
        cex = 1.2)         # slightly bigger dots
)  

#plots: density plot
library(ggplot2)

ggplot(train_copy, aes(x = AVG_TICKETS_RAISED, fill = CHURN_LABEL)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Average Tickets Raised Distribution by Churn Status",
    x = "Average Tickets Raised",
    y = "Density",
    fill = "Churn Status"
  ) +
  theme_minimal()


#plots: violon plot
library(ggplot2)

ggplot(train_copy, aes(x = CHURN_LABEL, y = AVG_TICKETS_RAISED, fill = CHURN_LABEL)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  # Adds a small boxplot inside
  labs(
    title = "Violin Plot of Average Tickets Raised by Churn Status",
    x = "Churn Status",
    y = "Average Tickets Raised"
  ) +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  theme_minimal()

# Normality (Shapiro-Wilk test for each group): didnt work on this t2akd
shapiro.test(train_copy$AVG_MONTHLY_LOGINS[train_copy$CHURN_LABEL == "Yes"])
shapiro.test(train_copy$AVG_MONTHLY_LOGINS[train_copy$CHURN_LABEL == "No"])
# Variance test (equal variances)
var.test(AVG_MONTHLY_LOGINS ~ CHURN_LABEL, data = train_copy)
# t-test (automatically uses Welch’s t-test if variances are unequal)
t.test(AVG_MONTHLY_LOGINS ~ CHURN_LABEL, data = train_copy)

# Correlation Analysis





#Numerical feature 3: Average feature usage 

summary(b2b_data4$AVG_FEATURE_USAGE)

#plots: histogram 
library(ggplot2)

ggplot(b2b_data4, aes(x = AVG_FEATURE_USAGE)) +
  geom_histogram(
    binwidth = 5,
    fill = "#a8dadc",      # Soft slate blue
    color = "white",       # Clean white bar borders
    alpha = 0.9
  ) +
  labs(
    title = "Feature Usage Distribution",
    subtitle = "Histogram of Average Feature Usage",
    x = "Average Feature Usage (%)",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "#dddddd"),
    panel.grid.minor = element_blank()
  )


#plots: Kernel density plot
ggplot(b2b_data4, aes(x = AVG_FEATURE_USAGE)) +
  geom_density(
    fill = "#a8dadc",     # Soft turquoise fill
    color = "#1d3557",    # Deep navy border
    alpha = 0.7,
    size = 1,
    adjust = 0.6          # Slight smoothing; adjust if too spiky or too flat
  ) +
  labs(
    title = "Distribution of Average Feature Usage per Company",
    x = "Average Feature Usage (%)",
    y = "Density"
  ) +
  xlim(0, 100) +           # Matches full range of values in summary
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )




#plots: boxplot
boxplot(b2b_data4$AVG_FEATURE_USAGE,
        main = "Overall Distribution of Average Feature Usage per Company",
        ylab = "Average Feature Usage",
        col = "lightblue",
        outline = TRUE,
        col.out = "red",   # red dots for outliers
        pch = 21,          # solid circle
        cex = 1.2)         # slightly bigger dots




# Q-Q Plot for normality
#option 1
qqnorm(b2b_data4$AVG_FEATURE_USAGE, main = "Q-Q Plot: AVG_FEATURE_USAGE")
qqline(b2b_data4$AVG_FEATURE_USAGE, col = "red", lwd = 2)


#option 2
# Load the required package
library(ggplot2)

# Create a Q-Q plot
ggplot(b2b_data4, aes(sample = AVG_FEATURE_USAGE)) +
  stat_qq(color = "#1d3557", size = 2, alpha = 0.6) +
  stat_qq_line(color = "#e63946", size = 1.2, linetype = "dashed") +
  labs(
    title = "Q-Q Plot of AVG_FEATURE_USAGE",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#d3d3d3", size = 0.3)
  )

# Create ECDF plot 
plot(ecdf(b2b_data4$AVG_FEATURE_USAGE),
     main = "ECDF of AVG_FEATURE_USAGE",
     xlab = "Average Feature Usage",
     ylab = "ECDF",
     col = "blue",
     lwd = 2)

# Normality Tests
# Shapiro-Wilk Test
sum(is.na(b2b_data4$AVG_FEATURE_USAGE))
# Perform the Shapiro-Wilk Test on the variable
shapiro_result <- shapiro.test(train_copy$AVG_FEATURE_USAGE)
# Display the result
print(shapiro_result)
# interpretation
if (shapiro_result$p.value > 0.05) {
  cat("✅ Data is likely normally distributed (p-value =", shapiro_result$p.value, ")\n")
} else {
  cat("❌ Data is likely NOT normally distributed (p-value =", shapiro_result$p.value, ")\n")
}


# Kolmogorov-Smirnov Test
# Extract your variable
data <- b2b_data4$AVG_FEATURE_USAGE
data <- na.omit(data) # Remove NA values
mean_val <- mean(data) # Estimate sample mean and SD
sd_val <- sd(data)
# Perform the K-S test against normal distribution with estimated parameters
ks_result <- ks.test(data, "pnorm", mean = mean_val, sd = sd_val)
print(ks_result) # Show result
# interpretation
if (ks_result$p.value > 0.05) {
  cat("✅ Data is likely normally distributed (p-value =", ks_result$p.value, ")\n")
} else {
  cat("❌ Data is likely NOT normally distributed (p-value =", ks_result$p.value, ")\n")
}

# Anderson-Darling Test
install.packages("nortest")  # Only run once
library(nortest)
data <- na.omit(b2b_data4$AVG_FEATURE_USAGE) # Prepare data
ad_result <- ad.test(data) # Run Anderson-Darling Test
print(ad_result) # View the result
# Optional interpretation
if (ad_result$p.value > 0.05) {
  cat("✅ Data is likely normally distributed (p-value =", ad_result$p.value, ")\n")
} else {
  cat("❌ Data is likely NOT normally distributed (p-value =", ad_result$p.value, ")\n")
}


#Other Statistical Calculations
#C.1. Central Tendency & Dispersion

mean_val <- mean(train_copy$AVG_FEATURE_USAGE, na.rm = TRUE)  # 'na.rm' excludes missing values
print(mean_val)
mean_cvsn1<- tapply(train_copy$AVG_FEATURE_USAGE, train_copy$CHURN_LABEL, mean, na.rm = TRUE)
print(mean_cvsn1)

median_val <- median(train_copy$AVG_FEATURE_USAGE, na.rm = TRUE)
print(median_val)
median_cvsn1<- tapply(train_copy$AVG_FEATURE_USAGE, train_copy$CHURN_LABEL, median, na.rm = TRUE)
print(median_cvsn1)

sd(train_copy$AVG_FEATURE_USAGE, na.rm = TRUE)  # Remove NA values if needed
tapply(train_copy$AVG_FEATURE_USAGE, train_copy$CHURN_LABEL, sd, na.rm = TRUE)  # std per grp churners vs non churners


IQR(train_copy$AVG_FEATURE_USAGE)
tapply(train_copy$AVG_FEATURE_USAGE
       
#C.2. Shape Metrics (Skewness & Kurtosis)
# install.packages("e1071")

library(e1071)

# Skewness
skewness(train_copy$AVG_FEATURE_USAGE)

# Kurtosis
kurtosis(train_copy$AVG_FEATURE_USAGE)

#C.3. Outlier Detection
# Calculate Q1 and Q3
Q1 <- quantile(train_copy$AVG_FEATURE_USAGE, 0.25)
Q3 <- quantile(train_copy$AVG_FEATURE_USAGE, 0.75)
IQR_value <- IQR(train_copy$AVG_FEATURE_USAGE)
print(Q1)
print(Q3)
print(IQR_value)  

# Find outliers
outliers <- train_copy[train_copy$AVG_FEATURE_USAGE < (Q1 - 1.5 * IQR_value) | train_copy$AVG_FEATURE_USAGE > (Q3 + 1.5 * IQR_value), ]
# Number of outliers
nrow(outliers)

sum(is.na(train_copy$AVG_FEATURE_USAGE))
mean(is.na(train_copy$AVG_FEATURE_USAGE)) * 100
colSums(is.na(train_copy))

#C.1. Central Tendency & Dispersion of churners and non churners
mean_cvsn<- tapply(train_copy$AVG_FEATURE_USAGE, train_copy$CHURN_LABEL, mean, na.rm = TRUE)
print(mean_cvsn)

median_cvsn<- tapply(train_copy$AVG_FEATURE_USAGE, train_copy$CHURN_LABEL, median, na.rm = TRUE)
print(median_cvsn)

std_cvsn<- tapply(train_copy$AVG_FEATURE_USAGE, train_copy$CHURN_LABEL, sd, na.rm = TRUE)
print(std_cvsn)

IQR_cvsn<-tapply(train_copy$AVG_FEATURE_USAGE, train_copy$CHURN_LABEL, IQR)
print(IQR_cvsn)


#C.2. Shape Metrics (Skewness & Kurtosis)
install.packages("moments")    # Optional alternative
library(e1071)                 # Every session
library(moments)
tapply(train_copy$AVG_FEATURE_USAGE, train_copy$CHURN_LABEL, skewness, na.rm = TRUE) # For skewness

tapply(train_copy$AVG_FEATURE_USAGE, train_copy$CHURN_LABEL, kurtosis, na.rm = TRUE) # For kurtosis

#C.5. Group-wise Statistics by Churn.
boxplot(AVG_FEATURE_USAGE ~ CHURN_LABEL,
        data = b2b_data4,
        main = "Average Feature Usage by Churn Status",
        xlab = "Churn Status",
        ylab = "Average Feature Usage",
        col = c("lightblue", "salmon"),
        outline = TRUE, # keep TRUE to show outliers
        col.out = "red",   # red dots for outliers
        pch = 21,          # solid circle
        cex = 1.2)         # slightly bigger dots
)  

#plots: density plot
library(ggplot2)

ggplot(train_copy, aes(x =AVG_FEATURE_USAGE , fill = CHURN_LABEL)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Average Feature Usage Distribution by Churn Status",
    x = "Average Feature Usage",
    y = "Density",
    fill = "Churn Status"
  ) +
  theme_minimal()


#plots: violon plot
library(ggplot2)

ggplot(train_copy, aes(x = CHURN_LABEL, y = AVG_FEATURE_USAGE, fill = CHURN_LABEL)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  # Adds a small boxplot inside
  labs(
    title = "Violin Plot of Average Feature Usage by Churn Status",
    x = "Churn Status",
    y = "Average Feature Usage"
  ) +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  theme_minimal()

# Normality (Shapiro-Wilk test for each group): didnt work on this t2akd
shapiro.test(train_copy$AVG_MONTHLY_LOGINS[train_copy$CHURN_LABEL == "Yes"])
shapiro.test(train_copy$AVG_MONTHLY_LOGINS[train_copy$CHURN_LABEL == "No"])
# Variance test (equal variances)
var.test(AVG_MONTHLY_LOGINS ~ CHURN_LABEL, data = train_copy)
# t-test (automatically uses Welch’s t-test if variances are unequal)
t.test(AVG_MONTHLY_LOGINS ~ CHURN_LABEL, data = train_copy)

# Correlation Analysis
library(ggplot2)
library(dplyr)
library(patchwork)  # for combining plots side by side

# Select your variables
df <- train_copy %>%
  select(AVG_MONTHLY_LOGINS, AVG_TICKETS_RAISED, AVG_FEATURE_USAGE)

# 1. Logins vs Tickets Raised
p1 <- ggplot(df, aes(x = AVG_MONTHLY_LOGINS, y = AVG_TICKETS_RAISED)) +
  geom_point(alpha = 0.6, color = "#1f78b4") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  # linear fit
  geom_smooth(method = "loess", se = FALSE, color = "blue", linetype = "solid") + # non-linear smoother
  labs(
    title = "Logins vs Tickets Raised",
    x = "Average Monthly Logins",
    y = "Average Tickets Raised"
  ) +
  theme_minimal()

# 2. Logins vs Feature Usage
p2 <- ggplot(df, aes(x = AVG_MONTHLY_LOGINS, y = AVG_FEATURE_USAGE)) +
  geom_point(alpha = 0.6, color = "#1f78b4") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "blue", linetype = "solid") +
  labs(
    title = "Logins vs Feature Usage",
    x = "Average Monthly Logins",
    y = "Average Feature Usage"
  ) +
  theme_minimal()

# 3. Tickets Raised vs Feature Usage
p3 <- ggplot(df, aes(x = AVG_TICKETS_RAISED, y = AVG_FEATURE_USAGE)) +
  geom_point(alpha = 0.6, color = "#1f78b4") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "blue", linetype = "solid") +
  labs(
    title = "Tickets Raised vs Feature Usage",
    x = "Average Tickets Raised",
    y = "Average Feature Usage"
  ) +
  theme_minimal()

# Combine plots side by side
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}
library(patchwork)

(p1 | p2 | p3) + plot_layout(guides = "collect")

#Correlations 
# If not installed yet
install.packages("lsr")

library(lsr)

cat_features <- c("SEGMENT", "INDUSTRY", "COUNTRY", "PLAN")

results <- data.frame(Feature = character(),
                      Chi_Square_p = numeric(),
                      Cramers_V = numeric(),
                      stringsAsFactors = FALSE)

for (feature in cat_features) {
  tbl <- table(train_copy[[feature]], train_copy$CHURN_LABEL)
  chisq_test <- suppressWarnings(chisq.test(tbl))
  cramer_v <- cramersV(tbl)
  
  results <- rbind(results, data.frame(
    Feature = feature,
    Chi_Square_p = chisq_test$p.value,
    Cramers_V = cramer_v
  ))
}

results <- results[order(results$Chi_Square_p), ]

print(results)

