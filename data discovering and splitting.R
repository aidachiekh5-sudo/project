install.packages("readxl")     # for reading Excel files
install.packages("openxlsx")   # for writing Excel files
library(readxl)

# Read CSV file with full path and avoid converting text to factors
pt_data <- read.csv("C:/Users/Aïda/Desktop/archive/WA_Fn-UseC_-Telco-Customer-Churn.test.csv", 
                    stringsAsFactors = FALSE)

# View the first few rows
head(pt_data)

# Optional: Open a spreadsheet-like viewer
View(pt_data)



tech_data <- read.csv(file.choose(), stringsAsFactors = FALSE)
View(tech_data)

b2b_data<- read.csv(file.choose(), stringsAsFactors= FALSE)
View(b2b_data)

b2b_data1<- read.csv(file.choose(), stringsAsFactors= FALSE)
View(b2b_data1)

b2b_data2<- read.csv(file.choose(), stringsAsFactors= FALSE)
View(b2b_data2)

b2b_data3<- read.csv(file.choose(), stringsAsFactors= FALSE)
View(b2b_data3)

b2b_data4<- read.csv(file.choose(), stringsAsFactors= FALSE)
View(b2b_data4)

LS1<- read.csv(file.choose(), stringsAsFactors= FALSE)
View(LS1)

LS2<- read.csv(file.choose(), stringsAsFactors= FALSE)
View(LS2)

#summary of data columns and their types 
str(b2b_data1)

#Exact columns names 
colnames(b2b_data1)

#nb of churned columns and non churned
sum(b2b_data1$CHURN_LABEL == 1)
sum(b2b_data1$CHURN_LABEL == 0)

# Training datset keeping the order of rows 
wbcd_train <- b2b_data1[1:3500, ]
#nb of churned columns and non churned
sum(wbcd_train$CHURN_LABEL == 1)
sum(wbcd_train$CHURN_LABEL == 0)

# Testing datset
wbcd_test <- b2b_data1[3501:5000, ]
#nb of churned columns and non churned
sum(wbcd_test$CHURN_LABEL == 1)
sum(wbcd_test$CHURN_LABEL == 0)

prop.table(table(wbcd_train$CHURN_LABEL))

prop.table(table(wbcd_test$CHURN_LABEL))


#Training datset and # Testing datset using SRS 

# for reproducibility
set.seed(123)  

# Total number of rows
n <- 5000

# Generate a random sample of 70% of indices for training
train_indices <- sample(1:n, size = 0.7 * n, replace = FALSE)

# Split the data
wbcd_train1 <- b2b_data1[train_indices, ]
wbcd_test1  <- b2b_data1[-train_indices, ]

# Checking the % of churned and non churned 
sum(wbcd_train1$CHURN_LABEL == 1)
sum(wbcd_train1$CHURN_LABEL == 0)

prop.table(table(wbcd_train1$CHURN_LABEL))

prop.table(table(wbcd_test1$CHURN_LABEL))

summary(b2b_data1)

colSums(is.na(b2b_data1))
sapply(b2b_data1, class)

#Target Variable Overview (Churn)
table(wbcd_train$CHURN_LABEL)
prop.table(table(wbcd_train$CHURN_LABEL))

install.packages("ggplot2")
library(ggplot2)

library(ggplot2)
install.packages("caret")
install.packages("caTools")

library(caTools)
library(caret)

#splitting using sampling.split
split <- sample.split(b2b_data4$CHURN_LABEL, SplitRatio = 0.7)
train <- b2b_data4[split, ]
test <- b2b_data4[!split, ]

prop.table(table(b2b_data4$CHURN_LABEL))       # Original dataset
prop.table(table(train$CHURN_LABEL))    # Training set
prop.table(table(test$CHURN_LABEL))     # Testing set

b2b_data4 <- read.csv(file.choose())


#Plotting the churn distribution
library(ggplot2)
library(scales)  # for percent_format

ggplot(train, aes(x = factor(CHURN_LABEL), fill = factor(CHURN_LABEL))) +
  geom_bar(aes(y = after_stat(count / sum(count))), width = 0.6) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Churn Rate in Percent",
       x = "Churn (0 = No, 1 = Yes)",
       y = "Percentage",
       fill = "Churn") +
  theme_minimal()

#Churn Rate by Segment
library(dplyr)
library(ggplot2)
library(scales)

# Summarize churn rate by country
library(dplyr)
library(ggplot2)
library(scales)

# Summarize churn rate by country
churn_by_country <- train %>%
  mutate(CHURN_LABEL = as.numeric(as.character(CHURN_LABEL))) %>%
  group_by(COUNTRY) %>%
  summarise(
    churn_rate = mean(CHURN_LABEL, na.rm = TRUE),
    count = n()
  ) %>%
  filter(count > 10) %>%
  arrange(desc(churn_rate))

# 🔥 Plot with precise y-axis + exact % labels
ggplot(churn_by_country, aes(x = reorder(COUNTRY, -churn_rate), y = churn_rate, fill = COUNTRY)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = sprintf("%.1f%%", churn_rate * 100)),
    vjust = -0.5, size = 3
  ) +
  scale_y_continuous(
    limits = c(0, max(churn_by_country$churn_rate) + 0.05),
    breaks = seq(0, 1, by = 0.05),
    labels = percent_format(accuracy = 0.1)
  ) +
  labs(
    title = "Churn Rate by Country",
    x = "Country",
    y = "Churn Rate (%)",
    fill = "Country"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )


# Summarize churn rate by segment
churn_by_segment <- train %>%
  mutate(CHURN_LABEL = as.numeric(as.character(CHURN_LABEL))) %>%
  group_by(SEGMENT) %>%
  summarise(
    churn_rate = mean(CHURN_LABEL, na.rm = TRUE),
    count = n()
  ) %>%
  filter(count > 10) %>%  # optional: only segments with more than 10 records
  arrange(desc(churn_rate))

# Plot churn rate by segment with percentage labels
ggplot(churn_by_segment, aes(x = reorder(SEGMENT, -churn_rate), y = churn_rate, fill = SEGMENT)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = sprintf("%.1f%%", churn_rate * 100)),
    vjust = -0.5, size = 3
  ) +
  scale_y_continuous(
    limits = c(0, max(churn_by_segment$churn_rate) + 0.05),
    breaks = seq(0, 1, by = 0.05),
    labels = scales::percent_format(accuracy = 0.1)
  ) +
  labs(
    title = "Churn Rate by Segment",
    x = "Segment",
    y = "Churn Rate (%)",
    fill = "Segment"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Summarize churn rate by industry
churn_by_industry <- train %>%
  mutate(CHURN_LABEL = as.numeric(as.character(CHURN_LABEL))) %>%
  group_by(INDUSTRY) %>%
  summarise(
    churn_rate = mean(CHURN_LABEL, na.rm = TRUE),
    count = n()
  ) %>%
  filter(count > 10) %>%  # filter industries with more than 10 records
  arrange(desc(churn_rate))

# Plot churn rate by industry with percentage labels
ggplot(churn_by_industry, aes(x = reorder(INDUSTRY, -churn_rate), y = churn_rate, fill = INDUSTRY)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = sprintf("%.1f%%", churn_rate * 100)),
    vjust = -0.5, size = 3
  ) +
  scale_y_continuous(
    limits = c(0, max(churn_by_industry$churn_rate) + 0.05),
    breaks = seq(0, 1, by = 0.05),
    labels = scales::percent_format(accuracy = 0.1)
  ) +
  labs(
    title = "Churn Rate by Industry",
    x = "Industry",
    y = "Churn Rate (%)",
    fill = "Industry"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

#  churn rate vs MRR
# Load libraries
library(dplyr)
library(ggplot2)

# Step 1: Create quantile-based MRR bins (quintiles)
quantile_breaks <- quantile(train$MRR, probs = seq(0, 1, 0.2), na.rm = TRUE)

# Create interval labels like "500 - 1000"
labels <- paste0(
  round(quantile_breaks[-length(quantile_breaks)]), " - ",
  round(quantile_breaks[-1])
)

# Step 2: Apply bins and ensure CHURN_LABEL is numeric 0/1
train <- train %>%
  mutate(
    MRR_bin = cut(MRR,
                  breaks = quantile_breaks,
                  labels = labels,
                  include.lowest = TRUE),
    CHURN_LABEL = as.numeric(as.character(CHURN_LABEL))  # converts factor/char to numeric if needed
  )

# Step 3: Calculate churn rate and count per MRR bin
churn_rate <- train %>%
  filter(!is.na(MRR_bin) & CHURN_LABEL %in% c(0, 1)) %>%
  group_by(MRR_bin) %>%
  summarise(
    Churn_Rate = mean(CHURN_LABEL) * 100,
    Count = n()
  ) %>%
  ungroup()


# Step 4: Plot churn rate vs. MRR bin (line plot)
ggplot(churn_rate, aes(x = MRR_bin, y = Churn_Rate, group = 1)) +  # group=1 connects points in order
  geom_line(color = "steelblue", size = 1) +     # draws the line connecting points
  geom_point(color = "steelblue", size = 3) +    # draws dots at each point
  geom_text(aes(label = paste0(round(Churn_Rate, 1), "%")), vjust = -1, size = 4) +  # adds % labels
  labs(title = "Churn Rate by MRR Quintile",
       x = "MRR Range",
       y = "Churn Rate (%)") +
  theme_minimal() +
  ylim(0, max(churn_rate$Churn_Rate) + 10)       # ensures y-axis has some space above max point


#  churn rate vs subscription duration
summary(train$SUBSCRIPTION_DURATION)

ggplot(train, aes(x = SUBSCRIPTION_DURATION, fill = churn_status)) +
  geom_histogram(binwidth = 30, position = "stack", color = "black") +
  labs(
    title = "Distribution of Churned vs. Non-Churned Users",
    x = "Subscription Duration (Days)",
    y = "Count",
    fill = "Churn Status"
  ) +
  scale_fill_manual(values = c("Churned" = "#ff6666", "Not Churned" = "#66cc99")) +
  theme_minimal()





test <- read.csv(file.choose())






