
# Load required packages
library(tidyverse)
library(corrplot)
library(fastDummies)
library(reshape2)
library(ggplot2)

# Step 1: Data Preparation
df <- b2b_data4

# Ensure CHURN_LABEL is numeric
df$CHURN_LABEL <- as.numeric(as.character(df$CHURN_LABEL))

# Encode PLAN and SEGMENT
df$Plan_encoded <- factor(df$PLAN, levels = c("Pro", "Growth", "Enterprise"), ordered = TRUE)
df$Segment_encoded <- factor(df$SEGMENT, levels = c("SMB", "Mid-Market", "Enterprise"), ordered = TRUE)
df$Plan_num <- as.numeric(df$Plan_encoded)
df$Segment_num <- as.numeric(df$Segment_encoded)

# One-hot encode INDUSTRY
df <- dummy_cols(df, select_columns = "INDUSTRY", remove_first_dummy = TRUE)

# Step 2: Select variables for heatmap
heatmap_vars <- df %>%
  select(
    AVG_MONTHLY_LOGINS,
    AVG_TICKETS_RAISED,
    AVG_FEATURE_USAGE,
    MRR,
    SUBSCRIPTION_DURATION,
    CHURN_LABEL,
    Plan_num,
    Segment_num,
    starts_with("INDUSTRY_")
  )

# Step 3: Compute Spearman correlation matrix
cor_matrix <- cor(heatmap_vars, use = "complete.obs", method = "spearman")

# Step 4: Melt the correlation matrix for ggplot
cor_melted <- melt(cor_matrix)

# Step 5: Plot using ggplot
ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 1.5) +  # Bigger white borders for square feel
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limit = c(-1, 1),
                       name = "Spearman\nCorrelation") +
  theme_minimal(base_size = 16) +
  labs(title = "Spearman Correlation Heatmap") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 10)
  ) +
  coord_fixed()  # Keeps squares equal in size
