# Load required libraries
library(dplyr)
library(ggplot2)
library(caret)
library(pROC)

# Read the dataset
df <- read.csv("bank-loan.csv")

# STEP 1: Clean and Explore the Data
df <- df %>%
  mutate(default = as.factor(default))  # Convert target to factor

# Basic visualization
ggplot(df, aes(x = income, fill = default)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
  labs(title = "Income Distribution by Loan Default")

# STEP 2: Split into training and test sets
set.seed(123)
train_index <- createDataPartition(df$default, p = 0.7, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# STEP 3: Build Logistic Regression Model
model <- glm(default ~ ., data = train_data, family = binomial)

# STEP 4: Predict probabilities and classify
probabilities <- predict(model, test_data, type = "response")
predicted_class <- ifelse(probabilities > 0.5, 1, 0)
predicted_class <- factor(predicted_class, levels = c(0, 1))

# STEP 5: Evaluate Model
conf_matrix <- confusionMatrix(predicted_class, test_data$default)
print(conf_matrix)

# ROC Curve and AUC
roc_obj <- roc(as.numeric(as.character(test_data$default)), probabilities)
plot(roc_obj, main = "ROC Curve")
cat("AUC =", auc(roc_obj), "\n")
