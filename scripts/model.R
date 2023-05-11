# Load package
library(dplyr)
library(lubridate)
library(ROCR)
library(ranger)
library(ggplot2)
library(pROC)

# set seed
set.seed(1024)

# load data
setwd("/Users/laa.gxy/Desktop/2047Proj")
all_data <- read.csv("./data/data1.csv")
# make a copy
data <- all_data

# create a function to calculate mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Drop rows with missing values in Vehicle.Body.Type and Day_Night columns
data <- data[!is.na(data$Vehicle.Body.Type) & !is.na(data$Day_Night),]
# Impute missing data in the Vehicle.Body.Type column with the most frequent value
data$Vehicle.Body.Type[is.na(data$Vehicle.Body.Type)] <- names(which.max(table(data$Vehicle.Body.Type)))
# Impute missing data in the Day_Night column with the mode
data$Day_Night[is.na(data$Day_Night)] <- get_mode(data$Day_Night)

# train-test split at the ration 80% : 20%
split_at <- floor(nrow(data) * 0.8)
train <- data %>% 
  slice(1:split_at)
test <- data %>% 
  slice(split_at+1:n())

### Random Forest Model
# Define the values of num.trees
num_trees <- c(10, 50, 100, 150, 200)

# Create an empty list to store roc objects, precision, and recall values
roc_list <- vector("list", length(num_trees))

# Loop over num.trees and compute ROC curves
for (i in seq_along(num_trees)) {
  # Fit the random forest model
  model_rf <- ranger(Violation.Code ~ Vehicle.Body.Type + Violation.In.Front.Of.Or.Opposite
                     + Vehicle.Year + Day_Night + Violation.Borough + Intersected + Month + PAS,
                     data = train, 
                     probability = TRUE, 
                     respect.unordered.factors = TRUE, 
                     num.trees = num_trees[i])
  
  # Compute the AUC of this model on test
  pred_probability <- predict(model_rf, test, type = 'response')$predictions[, 2]
  roc_list[[i]] <- roc(test$Violation.Code, pred_probability)
}

# Print auc scores
roc_list

# Prepare to save figure
png(filename='./figures/ROC_rf.png')

# Add extra space to right of plot area
par(xpd=FALSE)

# Plot all ROC curves on the same graph
plot(roc_list[[1]], col = "red", print.auc = FALSE, auc.polygon = TRUE, grid = TRUE, main = "ROC Curves", xlab = "False Positive Rate", ylab = "True Positive Rate")
for (i in 2:length(num_trees)) {
  lines(roc_list[[i]], col = i, print.auc = FALSE, auc.polygon = TRUE)
}

# Add a legend
legend("bottomright", c("n.tree = 10", "n.tree = 50", "n.tree = 100", "n.tree = 150", "n.tree = 200"), col = 1:length(num_trees), lty = 1, cex = 0.8)

# Save the plot to a local file
dev.off()
