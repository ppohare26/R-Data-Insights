# ********************* Cluster Analysis ****************************
# K-Means Clustering to create 2 clusters based on selected variables
# Variables considered: Income, Recency, NumWebPurchases, 
# NumCatalogPurchases, NumStorePurchases

install.packages("readxl")
library(readxl)
library(openxlsx)

# Step 1: Load the dataset
dataset <- read_excel("Team4_R.xlsx")

# Step 2: Select relevant variables for clustering
selected_data <- dataset[, c("Income", "Recency", "NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases")]

# Step 3: Check for missing values
selected_data <- na.omit(selected_data)

# Step 4: Normalize the data for clustering
scaled_data <- scale(selected_data)

# Print the scaled data to inspect before clustering
cat("Scaled Data (First 5 Rows):\n")
print(head(scaled_data, 5))

# Step 5: Perform K-Means clustering with k=2
set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 2, nstart = 25)

# Step 6: Add cluster labels to the original dataset
dataset$Cluster <- NA
dataset[rownames(selected_data), "Cluster"] <- as.factor(kmeans_result$cluster)

dataset_1<-subset(dataset,Cluster==1)
dataset_2<-subset(dataset,Cluster==2)

# Step 7: Summarize the clusters
cluster_summary <- aggregate(selected_data, by = list(Cluster = kmeans_result$cluster), FUN = mean)

# Step 8: Save the cluster results
write.xlsx(dataset_1, "Cluster_Analysis_Results_1.xlsx", overwrite = TRUE)
write.xlsx(dataset_2, "Cluster_Analysis_Results_2.xlsx", overwrite = TRUE)
write.xlsx(cluster_summary, "Cluster_Summary.xlsx", overwrite = TRUE)

# ********************* Predictive Analysis ****************************
# Classification Tree to predict the Response variable for Cluster 1 customers

install.packages("rpart.plot")
library(rpart)
library(rpart.plot) # for better visualisation
library(readxl)

# Step 1: Load the dataset from the clustering results file
cluster1_data <- read_excel("Cluster_Analysis_Results_1.xlsx")

View(cluster1_data)
# Step 2: Split data into training (70%) and testing (30%)
set.seed(456)
train_idx <- sample(seq_len(nrow(cluster1_data)), size = 0.7 * nrow(cluster1_data))
train_data <- cluster1_data[train_idx, ]
test_data <- cluster1_data[-train_idx, ]

# Step 3: Build a Classification Tree model
class_tree <- rpart(Response ~ Income + Recency + NumWebPurchases + NumCatalogPurchases + NumStorePurchases,
                    data = train_data, method = "class")

# Step 4: Print and visualize the Classification Tree
print(class_tree)
rpart.plot(class_tree, type = 3, fallen.leaves = TRUE, main = "Classification Tree for Cluster 1")

# Step 5: Predict on the test data
predictions <- predict(class_tree, test_data, type = "class")

# Step 6: Evaluate the model using a Confusion Matrix
conf_matrix <- table(test_data$Response, predictions)

# Calculate Evaluation Metrics
TP <- conf_matrix[2, 2]
FN <- conf_matrix[2, 1]
FP <- conf_matrix[1, 2]
TN <- conf_matrix[1, 1]

# Accuracy
accuracy <- (TP + TN) / sum(conf_matrix)

# Sensitivity (Recall)
sensitivity <- TP / (TP + FN)

# Precision
precision <- TP / (TP + FP)

# F1 Score
f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)

# Step 7: Print Evaluation Metrics
cat("Evaluation Metrics:\n")
cat("Accuracy: ", round(accuracy, 4), "\n")
cat("Sensitivity (Recall): ", round(sensitivity, 4), "\n")
cat("Precision: ", round(precision, 4), "\n")
cat("F1 Score: ", round(f1_score, 4), "\n")

