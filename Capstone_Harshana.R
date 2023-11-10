#install.packages("webshot")
#install.packages("kableExtra")
#install.packages('gmodels')
#webshot::install_phantomjs()
library(kableExtra)
library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)
library(tibble)
library(stats)


setwd("C:/Users/Patron/Documents/Capstone")
E_C_B <- read.csv("C:/Users/Patron/Documents/Capstone/E_C_B.csv")
View(E_C_B)

#checking for null values
E_C_B %>% summarise_all(funs(sum(is.na(.))))

# Null values are in days_since_prior_order, Filling NA values in days_since_prior_order with -1
E_C_B$days_since_prior_order[is.na(E_C_B$days_since_prior_order)] <- -1

# Change to integer
E_C_B$days_since_prior_order <- as.integer(E_C_B$days_since_prior_order)

# Change to factor
E_C_B$department_id <- as.factor(E_C_B$department_id)
E_C_B$product_id <- as.factor(E_C_B$product_id)
E_C_B$order_id <- as.factor(E_C_B$order_id)  
E_C_B$user_id <- as.factor(E_C_B$user_id)

# Change to logical
E_C_B$reordered <- as.logical(E_C_B$reordered)


# Create a data frame with summary statistics for the selected attributes
summary_stats <- data.frame(
  Definition = c("Day of week order was palced", "Hour of day order placed", "Number of days since prior order"),
  Minimum = sapply(E_C_B[, c("order_dow", "order_hour_of_day", "days_since_prior_order")], min, na.rm = TRUE),
  Maximum = sapply(E_C_B[, c("order_dow", "order_hour_of_day", "days_since_prior_order")], max, na.rm = TRUE),
  Mean = sapply(E_C_B[, c("order_dow", "order_hour_of_day", "days_since_prior_order")], mean, na.rm = TRUE),
  Std_Dev = sapply(E_C_B[, c("order_dow", "order_hour_of_day", "days_since_prior_order")], sd, na.rm = TRUE)
)


# Create the summary table with headings and lines
summary_table <- bind_rows(
  summary_stats
) %>%
  kable(format = "html", escape = FALSE) %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(0, bold = TRUE) %>%
  add_footnote(c("Summary statistics calculated from the data."))

summary_table


## finding the relationship between the days of the week and the number of orders
orders_by_day <- E_C_B %>%
  group_by(order_dow) %>%
  summarise(total_orders = n()) %>%
  mutate(day_of_week = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

# Create a bar plot
ggplot(orders_by_day, aes(x = factor(day_of_week, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")), y = total_orders)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(x = "Day of the Week", y = "Total Orders x 1000") +
  scale_x_discrete(labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")) +
  scale_y_continuous(labels = comma_format(scale = 1e-3, scale_suffix = "K")) + # Format y-axis labels
  theme_minimal()

## finding the relationship between the hours in a day and the number of orders
hours_of_day <- E_C_B %>%
  group_by(order_hour_of_day) %>%
  summarise(total_orders = n())

# Create a bar plot
ggplot(hours_of_day, aes(x = factor(order_hour_of_day), y = total_orders)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(x = "Hours of Day", y = "Total Orders x 1000") +
  scale_x_discrete() +
  scale_y_continuous(labels = comma_format(scale = 1e-3, scale_suffix = "K")) + # Format y-axis labels
  theme_minimal()

## find the most popular department
department_orders <- E_C_B %>%
  group_by(department) %>%
  summarise(total_orders = n())

# Sort departments by total_orders in descending order
department_orders <- department_orders %>%
  arrange(desc(total_orders))

# Create a color palette for gradient fill
color_palette <- scales::col_numeric(palette = "viridis", domain = department_orders$total_orders)(department_orders$total_orders)

# Create a bar plot
ggplot(department_orders, aes(x = reorder(department, -total_orders), y = total_orders, fill = total_orders)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma_format(scale = 1)) +  # Format y-axis labels without exponents
  labs(x = "Department", y = "Total Orders") +
  theme_minimal() +
  scale_fill_gradient(low = "lightblue", high = "darkblue", labels = comma_format(scale = 1)) +  # Format legend labels
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Vertical x-axis labels

# find the most the re-orderd products 
reordered_products <- E_C_B %>%
  group_by(product_name) %>%
  summarise(total_reordered = sum(reordered))

# Arrange products by total_reordered in descending order
reordered_products <- reordered_products %>%
  arrange(desc(total_reordered))

# Take the top 15 most reordered products
top_reordered_products <- head(reordered_products, 15)

# Create a bar plot
ggplot(top_reordered_products, aes(x = total_reordered, y = reorder(product_name, total_reordered))) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(x = "Number of Reorders", y = "Product Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Most Reordered Products")

# finding the relationship the number of orders and days passes since prior order of that.
orders_by_days <- E_C_B %>%
  group_by(days_since_prior_order) %>%
  summarise(total_orders = n())

# Create a bar plot
ggplot(orders_by_days, aes(x = factor(days_since_prior_order), y = total_orders)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(x = "Number of Days", y = "Number of Orders") +
  scale_x_discrete(limits = as.character(-1:31)) +  # Set the x-axis limits from -1 to 31
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Rotate x-axis labels


#K-Means
clst_prd <- data.table::dcast(E_C_B, user_id ~ department, fun.aggregate = length)

# Convert the resulting data table to a matrix
X_train <- as.matrix(clst_prd[,-1])

# Set the seed for reproducibility
set.seed(540)

# Perform K-means clustering with 6 clusters
kmeans_model <- kmeans(X_train, centers = 6)

# Print the clusters assigned to each observation
cat("The clusters are:", kmeans_model$cluster, "\n")

# Print Inertia
cat("The Inertia is:", kmeans_model$tot.withinss, "\n")

# Running K-Means on a range of clusters to find the optimal number
no_of_clusters <- 2:10
inertia <- numeric(length(no_of_clusters))

for (i in 1:length(no_of_clusters)) {
  set.seed(540)
  kmeans_model <- kmeans(X_train, centers = no_of_clusters[i])
  inertia[i] <- kmeans_model$tot.withinss
  cat("The inertia for", no_of_clusters[i], "Cluster is:", kmeans_model$tot.withinss, "\n")
}

inertia <- inertia / 1000000
# Creating a scree plot to visualize inertia (Elbow Method)
plot(no_of_clusters,inertia, type = "b", xlab = "Number of clusters", ylab = "Inertia Score")
title("Inertia Plot per k")

# Re-running K-Means on 5 clusters
set.seed(2)
kmeans_model <- kmeans(X_train, centers = 5)

# Predictions for new data
predictions <- kmeans_model$cluster

# Calculating the counts for each cluster
cluster_counts <- table(predictions)

# Creating a data frame for the counts
countscldf <- data.frame(cluster_counts)

# Display the data frame
countscldf

#PCA
X <- X_train
y_num <- predictions

# IDENTIFYING THE "BEST" NUMBER OF COMPONENTS: TRYING WITH DIMENSIONALITY REDUCTION & K-MEANS
n_components <- ncol(X)

# RUNNING PCA WITH ALL COMPONENTS
set.seed(453)
pca <- PCA(X, ncp = n_components,scale.unit = TRUE, graph = FALSE)
X_r <- as.data.frame(pca$ind$coord)

# CALCULATING THE 95% VARIANCE
total_variance <- sum(pca$eig[, "eigenvalue"])
cat("Total Variance in our dataset is: ", total_variance, "\n")
var_95 <- total_variance * 0.95
cat("The 95% variance we want to have is: ", var_95, "\n")

# TRYING TO HIT 95% VARIANCE
a <- data.frame(PCA_Comp = 1:n_components, Explained_Variance = pca$eig[, "eigenvalue"])
cat("Variance explain with 4 n_components: ", sum(a$Explained_Variance[1:4]), "\n")
cat("Variance explain with 8 n_components: ", sum(a$Explained_Variance[1:8]), "\n")
cat("Variance explain with 9 n_components: ", sum(a$Explained_Variance[1:9]), "\n")
cat("Variance explain with 10 n_components: ", sum(a$Explained_Variance[1:10]), "\n")
cat("Variance explain with 15 n_components: ", sum(a$Explained_Variance[1:15]), "\n")
cat("Variance explain with 18 n_components: ", sum(a$Explained_Variance[1:18]), "\n")
cat("Variance explain with 21 n_components: ", sum(a$Explained_Variance[1:21]), "\n")


ydata <- pca$eig[, "eigenvalue"] / 10

# PLOTTING DATA POINTS
plot(1:n_components, ydata, type = "b", xlab = "Components", ylab = "Explained Variance Ratio")

# PLOTTING LINE WITH 95% E.V.
abline(v = 9, col = "blue", lty = 2)

# Adding a label to the line
text(x = 18, y = 0.44, labels = "Components - 95% explained", col = "blue")

# ADDING ARROW POINTER
text(9, ydata[9], "9 vectors used", pos = 2, col = "blue")

# RUNNING PCA AGAIN WITH 9 COMPONENTS
pca <- PCA(X, ncp = 9,scale.unit = TRUE, graph = FALSE)
X_r <- as.data.frame(pca$ind$coord)

no_of_clusters <- 1:10

inertia <- numeric(length(no_of_clusters))

# RUNNING K-MEANS
for (f in no_of_clusters) {
  set.seed(453)
  kmeans_result <- kmeans(X_r, centers = f, nstart = 20)
  inertia[f] <- kmeans_result$tot.withinss
  cat("The inertia for", f, "Clusters is:", inertia[f], "\n")
}
inertia <- inertia / 1000000
# CREATING SCREE PLOT TO VISUALIZE INERTIA - ELBOW METHOD
plot(no_of_clusters, inertia, type = "b", xlab = "Number of Clusters", ylab = "Inertia Score")

## FINAL STEP:

# RUNNING PCA WITH 9 COMPONENTS
pca <- PCA(X, ncp = 9, graph = FALSE)
X_r <- as.data.frame(pca$ind$coord)

# RUNNING K-MEANS WITH 5 CLUSTERS
kmeans_result <- kmeans(X_r, centers = 5, nstart = 20)
clusters <- kmeans_result$cluster

# Create a data frame with the original dataset and cluster labels
clst_prd <- data.frame(X = X, clusters = clusters)

# CREATING A CLUSTER CATEGORY
clst_prd$Cluster_Category <- "No Data"
clst_prd$Cluster_Category[clst_prd$clusters == 1] <- "Cluster 1"
clst_prd$Cluster_Category[clst_prd$clusters == 2] <- "Cluster 2"
clst_prd$Cluster_Category[clst_prd$clusters == 3] <- "Cluster 3"
clst_prd$Cluster_Category[clst_prd$clusters == 4] <- "Cluster 4"
clst_prd$Cluster_Category[clst_prd$clusters == 5] <- "Cluster 5"

print(clst_prd)









