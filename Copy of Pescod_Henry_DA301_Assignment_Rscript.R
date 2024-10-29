# LSE Data Analytics Online Career Accelerator 
## Pescod_Henry_DA301_Assignment_R_Studio_Script
# DA301: Predicting future outcomes for Turtle Games

###############################################################################
#SECTION 1 - EXPLORATORY DATA ANALYSIS
###############################################################################

# 1. Import all required libraries

# The whole tidyverse package.
library(tidyverse)
# Import and read CSV file.
library(readr)
# Data wrangling.
library(dplyr)
# Data wrangling.
library(tidyr)
# Create statistical summaries.
library(skimr)
# Create a report as an HTML file.
library(DataExplorer)
# Data plotting.
library(ggplot2)

# install the moments package and load the library
install.packages('moments')
library(moments)

# Install the car package if not already installed
install.packages("car")
library(car)
############################################################################

# 2. Load and explore the data

# Read the CSV file (reviews_new.csv).
reviews_new <- read.csv('reviews_new_5_clusters.csv', header=TRUE)

# View the first six lines of the data frame.
head(reviews_new)
str(reviews_new)

# Review/sense-check the data set.
as_tibble (reviews_new)

# These functions provide summary statistics of the data set.
summary(reviews_new)

View(reviews_new)

# Count number of unique values in product column
reviews_new %>%
  count(product)

# To search for missing values in a data set.
reviews_new[is.na(reviews_new)]

############################################################################

# 3. Exploratory data analysis and visualisation

# Scatter plot of spend vs loyalty points with a linear regression line
ggplot(data = reviews_new,
       mapping = aes(x = spending_score, y = loyalty_points)) +
  geom_point(color = 'blue', alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Loyalty Points vs. Spending score with Trend Line", x = "Spending Score", y = "Loyalty Points") +
  theme_minimal()

########################################################################### 

# Scatter plot of remuneration vs loyalty points with a linear regression line
ggplot(data = reviews_new, 
       mapping = aes(x = remuneration, y = loyalty_points)) + 
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Loyalty Points vs. Remuneration with Trend Line", x = "Remuneration", y = "Loyalty Points") +
  theme_minimal()

########################################################################### 

# bar plot of gender vs loyalty points
# Summarise data: total loyalty points by gender
summarised_data <- reviews_new %>%
  group_by(gender) %>%
  summarise(total_loyalty_points = sum(loyalty_points, na.rm = TRUE))

# Create the bar plot
ggplot(data = summarised_data, mapping = aes(x = gender, y = total_loyalty_points)) +
  geom_col(color = 'black', fill = 'steelblue',size = 1.5) +
  labs(title = "Total Loyalty Points by Gender", x = "Gender", y = "Total Loyalty Points") +
  theme_minimal()

###########################################################################

# Summarize data: mean loyalty points by gender
summarized_data <- reviews_new %>%
  group_by(gender) %>%
  summarize(mean_loyalty_points = mean(loyalty_points, na.rm = TRUE))

# Create the bar plot
ggplot(data = summarized_data, mapping = aes(x = gender, y = mean_loyalty_points)) +
  geom_col(color = 'black', fill ='steelblue', size = 1.5) +
  labs(title = "Mean Loyalty Points by Gender", x = "Gender", y = "Mean Loyalty Points") +
  theme_minimal()

###########################################################################
# Summarise data: total loyalty points by education
summarised_data <- reviews_new %>%
  group_by(education) %>%
  summarise(total_loyalty_points = sum(loyalty_points, na.rm = TRUE))

# Create the bar plot
ggplot(data = summarised_data, mapping = aes(x = education, y = total_loyalty_points)) +
  geom_col(color = 'green', fill = 'green', alpha = 0.7, size = 1.5) +
  labs(title = "Total Loyalty Points by Education", x = "Education", y = "Total Loyalty Points") +
  theme_minimal()

###########################################################################

# Summarise data: mean loyalty points by education
summarised_data <- reviews_new %>%
  group_by(education) %>%
  summarise(total_loyalty_points = mean(loyalty_points, na.rm = TRUE))

# Create the bar plot
ggplot(data = summarised_data, mapping = aes(x = education, y = total_loyalty_points)) +
  geom_col(color = 'green', fill = 'green', alpha = 0.7, size = 1.5) +
  labs(title = "Mean Loyalty Points by Education", x = "Education", y = "Total Loyalty Points") +
  theme_minimal()

###########################################################################

# Bar plot showing ages bins against total loyalty points
# Step 1: Create age bins using the `cut()` function
reviews_new <- reviews_new %>%
  mutate(age_group = cut(age, breaks = c(0, 25, 35, 45, 60, Inf), right = FALSE, 
                         labels = c("<24", "25-34", "35-44", "45-59", "60+")))

# Step 2: Summarize total loyalty points by age group
summarized_data <- reviews_new %>%
  group_by(age_group) %>%
  summarize(total_loyalty_points = sum(loyalty_points, na.rm = TRUE))

# Step 3: Create the bar plot to show total loyalty points by age group
ggplot(data = summarized_data, aes(x = age_group, y = total_loyalty_points)) +
  geom_col(fill ="steelblue", color ="black") +
  labs(title = "Total Loyalty Points by Age Group", x = "Age Group", y = "Total Loyalty Points") +
  theme_minimal()


###########################################################################

# visualise data with histogram

# Histogram of age
ggplot(reviews_new,aes(x = age)) +
  # Add a geom layer to specify the plot type
  geom_histogram(stat = 'count')

# Histogram of age with assigned bin widths
ggplot(reviews_new, aes(x = age)) +
  # Add a geom layer to specify the plot type with bin width
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Age",
       x = "Age",
       y = "Count") +
  theme_minimal()

########################################################################### 

# Histogram of education
ggplot(reviews_new,aes(x = education)) +
  # Add a geom layer to specify the plot type
  geom_histogram(stat = 'count')

# Histogram of sales per product
ggplot(data = reviews_new, mapping = aes(x = product)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Sales per Product", x = "Product", y = "Frequency") +
  theme_minimal()

# visualise data with boxplot to determine normal 

# Boxplot using base R
boxplot(reviews_new$loyalty_points,
        col = "lightblue",         
        border = "black",           
        main = "Distribution of Total Loyalty Points",  
        ylab = "Loyalty Points",    
        xlab = "")                  

# Boxplot showing the distribution of loyalty points by gender
ggplot(data = reviews_new, mapping = aes(x = gender, y = loyalty_points)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Loyalty Points by Gender", x = "Gender", y = "Loyalty Points") +
  theme_minimal()

# Checks of distribution of other variables.
boxplot(reviews_new$age)

boxplot(reviews_new$remuneration)

boxplot(reviews_new$spending_score)

###################################################################################
# Looking at sales per product analysis by creating a proxy for sales using loyalty points

# Inspect the first few rows of the dataset
head(reviews_new)

# Check the structure of the dataset
str(reviews_new)

# Summary statistics for numeric columns
summary(reviews_new)

# Check for missing values
colSums(is.na(reviews_new))

# Convert categorical variables to factors
reviews_new$gender <- as.factor(reviews_new$gender)
reviews_new$education <- as.factor(reviews_new$education)
reviews_new$product <- as.factor(reviews_new$product)

# Create a sales summary dataset
sales_per_product <- reviews_new %>%
  group_by(product) %>%
  summarise(
    total_loyalty_points = sum(loyalty_points),
    average_age = mean(age),
    average_spending_score = mean(spending_score),
    count_reviews = n()
  )

# Inspect the aggregated sales data
head(sales_per_product)

# Count unique products [Not telling us much]
unique_products <- reviews_new %>%
  count(product)

# View the unique product counts [Not telling us much]
head(unique_products)

# Bar plot for total loyalty points per product [Not telling us much]
ggplot(sales_per_product, aes(x = reorder(product, -total_loyalty_points), y = total_loyalty_points)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Loyalty Points per Product",
       x = "Product",
       y = "Total Loyalty Points") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


################################################################################
# Analyse Reviews and Summaries for Top 5 Products based on loyalty points by product

# Step 1: Calculate total loyalty points per product
top_products <- reviews_new %>%
  group_by(product) %>%
  summarise(total_loyalty_points = sum(loyalty_points, na.rm = TRUE)) %>%
  arrange(desc(total_loyalty_points)) %>%
  slice(1:5) # Get top 5 products

# View the top 5 products
print(top_products)

# Step 2: Join the reviews and summaries to the top products
top_product_reviews <- top_products %>%
  left_join(reviews_new %>% select(product, review, summary), by = "product")

# View the resulting data frame with reviews and summaries
print(top_product_reviews)

View(top_product_reviews)

#############################################################################
# Summarise demographics

# Summary of demographics 
demographic_summary <- reviews_new %>%
  group_by(gender, education) %>%
  summarise(
    average_age = mean(age),
    total_loyalty_points = sum(loyalty_points),
    count_reviews = n()
  )

# View the demographic summary
head(demographic_summary)
as_tibble(demographic_summary)

demographic_summary <- reviews_new %>%
  group_by(gender, education) %>%
  summarise(
    average_age = mean(age, na.rm = TRUE),
    total_loyalty_points = sum(loyalty_points, na.rm = TRUE),
    count_reviews = n()
  )

# View the demographic summary
print(demographic_summary)

# Visualization of demographic summary

# Bar plot for total loyalty points by gender and education
ggplot(demographic_summary, aes(x = education, y = total_loyalty_points, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Loyalty Points by Gender and Education Level",
       x = "Education Level",
       y = "Total Loyalty Points") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Now including average age
demographic_summary <- reviews_new %>%
  group_by(gender, education) %>%
  summarise(
    average_age = mean(age, na.rm = TRUE),
    total_loyalty_points = sum(loyalty_points, na.rm = TRUE),
    review_count = n()
  )

# Plotting the demographic summary
ggplot(demographic_summary, aes(x = education, y = average_age, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Demographic Summary: Average Age by Gender and Education",
       x = "Education Level",
       y = "Average Age") +
  theme_minimal()


# Normalise by calculating the mean number of loyalty points

# Modify the demographic summary to include mean loyalty points
demographic_summary <- reviews_new %>%
  group_by(gender, education) %>%
  summarise(
    average_age = mean(age, na.rm = TRUE),
    total_loyalty_points = sum(loyalty_points, na.rm = TRUE),
    review_count = n(),
    mean_loyalty_points = total_loyalty_points / review_count  # Calculate mean loyalty points
  )

# Bar plot for mean loyalty points by gender and education
ggplot(demographic_summary, aes(x = education, y = mean_loyalty_points, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Loyalty Points by Gender and Education Level",
       x = "Education Level",
       y = "Mean Loyalty Points") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###########################################################################
# Further exploratory Data Analysis to look at correlation between variables

# Step 1: Summarize total loyalty points, mean loyalty points, average spending score, and average renumeration by education, gender, and cluster
demographic_summary <- reviews_new %>%
  group_by(education, gender, cluster) %>%
  summarize(
    total_loyalty_points = sum(loyalty_points, na.rm = TRUE),
    mean_loyalty_points = mean(loyalty_points, na.rm = TRUE),
    average_spending_score = mean(spending_score, na.rm = TRUE),
    average_remuneration = mean(remuneration, na.rm = TRUE),
    .groups = "drop" # to prevent group attributes in the output
  )

# Step 2: Assign descriptive names to clusters
demographic_summary <- demographic_summary %>%
  mutate(cluster_name = case_when(
    cluster == 0 ~ "High Spenders",
    cluster == 1 ~ "Moderate Earners",
    cluster == 2 ~ "Low Spenders, High Income",
    cluster == 3 ~ "Value Seekers",
    cluster == 4 ~ "Budget-Conscious",
    TRUE ~ NA_character_
  ))

# Check for NA values in the cluster_name column
na_count <- sum(is.na(demographic_summary$cluster_name))
print(paste("Number of NA values in cluster_name:", na_count))

# Step 3: Create bar plots
# Bar plot of total loyalty points
ggplot(demographic_summary, aes(x = education, y = total_loyalty_points, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Loyalty Points by Gender, Education Level, and Cluster Name",
       x = "Education Level",
       y = "Total Loyalty Points") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ cluster_name)

# Bar plot of mean loyalty points
ggplot(demographic_summary, aes(x = education, y = mean_loyalty_points, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Loyalty Points by Gender, Education Level, and Cluster Name",
       x = "Education Level",
       y = "Mean Loyalty Points") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ cluster_name)

# Bar plot of average spending score
ggplot(demographic_summary, aes(x = education, y = average_spending_score, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Spending Score by Gender, Education Level, and Cluster Name",
       x = "Education Level",
       y = "Average Spending Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ cluster_name)

# Bar plot of average remuneration
ggplot(demographic_summary, aes(x = education, y = average_remuneration, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Remuneration by Gender, Education Level, and Cluster Name",
       x = "Education Level",
       y = "Average Remuneration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ cluster_name)

# Step 4: Combine plots with facets for total and mean loyalty points, spending score, and renumeration
demographic_summary_long <- demographic_summary %>%
  pivot_longer(cols = c(total_loyalty_points, mean_loyalty_points, average_spending_score, average_remuneration),
               names_to = "metric",
               values_to = "value")

ggplot(demographic_summary_long, aes(x = education, y = value, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Loyalty Points, Spending Score, and Remuneration by Education, Gender, and Cluster Name",
       x = "Education Level",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(metric ~ cluster_name, scales = "free_y")

# Step 5: Create age bins and summarize by age group and cluster
# Create age bins using the `cut()` function
reviews_new <- reviews_new %>%
  mutate(age_group = cut(age, breaks = c(0, 25, 35, 45, 60, Inf), right = FALSE,
                         labels = c("<=24", "25-34", "35-44", "45-59", "60+")))

# Summarize mean loyalty points, average spending score, average remuneration by age, gender, and cluster
demographic_summary_age <- reviews_new %>%
  group_by(age_group, gender, cluster) %>%
  summarize(
    mean_loyalty_points = mean(loyalty_points, na.rm = TRUE),
    average_spending_score = mean(spending_score, na.rm = TRUE),
    average_remuneration = mean(remuneration, na.rm = TRUE),
    .groups = "drop"
  )

# Add cluster names to the summary
demographic_summary_age <- demographic_summary_age %>%
  mutate(cluster_name = case_when(
    cluster == 0 ~ "High Spenders",
    cluster == 1 ~ "Moderate Earners",
    cluster == 2 ~ "Low Spenders, High Income",
    cluster == 3 ~ "Value Seekers",
    cluster == 4 ~ "Budget-Conscious",
    TRUE ~ NA_character_
  ))

# Now pivot the data to long format including cluster_name
demographic_summary_age_long <- demographic_summary_age %>%
  pivot_longer(cols = c(mean_loyalty_points, average_spending_score, average_remuneration),
               names_to = "metric",
               values_to = "value")

# Create the combined plot for age group and cluster name
ggplot(demographic_summary_age_long, aes(x = age_group, y = value, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Loyalty Points, Spending Score, and Remuneration by Age Group, Gender and Cluster Name",
       x = "Age Group",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(metric ~ cluster_name, scales = "free_y")

View(demographic_summary_age_long)

# Step 6: Create a summary matrix table showing the difference metrics of each cluster
cluster_summary <- reviews_new %>%
  group_by(cluster) %>%
  summarize(
    avg_spending_score = round(mean(spending_score, na.rm = TRUE), 2),
    avg_remuneration = round(mean(remuneration, na.rm = TRUE), 2),
    number_of_customers = n(),
    total_loyalty_points = sum(loyalty_points, na.rm = TRUE),
    avg_loyalty_points = round(mean(loyalty_points, na.rm = TRUE), 0),
    avg_age = round(mean(age, na.rm = TRUE), 0),
    .groups = "drop"
  )

# Add cluster names to the summary
cluster_summary <- cluster_summary %>%
  mutate(cluster_name = case_when(
    cluster == 0 ~ "High Spenders",
    cluster == 1 ~ "Moderate Earners",
    cluster == 2 ~ "Low Spenders, High Income",
    cluster == 3 ~ "Value Seekers",
    cluster == 4 ~ "Budget-Conscious",
    TRUE ~ NA_character_
  ))

# View the summary data
print(cluster_summary)
View(cluster_summary)

############################################################################
# Investigate the outliers further
# Step 1 - visualise the distribution

# Histogram and density plot for loyalty_points
hist(reviews_new$loyalty_points, main= "Loyalty Points Distribution", xlab = "Loyalty Points", col = "skyblue", breaks = 30)
plot(density(reviews_new$loyalty_points), main = "Loyalty Points Density", col = "red")

# Repeat for other variables like renumeration, spending_score, and age
hist(reviews_new$renumeration, main = "Renumeration Distribution", xlab = "Renumeration", col = "lightgreen", breaks = 30)
plot(density(reviews_new$renumeration), main = "Renumeration Density", col = "green")

# Boxplot for loyalty_points
boxplot(reviews_new$loyalty_points, main = "Boxplot of Loyalty Points", col = "lightblue")

# Boxplot for other variables
boxplot(reviews_new$renumeration, main = "Boxplot of Renumeration", col = "lightgreen")
boxplot(reviews_new$spending_score, main = "Boxplot of Spending score", col = "blue")
boxplot(reviews_new$age, main = "Boxplot of Age", col = "pink")

# Step 2 - Idenitfy outliers
# Calculate the IQR for loyalty_points
iqr_loyalty_points <- IQR(reviews_new$loyalty_points)
upper_bound <- quantile(reviews_new$loyalty_points, 0.75) + 1.5 * iqr_loyalty_points
lower_bound <- quantile(reviews_new$loyalty_points, 0.25) - 1.5 * iqr_loyalty_points

# Identify outliers
outliers <- reviews_new$loyalty_points[reviews_new$loyalty_points > upper_bound | reviews_new$loyalty_points < lower_bound]

# Print outliers
print(outliers)

# Step 3: investigate the extreme values
# Subset the data to focus on outliers
outlier_data <- reviews_new[reviews_new$loyalty_points > upper_bound | reviews_new$loyalty_points < lower_bound, ]

# Check if outliers are driven by renumeration, spending score, or age
summary(outlier_data)


###############################################################################
#SECTION 2 - STATISTICAL ANALYSIS AND MULTIPLER LINEAR REGRESSION
###############################################################################

################################################################################

# 3. # Perform a statistical analysis 
# comment on the descriptive statistics in the context of the review of how customers accumulate loyalty points.

# Load the data
# Read the CSV file (reviews_new.csv).
reviews_new <- read.csv('reviews_new.csv', header=TRUE)

# view the data
head(reviews_new)
as_tibble(reviews_new)

# we want to focus on how customers accumulate loyalty points 
# therefore going to focus on the loyalty points variable 

# Compute descriptive statistics

# what is the average number of loyalty points for a customer

mean(reviews_new$loyalty_points)

summary(reviews_new$loyalty_points)
summary(reviews_new)

# range = Max - min
max(reviews_new$loyalty_points) - min(reviews_new$loyalty_points)

# calculate IQR
IQR(reviews_new$loyalty_points)

# Measures of dispersion
var(reviews_new$loyalty_points)

# return the standard deviation
sd(reviews_new$loyalty_points)


# Distribution of the data
# specify box plot function
boxplot(reviews_new$loyalty_points)

# specify histogram function

hist(reviews_new$loyalty_points)

quantile(reviews_new$loyalty_points,0.25)

# specify qqnorm function(draw a qqplot) 
qqnorm(reviews_new$loyalty_points)

# specify qqline function
qqline(reviews_new$loyalty_points)

# Conduct a shapiro-wilk test 
shapiro.test(reviews_new$loyalty_points)


# Skewness and kurtosis

# specify the skewness and kurtosis functions
skewness(reviews_new$loyalty_points)
kurtosis(reviews_new$loyalty_points)

colnames(reviews_new)

 ###################################################################################
# 4. Create a multiple linear regression model 
# using selected numeric features

# Model a - loyalty points vs remuneration and spending score
modela = lm(loyalty_points~remuneration+spending_score, data=reviews_new)

# print the summary statistics
summary(modela)

# Add new variables.
modelb = lm(loyalty_points~remuneration+spending_score+age+product, data=reviews_new)

# Change the model name.
summary(modelb)

# New model.
modelc = lm(loyalty_points~remuneration+spending_score+age, data=reviews_new)

summary(modelc)


############################################################################
# Check for multicollinearity

# Check multicollinearity using VIF
model_vif <- lm(loyalty_points ~ age + remuneration + spending_score + gender + education + product, data = reviews_new)
vif(model_vif)

############################################################################
# Test the model

# Load the new data file (reviews_new.csv), and view its structure.
loyaltytest <- read.csv('reviews_new.csv', header=TRUE)

# View the data.
View(loyaltytest)

# Create a new object and specify the predict function.
predictTest = predict(modelc, newdata=loyaltytest,
                      interval='confidence')
# Print the object.
predictTest 


# Plot residuals
plot(modelc)

# QQ-plot for normality check
qqnorm(residuals(modelc))
qqline(residuals(modelc))

############################################################################
# Predict loyalty points for new data for new customer Lina (see presentation)
new_data <- data.frame(age = c(36), renumeration = c(74), 
                       spending_score = c(82))

predictions <- predict(modelc, new_data)
print(predictions)

############################################################################
# Normalise the data to see if it creates a more normal distbrution and improves
# performance of model C
# Apply the log transformation to model c

# Apply log transformation to loyalty points (add a small constant if necessary to avoid log(0))
reviews_new$log_loyalty_points <- log(reviews_new$loyalty_points + 1)

# Fit the linear regression model with the log-transformed loyalty points
model_logc <- lm(log_loyalty_points ~ renumeration + spending_score + age, data = reviews_new)

# Display the summary of the new model
summary(model_logc)

# Apply the log transformation to all data
# Create a new column for log of loyalty points
reviews_new$log_loyalty_points <- log(reviews_new$loyalty_points)

# Run the linear model using log-transformed loyalty points
model_log <- lm(log_loyalty_points ~ renumeration + spending_score + age + product, data = reviews_new)

# Print the summary of the model
summary(model_log)

######################################################################
# Scaling the model

# Normalize remuneration and spending score using z-score standardization
reviews_new <- reviews_new %>%
  mutate(
    renumeration_scaled = scale(renumeration),
    spending_score_scaled = scale(spending_score)
  )

# Fit the regression model using scaled variables
model_scaled <- lm(log_loyalty_points ~ renumeration_scaled + spending_score_scaled + age + product, data = reviews_new)

# Check the model summary
summary(model_scaled)





