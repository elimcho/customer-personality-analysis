#BTMA 531 Final Project
#Customer Personality Analysis
#Group 3 - Elim Cho, Imaan Bandali, Ishita Udasi, Nayab Noor

#setwd("~/Desktop/BTMA 531/Final Project")
#load library
library(lubridate)

#load data
df <- read.csv("BTMA 531 Final project_Group 3_marketing_campaign_data.csv", sep = "\t" )

#Data Cleanup
# Remove NAs in Income and remove duplicates
df <- df[!is.na(df$Income), ]
df <- unique(df)

# Convert character string to Date object
df$Dt_Customer <- dmy(df$Dt_Customer)

#customer Age (Reference year 2014)
df$Age <- 2014 - df$Year_Birth

#customer Tenure in days
df$Customer_Tenure_Days <- as.numeric(as.Date("2014-12-31") - df$Dt_Customer)

#adding up the spending columns
spending_cols <- c("MntWines", "MntFruits", "MntMeatProducts", 
                   "MntFishProducts", "MntSweetProducts", "MntGoldProds")
df$Total_Spent <- rowSums(df[, spending_cols])

#children variable
df$Children <- df$Kidhome + df$Teenhome

#total purchases
df$Total_Purchases <- df$NumWebPurchases + df$NumCatalogPurchases + df$NumStorePurchases

#Simplifying marital status
df$Marital_Status <- ifelse(df$Marital_Status %in% c("Married","Together"), "Partnered",
                     ifelse(df$Marital_Status %in% c("Single","Divorced","Widow","Alone"), "Single", "Other"))

#Convert variables to factor
df$Marital_Status <- as.factor(df$Marital_Status)
df$Education <- as.factor(df$Education)
df$Response <- as.factor(df$Response)
df$Complain <- as.factor(df$Complain)

#Outlier filtering
df <- df[df$Age < 100 & df$Income < 200000, ]

#Dropping unnecessary columns
df <- df[, !(names(df) %in% c("Z_CostContact", "Z_Revenue", "Year_Birth"))]


#EDA
#load libraries
library(ggplot2)
library(dplyr)
library(tidyr)

str(df)
summary(df)
dim(df)
names(df)
colSums(is.na(df))

#age distribution
ggplot(df, aes(x = Age)) + geom_histogram(bins = 30) +
labs(title = "Age Distribution of Customers", x = "Age", y = "Number of Customers")

#income distribution
ggplot(df, aes(x = Income)) + geom_histogram(bins = 30) +
labs(title = "Income Distribution", x = "Income", y = "Number of Customers")

#total spending distribution
ggplot(df, aes(x = Total_Spent)) + geom_histogram(bins = 30) +
labs(title = "Total Customer Spending", x = "Total Spending", y = "Number of Customers")

#Customer income vs spending
ggplot(df, aes(x = Income, y = Total_Spent)) + geom_point(alpha = 0.5) +
labs(title = "Income vs Total Spending", x = "Income", y = "Total Spending")

#Average spending by product category
spending_summary <- df %>% summarise(
Wines = mean(MntWines),
Fruits = mean(MntFruits),
Meat = mean(MntMeatProducts),
Fish = mean(MntFishProducts),
Sweets = mean(MntSweetProducts),
Gold = mean(MntGoldProds))

spending_long <- pivot_longer(spending_summary, cols = everything(), names_to = "Category", values_to = "Average_Spending")

ggplot(spending_long, aes(x = Category, y = Average_Spending)) + geom_bar(stat = "identity") +
labs(title = "Average Spending by Product Category", x = "Product Category", y = "Average Spending")

#campaign response distribution
ggplot(df, aes(x = Response)) + geom_bar() +
labs(title = "Campaign Response Distribution", x = "Response", y = "Count")

#correlation matrix
numeric_data <- df %>% select(where(is.numeric))
cor_matrix <- cor(numeric_data)
cor_df <- as.data.frame(as.table(cor_matrix))
ggplot(cor_df, aes(Var1, Var2, fill = Freq)) + geom_tile() +
labs(title = "Correlation Heatmap", x = "", y = "", fill = "Correlation") +
theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Multiple linear regression
# This model analyzes how demographic and 
# behavioral variables influence total customer spending.

# Fit regression model predicting Total Spending
fit1 = lm(df$Total_Spent ~ df$Income + df$Age + df$Children + df$Recency +
            df$Customer_Tenure_Days + df$NumWebVisitsMonth +
            df$Total_Purchases + df$Marital_Status + df$Education)

# View model
fit1

# The coefficients represent the estimated effect of each variable on Total_Spent.
# For example:
# - Income has a positive coefficient (~0.0118), meaning higher income customers tend to spend more.
# - Children has a negative coefficient (~ -152), suggesting households with more 
# children spend less on these products.
# - Total_Purchases has a strong positive coefficient (~36), meaning customers 
# who purchase more frequently spend significantly more overall.

# Model summary (coefficients, p-values, R-squared)
summary(fit1)

# Interpretation of key results:
# - Variables with p-values < 0.05 are statistically significant predictors of spending.
# - Significant predictors include:
#   Income, Age, Children, Customer_Tenure_Days, Total_Purchases, and several Education levels.
# - Income has a very strong positive effect on spending (p < 0.001).
# - Total_Purchases is also highly significant and strongly increases total spending.
# - Children has a strong negative relationship with spending.
# - Recency and NumWebVisitsMonth are not statistically significant in this model.

# Model fit statistics:
# Multiple R-squared = 0.791
# This means approximately 79% of the variation in customer spending is explained by the predictors.
# Adjusted R-squared = 0.790
# This accounts for the number of predictors and shows the model still explains about 79% of the variance,
# indicating a strong overall model.

# Residual plots (checking assumptions)
par(mfrow=c(1,2))

plot(predict(fit1), residuals(fit1),
     xlab="Predicted Values", ylab="Residuals", main="Residual Plot")

# The residual plot checks the assumptions of linearity and constant variance.
# Ideally, residuals should be randomly scattered around zero.
# In this plot, residuals are mostly centered around zero but show a slight funnel/curved pattern.
# The residual plot suggests some mild heteroscedasticity,
# as the spread of residuals increases slightly at higher predicted values.
# However, the residuals remain centered around zero, indicating the model
# still provides a reasonable fit.

plot(predict(fit1), rstandard(fit1),
     xlab="Predicted Values", ylab="Standardized Residuals", main="Standardized Residuals")

# Standardized residuals help detect outliers.
# Most observations fall within the typical range of -3 to +3.
# A few points fall slightly outside this range, indicating some potential outliers,
# which is common in large real-world datasets.

# Normality of residuals
par(mfrow=c(1,1))
qqnorm(rstandard(fit1), ylab="Standardized Residuals", xlab="Normal Scores")

qqline(rstandard(fit1))

# The Q-Q plot checks whether residuals follow a normal distribution.
# Most points lie close to the reference line in the middle of the distribution,
# indicating approximate normality.
# Some deviations appear at the extreme ends, suggesting a few outliers or skewed observations,
# which is typical in consumer spending data.

# R-squared values
summary(fit1)$r.squared
summary(fit1)$adj.r.squared
# R-squared = 0.791 
# The model explains about 79% of variation in total spending.
# Adjusted R-squared = 0.790 
# The model remains strong even after adjusting for number of predictors.

# with interaction terms 
# This tests if the effect of income on spending changes based on education level
fit_interaction = lm(Total_Spent ~ Income * Education, data = df)
summary(fit_interaction)

# This model confirms that the impact of income on spending is not uniform across all education levels.

# Positive interaction coefficients for Income:EducationGraduation and
# Income:EducationMaster suggest that for customers with these education levels,
# increases in income are associated with greater increases in spending
# compared to the reference education category.

# Income remains the most powerful predictor (p < 2e-16). Even without considering education,
# higher income leads to higher spending across the board.

# The Adjusted R-squared for this interaction model is 0.633.
# This is lower than the 0.790 achieved in the first model (fit1).
# While the interaction is statistically significant, including behavioral factors like
# Total_Purchases and household factors like Children provides a much more complete
# explanation of customer spending.

# The F-statistic tests whether the regression model provides a better fit
# than a model with no predictors.
# The extremely small p-value (< 2.2e-16) indicates that the model as a whole
# is statistically significant and that the predictors jointly explain
# variation in customer spending.

# Businesses should target high-income customers with advanced degrees for premium
# marketing campaigns. This segment shows the highest marginal propensity to spend
# as their wealth increases.

#K - means clustering
# We use: Income, Age, Tenure, Total_Spent, Total_Purchases, Recency, and Children 
cluster_vars <- df[, c("Income", "Age", "Customer_Tenure_Days", 
                       "Total_Spent", "Total_Purchases", "Recency", "Children")]

#scaling the data. 
cluster_scaled <- scale(cluster_vars)

#Finding the Elbow
set.seed(123) 
wss <- sapply(1:10,function(k) kmeans(cluster_scaled, centers=k,nstart=20)$tot.withinss)

#Plotting 
plot(1:10,wss,type="b", pch=19, frame=FALSE,xlab="Number of Clusters K",
     ylab="Total Within-Cluster Sum of Squares", main="Elbow Method for Optimal K")

#To determine the optimal number of customer segments, we utilized the Elbow Method. 
#As shown in the plot, the ‘Total Within-Cluster Sum of Squares’ decreases significantly as we move to 3 and 4 clusters. 
#We selected K=4 as the optimal point because itallows us to define four distinct and actionable customer personas.

#Run the final k-means with 4 clusters
# nstart = 25 runs the model 25 times and picks the best one.
final_km <- kmeans(cluster_scaled,centers=4,nstart=25)

#Attach the cluster ID back to our original 'df'
df$Cluster <- as.factor(final_km$cluster)

#Creating the Summary Table 
install.packages("dplyr")
library(dplyr)
persona_table <- df %>%
  group_by(Cluster) %>%  
  summarise(
    Count = n(),
    Avg_Income = round(mean(Income), 0),
    Avg_Spent = round(mean(Total_Spent), 0),
    Avg_Children = round(mean(Children), 1),
    Campaign_Success = round(mean(as.numeric(as.character(Response))) * 100, 1))
print(persona_table)
print(persona_table,n=Inf, width=Inf)

#Logistic regression

# Convert Response to numeric 0/1
df$Response <- as.numeric(as.character(df$Response))

#Check class balance
table(df$Response)

#Interpretation:
# In our results, most customers are 0 (non-responders) and
# fewer customers are 1 (responders).
# This tells us the dataset is imbalanced, which is important
# because accuracy alone may look high even if the model is
# not very good at finding responders.

#convert the counts into proportions.
prop.table(table(df$Response))

# Interpretation:
# About 84.95% of customers did not respond, while about
# 15.05% did respond.
# Since responders are the minority class, the model may
# naturally predict more 0s than 1s.

# Fit the logistic regression model

# This model predicts the probability of campaign response
# using customer income, age, children, recency, tenure,
# website engagement, purchases, marital status, education,
# and complaint history.

logit_fit <- glm(Response ~ Income + Age + Children + Recency +
                   Customer_Tenure_Days + NumWebVisitsMonth +
                   Total_Purchases + Marital_Status + Education +
                   Complain,
                 data = df,
                 family = binomial)

#View model summary

summary(logit_fit)

# Based on our results:
# Significant positive predictors:
# - Income
# - Customer_Tenure_Days
# - NumWebVisitsMonth
# - EducationPhD
#
# Significant negative predictors:
# - Children
# - Recency
#
# Not statistically significant:
# - Total_Purchases
# - Marital_Status variables
# - Complain
# - Some education categories
#
# Interpretation of key findings:
# - Higher income customers are more likely to respond.
# - Customers with more children are less likely to respond.
# - Customers who purchased more recently are more likely to respond.
# - Customers with longer tenure are more likely to respond.
# - Customers with more web visits per month are more likely to respond.
# - Customers with a PhD have higher odds of responding than the
#   reference education group.


#Convert coefficients to odds ratios

odds_ratios <- exp(coef(logit_fit))
odds_ratios

# - Income OR = 1.000031
#   This looks tiny because Income is measured per $1.
#   So each additional $1 slightly increases the odds of response.
#
# - Children OR = 0.472
#   Each additional child reduces the odds of response.
#
# - Recency OR = 0.976
#   Each extra day since last purchase lowers the odds of response.
#
# - NumWebVisitsMonth OR = 1.220
#   Each extra website visit per month increases the odds of response
#   by about 22%.
#
# - EducationPhD OR = 2.534
#   Customers with a PhD have about 2.53 times higher odds of response
#   than the reference education category.

#Predicted probabilities
pred_prob <- predict(logit_fit, type = "response")
head(pred_prob)

#Convert probabilities into predicted classes
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

#Confusion matrix
cm <- table(Predicted = factor(pred_class, levels = c(0,1)),
            Actual = factor(df$Response, levels = c(0,1)))
cm

#results:
# - True Negatives = 1831
# - False Negatives = 266
# - False Positives = 48
# - True Positives = 67
# Interpretation:
# The model is very good at identifying non-responders,
# but it misses many actual responders.

#Accuracy
accuracy <- sum(diag(cm)) / sum(cm)
accuracy

# result:
# Accuracy = 0.858, or 85.8%
# Interpretation:
# The model correctly classified about 85.8% of all customers.

# Sensitivity and specificity

sensitivity <- cm[2,2] / sum(cm[,2])
specificity <- cm[1,1] / sum(cm[,1])

sensitivity
specificity

# Sensitivity:
# Our result:
# Sensitivity = 0.201, or 20.1%
# Interpretation:
# The model only correctly identifies about 20.1% of customers
# who actually responded.
# So it is weak at catching responders at the 0.5 cutoff.

# Specificity:
# Measures how well the model identifies actual non-responders.
# Our result:
# Specificity = 0.974, or 97.4%
# Interpretation:
# The model correctly identifies about 97.4% of customers who
# did not respond.
# So it performs very well on the majority class.

#ROC Curve and AUC
#install.packages("pROC")
library(pROC)

roc_obj <- roc(df$Response, pred_prob)
plot(roc_obj, main = "ROC Curve for Logistic Regression")
auc(roc_obj)

# Our result:
# AUC = 0.8148
# Interpretation:
# The model has good overall ability to distinguish responders
# from non-responders.
# Even though sensitivity is low at the 0.5 cutoff, the AUC shows
# that the model still ranks customers reasonably well by response likelihood.

# Final overall interpretation
# The logistic regression model identifies meaningful drivers of
# campaign response.
#
# Customers are more likely to respond when they:
# - have higher income
# - have longer customer tenure
# - visit the website more often
# - belong to the PhD education category
#
# Customers are less likely to respond when they:
# - have more children
# - have not purchased recently
#
# Performance-wise:
# - Accuracy is high (85.8%), but this is partly due to class imbalance.
# - Sensitivity is low (20.1%), so the model misses many responders.
# - Specificity is very high (97.4%), so it is strong at identifying
#   non-responders.
# - AUC is 0.8148, showing good overall discrimination.
#
# Business implication:
# The model is useful for ranking customers by likelihood of response
# and can support targeted marketing campaigns, especially toward
# higher-income, more engaged, and longer-tenure customers.

#GBM 

# Train/Test Split for Model Comparison

set.seed(123)

# Make sure Response is numeric 0/1
df$Response <- as.numeric(as.character(df$Response))

# Create 70/30 split
n <- nrow(df)
GBM_training <- sample(1:n, size = 0.7 * n)

GBM_train_data <- df[GBM_training, ]
GBM_test_data  <- df[-GBM_training, ]

# Check sizes
nrow(GBM_train_data)
nrow(GBM_test_data)

# Logistic Regression on the Same Training Set

logit_fit_split <- glm(Response ~ Income + Age + Children + Recency +
                         Customer_Tenure_Days + NumWebVisitsMonth +
                         Total_Purchases + Marital_Status + Education +
                         Complain,
                       data = GBM_train_data,
                       family = binomial)

summary(logit_fit_split)

# Logistic Regression Predictions and Performance

# Predicted probabilities on test set
logit_prob <- predict(logit_fit_split, newdata = GBM_test_data, type = "response")

# Convert probabilities into class predictions using 0.5 cutoff
logit_class <- ifelse(logit_prob > 0.5, 1, 0)

# Confusion matrix
logit_cm <- table(Predicted = factor(logit_class, levels = c(0,1)),
                  Actual = factor(GBM_test_data$Response, levels = c(0,1)))
logit_cm

# Accuracy
logit_accuracy <- sum(diag(logit_cm)) / sum(logit_cm)
logit_accuracy

# Sensitivity and specificity
logit_sensitivity <- logit_cm[2,2] / sum(logit_cm[,2])
logit_specificity <- logit_cm[1,1] / sum(logit_cm[,1])

logit_sensitivity
logit_specificity

# GBM Model

# install.packages("gbm")
library(gbm)

gbm_fit <- gbm(
  formula = Response ~ Income + Age + Children + Recency +
    Customer_Tenure_Days + NumWebVisitsMonth +
    Total_Purchases + Marital_Status + Education +
    Complain,
  distribution = "bernoulli",
  data = GBM_train_data,
  n.trees = 1000,
  interaction.depth = 2,
  shrinkage = 0.01,
  n.minobsinnode = 10,
  cv.folds = 5,
  verbose = FALSE
)

summary(gbm_fit)
# Get variable importance values
gbm_importance <- summary(gbm_fit, plotit = FALSE)

# View table
gbm_importance

par(mar = c(5, 10, 4, 2))
barplot(
  rev(gbm_importance$rel.inf),
  names.arg = rev(gbm_importance$var),
  horiz = TRUE,
  las = 1,
  xlab = "Relative Influence",
  main = "GBM Variable Importance"
)

# Best Number of Trees

best_iter <- gbm.perf(gbm_fit, method = "cv")
best_iter

# If gbm.perf causes an issue, use this instead:
# best_iter <- 1000

# GBM Predictions and Performance

# Predicted probabilities on test set
gbm_prob <- predict(gbm_fit, newdata = GBM_test_data, n.trees = best_iter, type = "response")

# Convert probabilities into class predictions using 0.5 cutoff
gbm_class <- ifelse(gbm_prob > 0.5, 1, 0)

# Confusion matrix
gbm_cm <- table(Predicted = factor(gbm_class, levels = c(0,1)),
                Actual = factor(GBM_test_data$Response, levels = c(0,1)))
gbm_cm

# Accuracy
gbm_accuracy <- sum(diag(gbm_cm)) / sum(gbm_cm)
gbm_accuracy

# Sensitivity and specificity
gbm_sensitivity <- gbm_cm[2,2] / sum(gbm_cm[,2])
gbm_specificity <- gbm_cm[1,1] / sum(gbm_cm[,1])

gbm_sensitivity
gbm_specificity

# ROC Curve and AUC Comparison

# install.packages("pROC")
library(pROC)

# Logistic regression ROC and AUC
logit_roc <- roc(GBM_test_data$Response, logit_prob)
logit_auc <- auc(logit_roc)

# GBM ROC and AUC
gbm_roc <- roc(GBM_test_data$Response, gbm_prob)
gbm_auc <- auc(gbm_roc)

logit_auc
gbm_auc

# Plot ROC Curves Together

plot(logit_roc, main = "ROC Curve Comparison: Logistic Regression vs GBM")
plot(gbm_roc, add = TRUE, lty = 2)

legend("bottomright",
       legend = c(
         paste("Logistic Regression AUC =", round(as.numeric(logit_auc), 4)),
         paste("GBM AUC =", round(as.numeric(gbm_auc), 4))
       ),
       lty = c(1,2),
       bty = "n")

# Comparison Table

comparison_table <- data.frame(
  Model = c("Logistic Regression", "GBM"),
  Accuracy = round(c(logit_accuracy, gbm_accuracy), 4),
  Sensitivity = round(c(logit_sensitivity, gbm_sensitivity), 4),
  Specificity = round(c(logit_specificity, gbm_specificity), 4),
  AUC = round(c(as.numeric(logit_auc), as.numeric(gbm_auc)), 4)
)

print(comparison_table, row.names = FALSE)



