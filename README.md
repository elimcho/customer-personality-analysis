# customer-personality-analysis
Analyzes customer personality and purchasing behavior to improve marketing strategies. Examines demographics, income, and engagement to identify high-value segments, enabling targeted campaigns that increase efficiency, response rates, and customer retention.
# Problem:
The aim is to analyze customer personality and purchasing behavior data to help businesses optimize their marketing strategies and customer targeting. Specifically, to determine how customer demographics, income, household characteristics and engagement behaviors influence spending patterns and responses to marketing campaigns. By identifying which customer segments are more likely to respond to promotions and which products they prefer, a business can transition from generic promotions to targeted, data driven campaigns that improve marketing efficiency and customer retention.
# Business Question: 
How do customer demographics and behavioral factors influence purchasing behavior, and can we predict whether a customer will respond to a marketing campaign?
# Dataset:
 We will be using the Customer Personality Dataset (sourced from Kaggle). This dataset contains transactional records including:
Demographics: Birth Year, Education, Marital Status, Income, Kids, Teens
Customer Behavior: Recency (days since last purchase), Customer enrollment date, Complain
Spending Patterns: Amount spent across multiple product categories (Wines, Fruits, Meat, Fish, Sweets, Gold products)
Promotion and Campaign Data
Purchase Channels
Target Variable: Response (whether the customer accepted the most recent campaign). 
# Proposed Analytics Methods:
Multiple Linear Regression: To quantify the relationship between demographic and behavioral variables (income, household composition, recency, purchase channels) and customer spending across product categories. This regression identifies the key factors that drive customer spending. (“Customers with higher income spend $X more on average.”)
Logistic Regression: To predict whether a customer will accept a marketing campaign using the Response variable. This model identifies which customers are most likely to respond to promotions and helps businesses target high-probability segments.
K-Means Clustering: To segment the customer base into distinct persona groups (e.g., “High-Income Frequent Buyers” vs. “Discount-Sensitive Families”) based on spending behavior, income, and engagement metrics. This method identifies meaningful customer segments for specialized marketing strategies.
GBM: To extend the predictive analysis, a Gradient Boosting Machine (GBM) model was applied to predict whether a customer would respond to the most recent marketing campaign. GBM is a machine learning technique that builds a sequence of decision trees, where each new tree attempts to reduce the errors made by the previous ones. This allows the model to capture more complex patterns, nonlinear effects, and variable interactions than traditional logistic regression. 
