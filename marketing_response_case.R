library(tidyverse)


marketing_response <- read_csv('https://www.dropbox.com/scl/fi/o2s8m4pe0wn0huyj9y3eb/14_train.csv?rlkey=hx03ffyaj9j6p9rf35k71s0uy&dl=1')


# A. Dependent Variable Characteristics


library(tidyverse)
library(ggplot2)

marketing_response <- read_csv('https://www.dropbox.com/scl/fi/o2s8m4pe0wn0huyj9y3eb/14_train.csv?rlkey=hx03ffyaj9j6p9rf35k71s0uy&dl=1')

# Graph 1

custom_colors <- c("#FF5733", "#FFBD33", "#33FF57", "#337CFF", "#B933FF")

response_1_rows <- marketing_response[marketing_response$Response == 1, ]

accepted_cmp_columns <- grep("^AcceptedCmp", names(response_1_rows), value = T)

accepted_cmp_counts <- numeric(length(accepted_cmp_columns))

for (i in seq_along(accepted_cmp_columns)) {
  accepted_cmp_counts[i] <- sum(response_1_rows[[accepted_cmp_columns[i]]] == 1, na.rm = T)
}

barplot(accepted_cmp_counts, names.arg = accepted_cmp_columns, 
        xlab = "AcceptedCmp Columns", ylab = "Frequency",
        main = "Distribution of AcceptedCmp* when Response = 1",
        col = custom_colors)



# Graph 2

campaign_columns <- grep("^AcceptedCmp", names(marketing_response), value = T)

campaign_counts <- sapply(campaign_columns, function(col) {
  sum(marketing_response[[col]] == 1, na.rm = T)
})

campaign_data <- data.frame(
  Campaign = paste("Campaign", 1:5),
  Customers = campaign_counts
)

ggplot(campaign_data, aes(x = Campaign, y = Customers)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Campaign offers accepted",
       x = "Campaign",
       y = "Number of Customers") +
  scale_fill_manual(values = custom_colors)
theme_minimal()



# Graph 3

response_counts <- table(ifelse(marketing_response$Response == 1, "Yes", "No"))

response_percentages <- round(100 * response_counts / sum(response_counts), 1)

pie(response_counts, 
    main = "Response Distribution",
    labels = paste(c("No", "Yes"), "\n", "Count:", response_counts, "\n", "Percent:", response_percentages, "%"),
    col = c("skyblue", "salmon"),
    clockwise = TRUE)











# B. High-Level Summary of Variables

# look at NAs accross the dataset
NAS <- marketing_response %>% 
  summarise(across(everything(), ~ sum(is.na(.))))

marketing_response %>%
  select(Education, Marital_Status, AcceptedCmp1:AcceptedCmp5, Complain, Response) %>%
  gather(key = "Variable", value = "Value") %>%
  group_by(Variable, Value) %>%
  count() %>%
  ungroup() %>%
  mutate(Variable = factor(Variable, levels = c("Education", "Marital_Status", "AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5", "Complain", "Response")))

boolean_vars <- c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5", "Complain", "Response")

marketing_response %>%
  select(-Education, -Marital_Status, -Dt_Customer, -Year_Birth, -boolean_vars) %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Value)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Histograms of Numeric Variables - excluding booleans")

marketing_response %>%
  select(Income, NumCatalogPurchases, NumDealsPurchases, NumWebPurchases, NumWebVisitsMonth) %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Value)) +
  geom_boxplot(binwidth = 10) +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "BoxPlot of Odd Distribution of outliers (Numeric Variables)")

marketing_response %>%
  select(Education, Marital_Status, Dt_Customer, Year_Birth) %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Value)) +
  geom_bar() +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Count Plots of Categorical Variables") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

marketing_response %>% 
  select(Dt_Customer)

marketing_response %>%
  select(all_of(boolean_vars)) %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = as.factor(Value), color = as.factor(Value), fill = as.factor(Value))) +
  geom_bar() +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Bar Plots of Boolean Variables")

marketing_response %>% 
  count(Z_CostContact)

marketing_response %>% 
  count(Z_Revenue)

marketing_response %>% 
  select(Z_Revenue)

marketing_response %>% 
  count(Dt_Customer)


















# C. Analysis of Independent Variables

# Load necessary libraries
library(tidyverse)
library(readr)
library(lubridate)
library(ggplot2)

# Load the dataset
marketing_response <- read_csv('https://www.dropbox.com/scl/fi/o2s8m4pe0wn0huyj9y3eb/14_train.csv?rlkey=hx03ffyaj9j6p9rf35k71s0uy&dl=1')

# Convert Dt_Customer to Date format
marketing_response$Dt_Customer <- as.Date(marketing_response$Dt_Customer, format="%m/%d/%Y")

# Ensure that Response is numeric
marketing_response$Response <- as.numeric(as.character(marketing_response$Response))

# Adjusting Marital_Status values
marketing_response$Marital_Status <- recode(marketing_response$Marital_Status,
                                            "Absurd" = "Other", 
                                            "YOLO" = "Other", 
                                            "Together" = "Married", 
                                            "Alone" = "Single", 
                                            "Divorced" = "Single", 
                                            "Widow" = "Single")

# Extract Dt_Customer_Year as a numeric column
marketing_response$Dt_Customer_Year <- year(marketing_response$Dt_Customer)

# Binning for Year_Birth with updated breaks
marketing_response$Binned_Year_Birth <- cut(marketing_response$Year_Birth,
                                            breaks = c(1893, 1919, 1945, 1970, 1996), 
                                            include.lowest = TRUE,
                                            labels = c("Very Old (1893-1919)", "Older (1919-1945)", "Younger (1945-1970)", "Youngest (1970-1996)"))

# Correct Binning for Dt_Customer_Year
marketing_response$Binned_Dt_Customer <- cut(marketing_response$Dt_Customer_Year,
                                             breaks = c(2011, 2012, 2013, 2014), 
                                             include.lowest = TRUE,
                                             labels = c("2012", "2013", "2014"))

# Calculate correlations for numerical variables, excluding ID and Dt_Customer_Year
numeric_vars <- marketing_response %>%
  select(where(is.numeric)) %>%
  select(-c(ID, Year_Birth, Dt_Customer_Year)) # Exclude non-relevant columns

correlations <- cor(numeric_vars, use = "complete.obs")
correlations_response <- correlations['Response',] # Extract correlations with Response

# Round the correlation values for better readability
correlations_response_rounded <- round(correlations_response, 3)

# Convert the rounded correlations to a data frame for a nicer print format
correlations_df <- as.data.frame(t(correlations_response_rounded))

# Assuming correlations have been calculated as before

# Round the correlation values for 'Response' and print in one row
correlations_response_rounded <- round(correlations['Response',], 3)

# Print the rounded correlations for 'Response' in one line
cat("Correlations with Response:", "\n")
print(correlations_response_rounded, print.gap = 3)


# Visualizations
education_plot <- ggplot(marketing_response, aes(x = Education, fill = as.factor(Response))) +
  geom_bar(position = "fill") +
  labs(title = "Response Rate by Education Level", x = "Education Level", y = "Proportion")

marital_status_plot <- ggplot(marketing_response, aes(x = Marital_Status, fill = as.factor(Response))) +
  geom_bar(position = "fill") +
  labs(title = "Response Rate by Marital Status", x = "Marital Status", y = "Proportion")

binned_year_birth_plot <- ggplot(marketing_response, aes(x = Binned_Year_Birth, fill = as.factor(Response))) +
  geom_bar(position = "fill") +
  labs(title = "Response Rate by Age Group", x = "Age Group", y = "Proportion")

binned_dt_customer_plot <- ggplot(marketing_response, aes(x = Binned_Dt_Customer, fill = as.factor(Response))) +
  geom_bar(position = "fill") +
  labs(title = "Response Rate by Customer Since", x = "Customer Since", y = "Proportion")

# Print the plots
print(education_plot)
print(marital_status_plot)
print(binned_year_birth_plot)
print(binned_dt_customer_plot)





# Boxplot to visualize the relationship between AcceptedCmp5 and Response
accepted_cmp5_plot <- ggplot(marketing_response, aes(x = as.factor(AcceptedCmp5), y = Response)) +
  geom_boxplot() +
  labs(title = "Response by Accepted Campaign 5", x = "Accepted Campaign 5", y = "Response")

# Print the boxplot
print(accepted_cmp5_plot)




# Scatter and Box plots can proceed as before but consider updating variable names or analysis based on the above changes if necessary.



# Scatter plots for continuous variables vs Response
# Adjust the variable names as needed based on your dataset

# Scatter plot for MntWines vs Response
ggplot(marketing_response, aes(x = MntWines, y = Response)) +
  geom_jitter(alpha = 0.5) +
  labs(title = "Scatter plot of MntWines vs Response", x = "MntWines", y = "Response")

# Scatter plot for MntMeatProducts vs Response
ggplot(marketing_response, aes(x = MntMeatProducts, y = Response)) +
  geom_jitter(alpha = 0.5) +
  labs(title = "Scatter plot of MntMeatProducts vs Response", x = "MntMeatProducts", y = "Response")

# Scatter plot for Recency vs Response
ggplot(marketing_response, aes(x = Recency, y = Response)) +
  geom_jitter(alpha = 0.5) +
  labs(title = "Scatter plot of Recency vs Response", x = "Recency", y = "Response")


