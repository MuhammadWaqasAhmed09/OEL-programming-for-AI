library(tidyverse)

data <- customer_churn

# 1 Import the dataset and display the number of customers who have churned (Churn == "Yes")

data2 <- data %>% filter(Churn == "Yes") %>% nrow()
print(data2)

# 2 Create a new column ChargeGap
data3 <- data %>% mutate(ChargeGap = TotalCharges - (MonthlyCharges * Tenure), ChargeGap = if_else(is.na(ChargeGap), 0, ChargeGap))
view(data3)

# 3 Filter customers who have been with the company for more than 2 years (Tenure > 24) and are still active (Churn == "No").

data4 <- data %>% filter(Tenure > 24, Churn == "No")
view(data4)

# 4 Find the average MonthlyCharges for each ContractType (e.g., "Month-to-Month", "One year", "Two year")

data5 <- data %>% group_by(ContractType) %>% summarise(AverageMonthlyCharges = mean(MonthlyCharges, na.rm = TRUE))
view(data5)

# 5 Categorize customers based on Age:

data6 <- data %>%
  mutate(
    AgeGroup = case_when(
      Age < 25 ~ "Youth",
      Age <= 55 ~ "Adult",
      Age > 55 ~ "Senior"
    )
  )
view(data6)

# 6 Find the top 5 cities with the highest number of churned customers.

data7 <- data %>% filter(Churn == "Yes") %>% count(City, sort = TRUE) %>% slice_head(n = 5)
view(data7)


# 7 Extract only the names and cities of customers whose TotalCharges > 3000, are on a Month-to-Month contract, and have churned.

data8 <- data %>% filter(TotalCharges > 3000,ContractType == "Month-to-Month",Churn == "Yes") %>% select(Name, City)

# 8 Create a table that shows the average tenure and total revenue (sum(TotalCharges)) for each ContractType.
data9 <- data %>% group_by(ContractType) %>% summarise( AvgTenure = mean(Tenure, na.rm = TRUE), TotalRevenue = sum(TotalCharges, na.rm = TRUE))
view(data9)
