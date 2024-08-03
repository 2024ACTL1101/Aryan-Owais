

# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```{r load-data}
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1])) # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")

```

#### Data Processing 
```{r data}
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

\[ E(R_i) = R_f + \beta_i (E(R_m) - R_f) \]

Where:

- \( E(R_i) \) is the expected return on the capital asset,
- \( R_f \) is the risk-free rate,
- \( \beta_i \) is the beta of the security, which represents the systematic risk of the security,
- \( E(R_m) \) is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```{r return}
# Calculate the daily returns for AMD and S&P 500
amd_returns <- dailyReturn(Cl(amd_data))
sp500_returns <- dailyReturn(Cl(gspc_data))

# Convert the returns to data frame so it can be merged
returns_data <- merge(amd_returns, sp500_returns, all = FALSE)
returns_data_df <- data.frame(Date = index(returns_data), coredata(returns_data))

# Make the names
colnames(returns_data_df) <- c("Date", "AMD_Returns", "SP500_Returns")

# Print the head of returns_data_df
print("Head of returns_data_df:")
print(head(returns_data_df))

# Merge the returns data with the main df
merged_df <- merge(df, returns_data_df, by = "Date", all = TRUE)

```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```{r riskfree}
#Calculate the RF rate
merged_df <- merged_df %>%
  mutate(Daily_RF = (1 + RF / 100)^(1 / 360) - 1)

```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```{r excess return}
# Calculate the Excess Returns
merged_df <- merged_df %>%
  mutate(AMD_Excess_Returns = AMD_Returns - Daily_RF,
         SP500_Excess_Returns = SP500_Returns - Daily_RF)

# Fix the columns into the final dataframe: final_df
final_df <- merged_df %>%
  select(Date, AMD, GSPC, RF, Daily_RF, AMD_Returns, SP500_Returns, AMD_Excess_Returns, SP500_Excess_Returns)

# Print the head of final_df to check all the values
print(head(final_df))
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```{r lm}

# Use linear regression to calculate the beta of AMD, relative to S&P 500
regression_model <- lm(AMD_Excess_Returns ~ SP500_Excess_Returns, data = final_df)

# Print the summary of the model
summary(regression_model)

# Get the beta which is the slope of the regression line
beta <- coef(regression_model)["SP500_Excess_Returns"]
print(paste("Estimated Beta of AMD:", beta))

```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**

The estimated beta (\(\beta\)) of AMD is approximately 1.57. This value is interpreted as, for every change of 1% in S&P500, AMD's returns are expected to change by a factor of 1.57%. This then means that AMD is more volatile than the market.

**Beta greater than 1:** Since (\(\beta\)) is greater than 1, AMD is currently more vulnerable to fluctuations (greater volatility) in the market.

**Investment risk:** The beta of 1.57 (\(\beta\)) results in a greater risk and volatility surrounding specifically AMD's stock compared to the entire market itself. However, investors can possibly be met with greater returns due to this and could compensate the increased risk.

**Market Sensitivity:** By understanding how AMD's stock price responds to market fluctuations through the use of beta (\(\beta\)), a strategic investing strategy can be made.

#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```{r plot}
# Plot the CAPM Line
ggplot(final_df, aes(x = SP500_Excess_Returns, y = AMD_Excess_Returns)) +
  geom_point(color = 'blue') +
  geom_smooth(method = 'lm', col = 'red') +
  labs(title = 'CAPM: AMD vs. S&P 500 Excess Returns',
       x = 'S&P 500 Excess Returns',
       y = 'AMD Excess Returns') +
  theme_minimal()

```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.

*Hint: Calculate the daily standard error of the forecast ($s_f$), and assume that the annual standard error for prediction is $s_f \times \sqrt{252}$. Use the simple return average method to convert daily stock returns to annual returns if needed.*


**Answer:**

```{r pi}
# Prediction Interval 
current_rf_rate <- 0.05
expected_market_return <- 0.133

# Calculate the standard error of the forecast
s_f <- summary(regression_model)$sigma

# Convert the daily standard error to annual standard error
annual_s_f <- s_f * sqrt(252)

# Calculate the expected return for AMD using the CAPM formula
expected_amd_return <- current_rf_rate + beta * (expected_market_return - current_rf_rate)

# Since the standard deviation is unknown, we can use t-distribution to calculate the interval. With a 90% confidence level, the remaining 10% is split into the two sides of the 90% , resulting with 5% below and 5% above. However, only the upper side 95% is needed since this represents the 95th to 100th percentile. Now,  we need to find the critical value (t_value) that leaves this 5% on the upper side and to do this we use the qt function. After doing so, the margin of error can be found and then used to find the bounds.

t_value <- qt(0.95, df = nrow(final_df) - 2)
margin_of_error <- t_value * annual_s_f

lower_bound <- expected_amd_return - margin_of_error
upper_bound <- expected_amd_return + margin_of_error

# Print the results in the format of [lower bound, upper bound]

print(paste("90% Prediction Interval for AMD's Annual Expected Return: [", round(lower_bound, 4), ", ", round(upper_bound, 4), "]", sep = ""))
```

