
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Customize Trading Period:** Choose your entry and exit dates.

3. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```{r load-data}
setwd("C:/Users/PC/Desktop/ACTL1101 Assingment 1")
# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```


##Plotting the Data
Plot the closing prices over time to visualize the price movement.
```{r plot}
plot(amd_df$date, amd_df$close,'l')
```
## Step 2: Customize Trading Period
- Define a trading period you wanted in the past five years 
```{r period}
# Fill your code here
start_date <- as.Date('2022-09-01')  # Define start date
end_date <- as.Date('2023-06-01')    # Define end date

# Filter the data to include only the trading period
amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ]

```



## Step 3: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.


```{r trading}
# Create a copy of the dataframe for the initial trading strategy
# This ensures that the original data is preserved for other analyses
amd_df_initial <- amd_df

# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df_initial
# 'trade_type' will store whether it's a 'buy' or 'sell'
# 'costs_proceeds' will store the financial impact of each trade
# 'accumulated_shares' will track the total number of shares held
amd_df_initial$trade_type <- NA
amd_df_initial$costs_proceeds <- NA
amd_df_initial$accumulated_shares <- 0

# Initialize variables for trading logic
# 'previous_price' will store the price of the previous trading day
# 'share_size' defines the number of shares to buy/sell in each trade
# 'accumulated_shares' tracks the total number of shares held over time
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Loop through each row of the dataframe to simulate daily trading
for (i in 1:(nrow(amd_df_initial) - 1)) {  # Loop through all rows except the last one
    current_price <- amd_df_initial$close[i]  # Get the current day's closing price
    
    # Check if this is the first day (previous_price is 0)
    if (previous_price == 0) {
        # Buy 100 shares on the first day
        amd_df_initial$trade_type[i] <- "buy"
        amd_df_initial$costs_proceeds[i] <- -current_price * share_size  # Record the cost of buying shares (negative)
        accumulated_shares <- accumulated_shares + share_size  # Update total shares held
    } else if (current_price < previous_price) {
        # Buy 100 shares if the current price is lower than the previous day's price
        amd_df_initial$trade_type[i] <- "buy"
        amd_df_initial$costs_proceeds[i] <- -current_price * share_size  # Record the cost of buying shares (negative)
        accumulated_shares <- accumulated_shares + share_size  # Update total shares held
    }
    
    # Store the accumulated shares count for the current day
    amd_df_initial$accumulated_shares[i] <- accumulated_shares
    # Update the previous_price to the current day's closing price
    previous_price <- current_price
}

# Final day: Ensure correct number of shares is sold
# Make an index for the last day
last_day_index <- nrow(amd_df_initial)

# Ensure no additional shares are bought on the last day
current_price <- amd_df_initial$close[last_day_index]  # Get the last day's closing price

# Record the trade type as 'sell'
amd_df_initial$trade_type[last_day_index] <- "sell"

# Calculate and record the proceeds from selling all remaining shares
amd_df_initial$costs_proceeds[last_day_index] <- accumulated_shares * current_price

# Reset accumulated_shares to 0 after selling all shares
amd_df_initial$accumulated_shares[last_day_index] <- 0

#Print the last day stats
cat("Accumulated shares on the last day:", accumulated_shares, "\n")
cat("Proceeds from the sale on the last day: $", amd_df_initial$costs_proceeds[last_day_index], "\n")

#Make a new csv
write.csv(amd_df_initial, file = "amd_initial.csv", row.names = FALSE)
```



## Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}
# Calculate Total Profit/Loss (Before Profit-Taking Strategy)
total_profit_loss_before <- sum(amd_df_initial$costs_proceeds, na.rm = TRUE)
cat("Total Profit/Loss before Profit-Taking Strategy: $", total_profit_loss_before, "\n")

# Calculate Invested Capital (sum of all 'buy' transactions)
total_invested_capital_before <- -sum(amd_df_initial$costs_proceeds[amd_df_initial$trade_type == "buy"], na.rm = TRUE)
cat("Total Invested Capital before Profit-Taking Strategy: $", total_invested_capital_before, "\n")

# Calculate ROI before Profit-Taking Strategy
roi_before <- (total_profit_loss_before / total_invested_capital_before) * 100
cat("ROI before Profit-Taking Strategy: ", roi_before, "%\n")
```

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.

Option 1 was changed to have price percentage as 25% instead of 20% and selling quarter of holdings instead of half.

```{r option}
# Create copy of the dataframe for the profit-taking strategy
amd_df_profit_taking <- amd_df

# Initialize columns for trade type, cost/proceeds, accumulated shares, and average purchase price
amd_df_profit_taking$trade_type <- NA
amd_df_profit_taking$costs_proceeds <- NA
amd_df_profit_taking$accumulated_shares <- 0
amd_df_profit_taking$avg_purchase_price <- NA  # New column for running average purchase price

# Initialize variables for trading logic
total_cost <- 0
total_shares <- 0
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
avg_purchase_price <- 0
profit_threshold <- 0.25  # 25% changed from the original 20%

# Loop through each row of the dataframe to simulate daily trading
for (i in 1:(nrow(amd_df_profit_taking) - 1)) {  # Loop through every row (ie: every day), except the last one (last day)
  current_price <- amd_df_profit_taking$close[i]  # Get the current day's closing price
  
  if (previous_price == 0) {
    # Buy on the first day
    amd_df_profit_taking$trade_type[i] <- "buy"  # Record the trade type as 'buy'
    cost <- -current_price * share_size  # Calculate the cost of buying shares (negative value)
    amd_df_profit_taking$costs_proceeds[i] <- cost  # Record the cost in the dataframe
    accumulated_shares <- accumulated_shares + share_size  # Update total shares held
    total_cost <- total_cost + (-cost)  # Update total cost (minus sign because cost is negative)
    total_shares <- total_shares + share_size  # Update total number of shares
    
  } else if (current_price < previous_price) {
    # Buy if the current price is lower than the previous day's price
    amd_df_profit_taking$trade_type[i] <- "buy"  # Record the trade type as 'buy'
    cost <- -current_price * share_size  # Calculate the cost of buying shares (negative value)
    amd_df_profit_taking$costs_proceeds[i] <- cost  # Record the cost in the dataframe
    accumulated_shares <- accumulated_shares + share_size  # Update total shares held
    total_cost <- total_cost + (-cost)  # Update total cost (subtract because cost is negative)
    total_shares <- total_shares + share_size  # Update total number of shares
  }
  
  # Calculate the running average purchase price if there are shares
  if (total_shares > 0) {
    avg_purchase_price <- total_cost / total_shares  # Calculate running average purchase price
    amd_df_profit_taking$avg_purchase_price[i] <- avg_purchase_price  # Record running average purchase price in dataframe
  } else {
    amd_df_profit_taking$avg_purchase_price[i] <- NA  # Set to NA if no shares are held
  }
  
  # Check for profit-taking condition if avg_purchase_price is valid
  if (total_shares > 0 && current_price >= (1 + profit_threshold) * avg_purchase_price) {
    # Sell half of the holdings
    shares_to_sell <- accumulated_shares / 4  # Find the number of shares to sell (quarter of total)
    amd_df_profit_taking$trade_type[i] <- "sell"  # Record the trade type as 'sell'
    amd_df_profit_taking$costs_proceeds[i] <- shares_to_sell * current_price  # Record the proceeds from selling shares
    accumulated_shares <- accumulated_shares - shares_to_sell  # Update total shares held
    total_shares <- total_shares - shares_to_sell  # Update total number of shares
    total_cost <- total_cost - (shares_to_sell * avg_purchase_price)  # Adjust total cost by removing sold shares
    
    # Update running average purchase price after selling
    if (total_shares > 0) {
      avg_purchase_price <- total_cost / total_shares  # Recalculate running average purchase price
    } else {
      avg_purchase_price <- 0  # Reset to 0 if no shares are held
    }
  }
  
  amd_df_profit_taking$accumulated_shares[i] <- accumulated_shares  # Store the accumulated shares count for the current day
  previous_price <- current_price  # Update the previous_price to the current day's closing price
}

# Final Sell on the last day if there are remaining shares
last_day_index <- nrow(amd_df_profit_taking)  # Get the index for the last day
current_price <- amd_df_profit_taking$close[last_day_index]  # Get the last day's closing price

if (accumulated_shares > 0) {
  amd_df_profit_taking$trade_type[last_day_index] <- "sell"  # Record the trade type as 'sell'
  amd_df_profit_taking$costs_proceeds[last_day_index] <- accumulated_shares * current_price  # Record the proceeds from selling all remaining shares
  amd_df_profit_taking$accumulated_shares[last_day_index] <- 0  # Reset to 0 after selling all shares
  amd_df_profit_taking$avg_purchase_price[last_day_index] <- NA  # Set to NA since no shares remain
}

# Calculate Total Profit/Loss (After Profit-Taking Strategy)
total_profit_loss_after <- sum(amd_df_profit_taking$costs_proceeds, na.rm = TRUE)
cat("Total Profit/Loss after Profit-Taking Strategy: $", total_profit_loss_after, "\n")

# Calculate Invested Capital (sum of all 'buy' transactions)
total_invested_capital_after <- -sum(amd_df_profit_taking$costs_proceeds[amd_df_profit_taking$trade_type == "buy"], na.rm = TRUE)
cat("Total Invested Capital after Profit-Taking Strategy: $", total_invested_capital_after, "\n")

# Calculate ROI after Profit-Taking Strategy
roi_after <- (total_profit_loss_after / total_invested_capital_after) * 100
cat("ROI after Profit-Taking Strategy: ", roi_after, "%\n")

# Make another new csv
write.csv(amd_df_profit_taking, file = "amd_profit_taking.csv", row.names = FALSE)
```



## Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```{r}

cat("Total Profit/Loss before Profit-Taking Strategy: $", total_profit_loss_before, "\n")
cat("Buy-and-Hold ROI for the period from September 2022 to June 2023: ", roi_before, "%\n")

cat("Total Profit/Loss after Profit-Taking Strategy: $", total_profit_loss_after, "\n")
cat("ROI after Profit-Taking Strategy: ", roi_after, "%\n")

cat("Change in ROI: ", roi_after-roi_before, "%\n")
cat("Change in Profit/Loss: $", total_profit_loss_after-total_profit_loss_before, "\n")
# Discuss based on the analysis and stock price graph
plot(amd_df$date, amd_df$close, type = 'l', col = 'blue', xlab = 'Date', ylab = 'Closing Price', main = 'AMD Stock Price')
```

During the selected period of September 1st 2022, to June 1st 2023, AMD's stock price experienced heavy market volatility however had a clear overall upward trend as seen in the graph. The stock price was seen to grow from approximately $70 to over $120 from beginning to end. This means that theoretically, it would be possible to have a ~70% ROI if a stock from the $60 mark was held all the way till its peak of $120.

Analysis of Performance.

Buy and Hold Strategy:
The buy and hold trading algorithm produced an ROI of 54.8462% and a profit of $393539. This large ROI and profit is a reflection of the clear upwards trend showing that simply holding the stock through the volatile trading period was able to result in large returns. However, this also means that this method is very risky and solely relied on the stock price increasing by the end of the period, if this were not to have occured, it could have resulted in significant losses and thus a potentially negative ROI. Since the stock price did in fact have such a great upward trend, it demonstrated that by simply holding shares, and then simply selling for a much higher price as seen at the end of the period, a high ROI and profit was obtained.


Profit-taking Strategy:
After the profit-taking strategy was implemented, the ROI decreased to 44.67316% with a profit of $292835.2, which is comparatively roughly 19% and 26% less than the results from the buy and hold strategy respectively. This strategy involved selling a quarter of the holdings when stock prices increased by 25%, which obviously led to more frequent trades and hence premature sells in such a volatile environment. As such, these trades often occurred far from price peaks and troughs, making the strategy more susceptible to the significant fluctuations of the stock price. However, this also means that the method is much safer and involves less risk compared to the buy and hold method by not having to rely as much on having a strong upward stock price trend to generate profit. Alas, this method was indeed able to lock in smaller gains but missed on the larger stock price increases that could be captured by simply holding the stock. Hence, it is obvious that this strategy was not able to fully capitalise on the large price increases and thus fell short to the buy and hold method.

It can also be noted that by selling a smaller portion of the holdings every time the stock pricess increased by 25%, a higher profit threshold could be obtained in this specific scenario. However, this will obviously introduce an increased risk.

Relevant Market Events.
Multiple market events occured during this trading period, which are likely to have caused the significant stock price increase. Such events can effect the stock price through various means, such as boosting stakeholder confidence through a good earnings report, as well as making advancements that push the technology industry forward. 

New Products: During this period, AMD launched their newest generation of Ryzen processors, which were quite well received by the general market. This likely contributed to the overall upward trend of the stock price.

Earnings Report: AMD's quarterly reports were also released around this time, and showed robust financial performance. This would have also likely boosted general investor confidence leading to a stock price increase.

Both of events likely contributed to the stock price increase and as such allowed such high returns to be obtained through the trading strategies used.







