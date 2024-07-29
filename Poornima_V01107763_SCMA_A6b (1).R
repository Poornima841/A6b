# Function to auto-install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("quantmod", "tseries", "rugarch", "ggplot2")

# Call the function
install_and_load(packages)

# Define the stock symbol for IRFC Ltd. (replace with actual symbol if different)
stock_symbol <- "IRFC.NS" 

# Download historical stock prices
getSymbols(stock_symbol, src = "yahoo", from = "2021-04-01", to = '2024-03-31')

# Extract adjusted close prices
prices <- Cl(get(stock_symbol))

# Calculate daily returns
returns <- diff(log(prices)) * 100

# Perform Ljung-Box test on squared returns
squared_returns <- returns^2
lb_test <- Box.test(squared_returns, lag = 10, type = "Ljung-Box")
print(lb_test)

# Create a data frame for plotting
returns_df <- data.frame(Date = index(returns), Returns = coredata(returns))
returns_df <- na.omit(returns_df)

# Print column names
colnames(returns_df)

# Plot returns using ggplot2
ggplot(returns_df, aes(x = Date, y = IRFC.NS.Close)) +
  geom_line() +
  labs(title = "Daily Returns of Jupiter Wagons Ltd.",
       x = "Date",
       y = "Returns") +
  theme_minimal()


# Define the GARCH model specification
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0)),
  distribution.model = "std"
)

# Fit the GARCH model
garch_fit <- ugarchfit(spec = garch_spec, data = returns_df)
print(garch_fit)

# Forecast the volatility for the next three months (approximately 63 trading days)
forecast <- ugarchforecast(garch_fit, n.ahead = 63)

# Extract forecasted volatility
volatility_values <- as.numeric(forecast@forecast$sigmaFor)

# Create a sequence of dates starting from a specific date
start_date <- as.Date('2024-04-01') # Define the start date as a Date object
forecast_dates <- seq(from = start_date, by = "days", length.out = length(volatility_values))

# Create a data frame for plotting
volatility_forecast_df <- data.frame(Date = forecast_dates, Volatility = volatility_values)

# Print column names
colnames(volatility_forecast_df)

# Plot forecasted volatility
ggplot(volatility_forecast_df, aes(x = Date, y = Volatility)) +
  geom_line() +
  labs(title = "Forecasted Volatility for Jupiter Wagons Ltd.",
       x = "Date",
       y = "Volatility") +
  theme_minimal()


# Function to auto-install and load packages
install_and_load_b <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages_b <- c("readxl", "dplyr", "janitor", "urca","vars")

# Call the function
install_and_load_b(packages_b)

# Load the dataset
df <- read_excel('CMO-Historical-Data-Monthly.xlsx', sheet = "Monthly Prices", skip = 6)

# Rename the first column to "Date"
colnames(df)[1] <- 'Date'
# Convert the Date column to Date format
df$Date <- as.Date(paste0(df$Date, "01"), format = "%YM%m%d")
str(df)

# Select specific columns (Date and selected commodities)
commodity <- df[,c(1, 63, 64, 65, 66)] %>%
  clean_names()

str(commodity)

# Remove the Date column for analysis
commodity_data <- dplyr::select(commodity, -date)

# Column names to test (if you want to specify particular columns)
columns_to_test <- names(commodity_data)

# Initialize counters and lists for stationary and non-stationary columns
non_stationary_count <- 0
stationary_columns <- list()
non_stationary_columns <- list()

# Loop through each column and perform the ADF test
for (col in columns_to_test) {
  adf_result <- ur.df(commodity_data[[col]], type = "none", selectlags = "AIC")
  p_value <- adf_result@testreg$coefficients[2, 4]  # Extract p-value for the test
  cat("\nADF test result for column:", col, "\n")
  print(summary(adf_result))
  
  # Check if the p-value is greater than 0.05 (commonly used threshold)
  if (p_value > 0.05) {
    non_stationary_count <- non_stationary_count + 1
    non_stationary_columns <- c(non_stationary_columns, col)
  } else {
    stationary_columns <- c(stationary_columns, col)
  }
}

# Print the number of non-stationary columns and the lists of stationary and non-stationary columns
cat("\nNumber of non-stationary columns:", non_stationary_count, "\n")
cat("Non-stationary columns:", unlist(non_stationary_columns), "\n")
cat("Stationary columns:")
stationary_columns

# Co-Integration Test (Johansen's Test)
# Determining the number of lags to use (you can use information criteria like AIC, BIC)
lags <- VARselect(commodity_data, lag.max = 10, type = "const")
lag_length <- lags$selection[1] # Choosing the lag with the lowest AIC

vecm_model <- ca.jo(commodity_data, ecdet = 'const', type = 'eigen', K = lag_length, spec = 'transitory')

# Summary of the Co-Integration Test
summary(vecm_model)

# Determine the number of co-integrating relationships (r) based on the test
# Here, we assume r = 1 if there's at least one significant eigenvalue
r <- 3 # Replace with the actual number from the test results

if (r > 0) {
  # If co-integration exists, estimate the VECM model
  vecm <- cajorls(vecm_model, r = r)  # r is the number of co-integration vectors
  
  # Summary of the VECM model
  summary(vecm)
  
  # Extracting the coefficients from the VECM model
  vecm_coefs <- vecm$rlm$coefficients
  print(vecm_coefs)
  
  # Creating a VAR model for prediction using the VECM
  vecm_pred <- vec2var(vecm_model, r = r)
  
  # Forecasting using the VECM model
  # Forecasting 12 steps ahead
  forecast <- predict(vecm_pred, n.ahead = 24)
  
  # Plotting the forecast
  par(mar = c(4, 4, 2, 2))  # Adjust margins: c(bottom, left, top, right)
  plot(forecast)
  
} else {
  # If no co-integration exists, proceed with Unrestricted VAR Analysis
  var_model <- VAR(commodity_data, p = lag_length, type = "const")
  
  # Summary of the VAR model
  summary(var_model)
  
  # Granger causality test
  causality_results <- causality(var_model)
  print(causality_results)
  
  # Forecasting using the VAR model
  forecast <- predict(var_model, n.ahead = 24)
  
  # Plotting the forecast
  par(mar = c(4, 4, 2, 2))  # Adjust margins: c(bottom, left, top, right)
  plot(forecast)
}

forecast
