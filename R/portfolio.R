# Add Portfolio Data

# Adds stock data to your portfolio for tracking dividends.

install.packages(c("dplyr", "ggplot2", "gtable"))


add_portfolio <- function(stock, shares, dividend_per_share, purchase_date) {
  if (missing(stock) || missing(shares) || missing(dividend_per_share) || missing(purchase_date)) {
    stop("All arguments (stock, shares, dividend_per_share, purchase_date) are required.")
  }

  data.frame(
    Stock = stock,
    Shares = as.numeric(shares),
    DividendPerShare = as.numeric(dividend_per_share),
    PurchaseDate = as.Date(purchase_date),
    stringsAsFactors = FALSE
  )
}

# Function to calculate total dividends for a portfolio
calculate_dividends <- function(portfolio) {
  if (!all(c("Shares", "DividendPerShare") %in% colnames(portfolio))) {
    stop("The portfolio data frame must contain 'Shares' and 'DividendPerShare' columns.")
  }

  library(dplyr)
  portfolio %>%
    mutate(TotalDividend = Shares * DividendPerShare)
}

# Function to plot dividend trends over time
plot_dividend_trend <- function(dividends_data) {
  if (!all(c("Date", "TotalDividend", "Stock") %in% colnames(dividends_data))) {
    stop("The data must contain 'Date', 'TotalDividend', and 'Stock' columns.")
  }

  library(ggplot2)
  ggplot(dividends_data, aes(x = Date, y = TotalDividend, color = Stock)) +
    geom_line() +
    labs(title = "Dividend Income Over Time", x = "Date", y = "Total Dividends") +
    theme_minimal()
}



