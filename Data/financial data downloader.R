
library(tidyquant)
library(ggplot2)
library(tidyr)
library(dplyr)

options("getSymbols.warning4.0" = FALSE)
options("getSymbols.yahoo.warning" = FALSE)

start_date = '2018-12-31'
end_date = format(Sys.Date(), format="%Y-%m-%d")

# Stock indices tickers
tickers = c('SPY',
            'XLB',
            'XLV',
            'XLP',
            'XLY',
            'XLE',
            'XLF',
            'XLI',
            'XLK',
            'XLU',
            'XLRE',
            'XLC',
            '^MXX')

# Names of indices
ticker_names = c('S&P 500',
                 'Materials',
                 'Health Care',
                 'Consumer Staples',
                 'Consumer Discretionary',
                 'Energy',
                 'Financials',
                 'Industrials',
                 'Technology',
                 'Utilities',
                 'Real State',
                 'Communication Services',
                 'Mexican Stocks Index')

# Stock prices query
stock_indices <- tq_get(tickers,
                      from = start_date,
                      to = end_date,
                      get = "stock.prices")

# Exchange rate
mxn_rates <- tq_get('DEXMXUS',
                      from = start_date,
                      to = end_date,
                      get = "economic.data")

# Outputting the data
write.csv(stock_indices, row.names = FALSE, file = 'Data/Stock indices.csv')
write.csv(mxn_rates, row.names = FALSE, file = 'Data/Exchange rate.csv')




