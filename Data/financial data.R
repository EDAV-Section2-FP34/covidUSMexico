### Financial data download and preprocessing

library(tidyquant)
library(ggplot2)
library(tidyr)
library(dplyr)

options("getSymbols.warning4.0" = FALSE)
options("getSymbols.yahoo.warning" = FALSE)

start_date = '2018-12-31'
end_date = format(Sys.Date(), format='%Y-%m-%d')

# Tickers
tickers = read.csv('Data/names indices.csv')

# Stock prices query
stock_indices <- tq_get(tickers$Ticker,
                      from = start_date,
                      to = end_date,
                      get = 'stock.prices')

# Exchange rate
fx_rate <- tq_get('DEXMXUS',
                    from = start_date,
                    to = end_date,
                    get = 'economic.data') %>% 
  subset(select = -c(symbol))

# Data preprocesing
final_df <- pivot_wider(data = stock_indices,
                        id_cols = c(date, symbol),
                        names_from = symbol,
                        values_from = adjusted)

final_df = inner_join(final_df, fx_rate, by = 'date')
#final_df <- drop_na(final_df)

# Column names
column_names = c(c('Date'), tickers$Name, c('USDMXN'))
colnames(final_df) = column_names

# Sort by date
final_df <- final_df[order(as.Date(final_df$Date, format="%m/%d/%Y")),]

# Final data to use
write.csv(final_df, row.names = FALSE, file = 'Data/Financial data.csv')

