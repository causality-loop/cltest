# cltest

## Background
- Provides various functions for testing a model of portfolios

## OHLCV data
- In your working directory, download OHLCV data for each symbol of each model
- In this example, we will be working with three models (see below) from the *clmodels* package
```r
library(updateprices)
kda_symbols <- c('SPY', 'FXI', 'EWJ', 'EEM', 'VNQ', 'RWX', 'DBC', 'GLD', 'VWO', 'AGG')
gan_symbols <- c('SPY', 'DIA', 'QQQ')
can_symbols <- c('SPY', 'VWO', 'AGG')
asset_info <- c(kda_symbols, gan_symbols, can_symbols)
update_prices(asset_info, FALSE, from_date = '1900-01-01')
```

## Generate reports
```r
library(cltest)

# 54.740 seconds
perm_data <- make_permutation_data(
  model_funs = c(
    CAN = clmodels::CAN,
    GAN = clmodels::GAN,
    KDA = clmodels::KDA_no_treasuries),
  model_units = c(1.5, 1, 1.5),
  max_units = 2.5,
  make_contributions = TRUE,
  benchmark_symbol = 'SPY'
)

# 19.17 seconds
wftest_list <- walk_forward_test(perm_data)

report_xts <- make_report_xts(perm_data, wftest_list)

# all reports
chart_rolling_performance(report_xts)
chart_performance_summary(report_xts)
chart_risk_return_scatter(report_xts)
table_annualized_returns(report_xts)
table_calendar_returns(report_xts)
table_drawdowns(report_xts)
# note perm_data is the input:
table_contribution(perm_data)
```
