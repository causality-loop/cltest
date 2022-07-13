.datatable.aware = TRUE 
if (getRversion() >= '2.15.1') 
  utils::globalVariables(c('.'), utils::packageName()) 
 
#' @title table_contribution
#' @description Makes a list of data tables indicating the portfolio contribution for each model, asset.
#' @param make_permutation_data_output the output of the `make_permutation_data()` function
#' @return A list of data tables.
#' @export 
#' @importFrom PerformanceAnalytics Return.cumulative
#' @importFrom data.table data.table
table_contribution <- function(make_permutation_data_output)
{

  model_contrib_list <- make_permutation_data_output$Model_Contribution
  asset_contrib_list <- make_permutation_data_output$Asset_Contribution
  bench_ret <- make_permutation_data_output$Benchmark_Return

  fn <- function(contrib_list, col_name) {
    lapply(contrib_list, function(x) {
      PerformanceAnalytics::Return.cumulative(x) %>%
        t %>%
        data.table::data.table(keep.rownames = TRUE) %>%
        `colnames<-`(c(col_name, 'Cum_Ret')) %>%
        .[order(-Cum_Ret)]
    })
  }

  list(
    Model = fn(model_contrib_list, 'Model'),
    Assets = fn(asset_contrib_list, 'Asset'))

}

.datatable.aware = TRUE 
if (getRversion() >= '2.15.1') 
  utils::globalVariables(c('.', 'ym', 'index', 'rets', 'Cum_Ret'), utils::packageName()) 

#' @importFrom data.table ':=' 
#' @importFrom magrittr '%$%' '%T>%' 
#' @title table_calendar_returns
#' @description Makes a list of calendar returns.
#' @param rets_xts xts, an xts of returns
#' @param ... additional parameters passed to `PerformanceAnalytics::table.CalendarReturns()`
#' @return A list of calendar returns.
#' @export 
#' @importFrom data.table data.table
#' @importFrom zoo as.yearmon
#' @importFrom xts xts
#' @importFrom PerformanceAnalytics table.CalendarReturns
table_calendar_returns <- function(rets_xts, ...)
{
  lapply(rets_xts, function(x) {
    data.table::data.table(x, keep.rownames = TRUE) %T>%
      {names(.)[2] <- 'rets'} %>%
      .[, ym := zoo::as.yearmon(index)] %>%
      .[, .(rets = prod(rets+1)-1), by = ym] %$%
      xts::xts(rets, ym+(1/12)) %>%
      `names<-`('Return') %>%
      PerformanceAnalytics::table.CalendarReturns(...)
  })
}

#' @title table_annualized_returns
#' @description A wrapper for `PerformanceAnalytics::table.AnnualizedReturns`.
#' @param rets_xts xts, an xts of returns
#' @param ... additional parameters passed to `PerformanceAnalytics::table.AnnualizedReturns()`
#' @return See documentation for `PerformanceAnalytics::table.AnnualizedReturns()`.
#' @export 
#' @importFrom PerformanceAnalytics table.AnnualizedReturns
table_annualized_returns <- function(rets_xts, ...)
{
  PerformanceAnalytics::table.AnnualizedReturns(rets_xts, ...)
}

#' @title table_drawdowns
#' @description A wrapper for `PerformanceAnalytics::table.Drawdowns`.
#' @param rets_xts xts, an xts of returns
#' @param ... additional parameters passed to `PerformanceAnalytics::table.Drawdowns()`
#' @return See documentation for `PerformanceAnalytics::table.Drawdowns()`.
#' @export 
#' @importFrom PerformanceAnalytics table.Drawdowns
table_drawdowns <- function(rets_xts, ...)
{
  lapply(rets_xts, function(x) {
    PerformanceAnalytics::table.Drawdowns(x, ...)
  })
}

