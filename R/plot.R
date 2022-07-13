#' @title chart_rolling_performance
#' @description A wrapper for `PerformanceAnalytics::charts.RollingPerformance()`.
#' @param rets_xts xts, an xts of returns
#' @param ... additional parameters passed to `PerformanceAnalytics::charts.RollingPerformance()`
#' @return See documentation for `PerformanceAnalytics::charts.RollingPerformance()`
#' @export 
#' @importFrom PerformanceAnalytics charts.RollingPerformance
chart_rolling_performance <- function(rets_xts, ...)
{
  rets_xts %>%
    PerformanceAnalytics::charts.RollingPerformance(
      main = 'Rolling 12-Month Performance', 
      legend.loc = 'topleft',
      ...)
}

#' @title chart_performance_summary
#' @description A wrapper for `PerformanceAnalytics::charts.PerformanceSummary()`.
#' @param rets_xts xts, an xts of returns
#' @param na_fill logical, if TRUE, NAs will be changed to zeros, Default: FALSE
#' @param ... additional parameters passed to `PerformanceAnalytics::charts.PerformanceSummary()`
#' @return See documentation for `PerformanceAnalytics::charts.PerformanceSummary()`
#' @export 
#' @importFrom PerformanceAnalytics charts.PerformanceSummary
chart_performance_summary <- function(rets_xts, na_fill = FALSE, ...)
{
  if (na_fill) rets_xts[is.na(rets_xts)] <- 0
  PerformanceAnalytics::charts.PerformanceSummary(
    rets_xts, 
    main = 'Performance Summary',
    ...)
}

#' @title chart_risk_return_scatter
#' @description A wrapper for `PerformanceAnalytics::chart.RiskReturnScatter()`.
#' @param rets_xts xts, an xts of returns
#' @param ... additional parameters passed to `PerformanceAnalytics::chart.RiskReturnScatter()`
#' @return See documentation for `PerformanceAnalytics::chart.RiskReturnScatter()`
#' @export 
#' @importFrom PerformanceAnalytics chart.RiskReturnScatter
chart_risk_return_scatter <- function(rets_xts, ...) 
{
  PerformanceAnalytics::chart.RiskReturnScatter(rets_xts, 
    symbolset = 21, 
    bg = 'blue',
    ...)
}

