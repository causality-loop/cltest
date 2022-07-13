#' @importFrom magrittr '%<>%' '%T>%' 
#' @title make_report_xts
#' @description Makes an xts which is later passed to other functions which create tables and plots for walk-forward test performance reporting.
#' @param make_permutation_data_output the output of the `make_permutation_data()` function
#' @param walk_forward_test_output the output of the `walk_forward_test()` function
#' @return An xts of model returns at various lookback year lengths (with benchmark return series for comparison).
#' @details See *Examples* for usage.  Takes output from `make_permutation_data()` and `walk_forward_test()` to create an xts comparing the return series of each walk-forward test using different lookback year lengths to the returns of a user-specified benchmark.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  perm_data <- make_permutation_data(
#'    model_funs = c(
#'      CAN = clmodels::CAN, 
#'      GAN = clmodels::GAN,
#'      KDA = clmodels::KDA),
#'    model_units = c(1.5, 1, 0.5),
#'    max_units = 3,
#'    make_contributions = TRUE,
#'    benchmark_symbol = 'SPY'
#'  )
#'  wftest_list <- walk_forward_test(perm_data)
#'  report_xts <- make_report_xts(perm_data, wftest_list)
#'  }
#' }
#' @export 
#' @importFrom xts merge.xts
#' @importFrom stats na.omit
make_report_xts <- function( 
  make_permutation_data_output, walk_forward_test_output)
{

  lapply(walk_forward_test_output[[1]], function(x) x[[1]]) %>%
    do.call(what = xts::merge.xts) %>%
    xts::merge.xts(make_permutation_data_output$Benchmark_Return) %T>%
    {names(.)[1:(ncol(.)-1)] <- walk_forward_test_output[[2]]} %>%
    stats::na.omit()

}

