.datatable.aware = TRUE 

if (getRversion() >= '2.15.1') 
  utils::globalVariables(c('.', 'Measure', 'Model_Set'), utils::packageName()) 

wft_fn <- function(make_permutation_data_output, lookback_year)
{

  mpdo <- make_permutation_data_output
  mpdo$Returns %<>% stats::na.omit()
  lookback_start_ym <- zoo::as.yearmon(zoo::index(xts::first(mpdo$Returns)))
  y <- lubridate::year(lookback_start_ym)
  m <- lubridate::month(lookback_start_ym)
  m_is_11_or_12 <- m %in% c(11,12)
  m[m %in% c(2,3)] <- 4
  m[m %in% c(5,6)] <- 7
  m[m %in% c(8,9)] <- 10
  m[m %in% c(11,12)] <- 1

  if (m_is_11_or_12) lookback_start_ym <- zoo::as.yearmon(paste0(y+1, '-', m))
  else lookback_start_ym <- zoo::as.yearmon(paste0(y, '-', m))

  lookback_start_qtr <- zoo::as.yearqtr(lookback_start_ym)
  lookback_end_qtr <- lookback_start_qtr + lookback_year - 1/4

  test_start_qtr <- lookback_end_qtr + 1/4
  test_end_qtr <- zoo::as.yearqtr(zoo::index(xts::last((mpdo$Returns))))
  test_len <- (test_end_qtr - test_start_qtr) * 4

  test_qtrs <- seq(
    zoo::as.Date(test_start_qtr), by = 'quarter', length.out = test_len+1) %>%
    zoo::as.yearqtr()

  results <- lapply(test_qtrs, function(x) {
    train_start <- zoo::as.Date(x - lookback_year)
    train_end <- zoo::as.Date(x)
    port_rets_subset <- mpdo$Returns[paste0(train_start, '/', train_end)]

    best_model_col_num <- port_rets_subset %>%
      PerformanceAnalytics::CalmarRatio() %>%
      t %>%
      data.table::data.table(keep.rownames = TRUE) %>%
      `names<-`(c('Model_Set', 'Measure')) %$%
      .[which.max(Measure)] %>%
      .[,Model_Set] %>%
      as.numeric

    best_model_rets <- mpdo$Returns %>% 
      .[zoo::as.yearqtr(zoo::index(.)) == x] %>%
      .[, best_model_col_num]

    list(best_model_rets, best_model_col_num)
  })

  all_rets <- lapply(results, function(x) x[[1]]) %>%
    do.call(what = xts::rbind.xts)

  all_nums <- lapply(results, function(x) x[[2]])

  best_models <- lapply(all_nums, function(x) {
    mpdo$Info[x, ]
  }) %>%
    do.call(what = rbind) %>%
    cbind(seq(nrow(.)), .) %T>%
    {names(.)[1] <- 'iteration'}

  list(all_rets, best_models)

}

#' @title walk_forward_test
#' @description At quarterly intervals, select the best model portfolio permutation using a training set of lookback length = X years.
#' @param make_permutation_data_output the output of the `make_permutation_data()` function
#' @param lookback_year_seq integer, a sequence of integers indicating various lookback lengths (in years), Default: c(1, 3, 5, 10)
#' @return A list of returns (in xts format), ancillary information for each lookback length, as well as the user-inputted `lookback_year_seq`.
#' @details Designed to be passed to the `make_report_xts()` function.
#' @export 
#' @importFrom parallel mclapply
walk_forward_test <- function(make_permutation_data_output, 
  lookback_year_seq = c(1, 3, 5, 10))
{

  out <- parallel::mclapply(lookback_year_seq, function(x) {
    wft_fn(
      make_permutation_data_output = make_permutation_data_output,
      lookback_year = x)
  })

  list(out, lookback_year_seq)

}
