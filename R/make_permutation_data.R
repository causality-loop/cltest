.datatable.aware = TRUE 
if (getRversion() >= '2.15.1') 
  utils::globalVariables(c('.'), utils::packageName()) 
 
#' @importFrom magrittr '%<>%' '%T>%' 
#' @title make_permutation_data
#' @description Provides an object which can then be passed to other functions to access the viability of each permutation of a collection of models.
#' @param model_funs a named vector of model functions
#' @param model_units numeric, a vector of units to allocate to a certain model (the ordering of this vector corresponds with the ordering of the `model_funs` parameter)
#' @param max_units numeric, the maximum number of units which may be allocated to the portfolio
#' @param make_contributions logical, if TRUE, the portfolio contributions will be outputted (better performance if set to FALSE), Default: TRUE
#' @param benchmark_symbol character, the symbol whose return series will be used as a benchmark, Default: 'SPY'
#' @return A list of data points, including model/benchmark returns, (optionally) model/asset portfolio contributions, correlations, and ancillary information.
#' @details Before use, make sure that `model_funs` is named, and that the necessary OHLCV series are in `./prices`.  This function is designed to create an object to be passed to other functions.  See documentation in the *References* section (below).
#' @references
#' \url{https://github.com/causality-loop/cltest}
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  # Where the units for CAN is 1.5, Gan: 1, and KDA: 0.5
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
#'  }
#' }
#' @export 
#' @importFrom TTR ROC
#' @importFrom stats na.omit cor
#' @importFrom parallel mclapply
#' @importFrom clhelpers make_permutations calculate_portfolio_return
#' @importFrom xts merge.xts
#' @importFrom data.table data.table
#' @importFrom utils combn
make_permutation_data <- function(model_funs, model_units, max_units, 
  make_contributions = TRUE, benchmark_symbol = 'SPY')
{

  if (is.null(names(model_funs))) stop('Please add give model_funs names')

  if (benchmark_symbol %ni% list.files('prices'))
    stop('Benchmark OHLCV not yet downloaded')

  bench_ret <- readRDS(file.path('prices', benchmark_symbol))[,4] %>% 
    { TTR::ROC(., type = 'discrete') * max_units } %>%
    stats::na.omit() %>%
    `names<-`(benchmark_symbol)

  lmf <- length(model_funs)
  smf <- seq(model_funs)

  if (lmf > 0) {

    nums_mat1 <- t(seq(3))
    x1 <- parallel::mclapply(smf, function(x) {
      make_test_units(
        model_funs[x], 
        model_units[x],
        max_units)
    })

  } else {

    stop('Length of model_funs must be > 0')

  }

  if (lmf > 1) {

    nums_mat2 <- cbind(utils::combn(smf, 2), utils::combn(smf, 2)[2:1,])
    x2 <- parallel::mclapply(seq(ncol(nums_mat2)), function(x) {
      make_test_units(
        model_funs[nums_mat2[,x]], 
        model_units[nums_mat2[,x]],
        max_units)
    })

  } 

  if (lmf > 2) {

    for (i in 3:lmf) {
      nums_mat <- clhelpers::make_permutations(seq(i)) %>% t
      x <- parallel::mclapply(seq(ncol(nums_mat)), function(x) {
        make_test_units(
          model_funs[nums_mat[,x]], 
          model_units[nums_mat[,x]],
          max_units)
      })
      assign(paste0('x', i), x)
      assign(paste0('nums_mat', i), nums_mat)
    }

  } 

  alls <- list()
  not_alls <- list()
  for (i in smf) {
    x_num <- get(paste0('x', i))
    alls[[i]] <- lapply(seq(x_num), function(x) x_num[[x]]$ALL)
    not_alls[[i]] <- lapply(seq(x_num), function(x) x_num[[x]][-1])
  }

  alls %<>% unlist(FALSE)
  not_alls %<>% unlist(FALSE)

  # TODO BOP/EOP value might be good in the future
  calc_port_ret_out <- parallel::mclapply(alls, function(x) {
    clhelpers::calculate_portfolio_return(x, TRUE)
  })

  rets_for_each_permutation_xts <- lapply(calc_port_ret_out, 
    function(x) x$returns) %>%
    do.call(what = xts::merge.xts) %>%
    `names<-`(seq(alls))

  if (make_contributions) {
    model_contribution_for_each_permutation_list <- parallel::mclapply(
      not_alls, function(x) {
        lapply(names(x), function(y) {
          clhelpers::calculate_portfolio_return(x[[y]])
        }) %>%
          do.call(what = xts::merge.xts) %>%
          `names<-`(names(x))
      })

    asset_contribution_for_each_permutation_list <- lapply(calc_port_ret_out, 
      function(x) x$contribution %>% .[, -ncol(.)])
  }

  model_nums <- lapply(smf, function(x) {
    nums_mat <- get(paste0('nums_mat', x))
    lapply(seq(ncol(nums_mat)), function(y) nums_mat[,y])
  }) %>%
    unlist(FALSE)

  model_names <- lapply(model_nums, function(x) {
    names(model_funs)[x]
  })

  info <- data.table::data.table(
    seq(model_names),
    do.call(rbind, lapply(model_names, paste0, collapse = ' ')),
    do.call(rbind, lapply(model_nums, paste0, collapse = ' '))
  ) %>%
    `names<-`(c('col_num', 'model_names', 'model_nums'))

  cor_tbl <- rets_for_each_permutation_xts[,seq(model_funs)] %>% 
    `names<-`(names(model_funs)) %>%
    stats::na.omit() %>%
    stats::cor() %T>%
    {diag(.) <- 0}

  if (length(cor_tbl[cor_tbl > 0.7]) != 0)
    cat('\nWARNING: Some models are highly correlated (see .$cor_tbl)\n\n')

  if (make_contributions) {
    list(
      Returns = rets_for_each_permutation_xts, 
      Model_Contribution = model_contribution_for_each_permutation_list,
      Asset_Contribution = asset_contribution_for_each_permutation_list,
      Benchmark_Return = bench_ret,
      Info = info, 
      Cor = cor_tbl)
  } else {
    list(
      Returns = rets_for_each_permutation_xts, 
      Benchmark_Return = bench_ret,
      Info = info, 
      Cor = cor_tbl)
  }

}

