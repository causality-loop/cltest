'%ni%' <- Negate('%in%')

make_test_units <- function(model_funs, model_units, max_units,
  index_ref_symbol = 'SPY')
{

  if (index_ref_symbol %ni% list.files('prices'))
    index_ref_symbol <- list.files('prices')[1]

  fn <- function(model_funs, model_units, max_units)
  {

    asset_units_list <- lapply(seq(model_funs), function(x) {
      model_funs[[x]](FALSE, model_units = model_units[x])
    })

    # scale_model_asset_units
    units_rowsums_list <- lapply(asset_units_list, function(x) {
      xts::reclass(rowSums(x), x)
    }) 

    last_date <- lapply(units_rowsums_list, 
      function(x) zoo::index(x) %>% utils::head(1)) %>%
      do.call(what = c) %>%
      sort %>%
      utils::tail(1)

    index_ref_xts <- readRDS(file.path('prices', index_ref_symbol))[,1]
  
    units_rowsums <- lapply(units_rowsums_list, function(x) {
      xts::merge.xts(x, index_ref_xts) %>%
        zoo::na.locf() %>%
        .[,-2]
    }) %>%
      do.call(what = xts::merge.xts) %>%
      .[paste0(last_date, '/')]

    # how much to subtract from the rowsums
    # first take the max_units - cumsum of each row
    subtract_units <- apply(
      units_rowsums,1, function(x) max_units-cumsum(x)) %>% 
      t %>% xts::reclass(units_rowsums)

    if (nrow(subtract_units) == 1) subtract_units %<>% unname %>% as.numeric

    # leaving out positive numbers just yields how much to subtract
    subtract_units[subtract_units > 0] <- 0

    # adjusted units_rowsums; the units of each model now don't exceed max_units
    units_rowsums_adj <- units_rowsums + subtract_units
    # long-only models, so there are no units less than zero
    units_rowsums_adj[units_rowsums_adj < 0] <- 0

    # scaling factor to be applied to the units of each security in each model
    scale_factors <- units_rowsums_adj/units_rowsums
    # NaNs come from 0/0
    scale_factors[is.nan(scale_factors)] <- 0

    # scale the the units for each security in each model
    model_asset_units_scaled <- lapply(seq(asset_units_list), function(x) {
      units_and_scale_factor <- merge(
        asset_units_list[[x]], scale_factors[,x], join = 'right') %>%
        zoo::na.locf()
      scale_factor <- units_and_scale_factor[,ncol(units_and_scale_factor)]
      units <- units_and_scale_factor[,-ncol(units_and_scale_factor)]
      units * as.numeric(scale_factor)
    }) %>%
      `names<-`(names(asset_units_list))

    # done scale_model_asset_units
    all_symbols <- sapply(model_asset_units_scaled, names) %>% 
      unlist %>% unique %>% sort 

    lapply(all_symbols, function(x) {
      lapply(model_asset_units_scaled, function(y) y[, names(y) == x]) %>%
        .[sapply(., function(z) length(z) > 0)] %>%
        do.call(what = xts::merge.xts) %>%
        {xts::xts(rowSums(.), zoo::index(.))} %>%
        `names<-`(x)
    }) %>%
      do.call(what = xts::merge.xts) %>%
      stats::na.omit()

  }

  if (!dir.exists('prices')) 
    stop('make some prices using the updateprices package')

  if (any(is.null(names(model_funs)))) model_names <- paste0('model', 1:4)
  else model_names <- names(model_funs)

  all_models_asset_units <- fn(model_funs, model_units, max_units) %>%
    list %>%
    `names<-`('ALL')
  
  indiv_model_asset_units <- lapply(seq(model_funs), function(x) {
    fn(model_funs[x], model_units[x], max_units) 
  }) %>% 
    `names<-`(model_names) 

  c(all_models_asset_units, indiv_model_asset_units)

}

