################################################################################
# devtools::load_all()
#' 
#' #' Prepare some data for analysis
#' dat <- 
#'   ISwR::stroke %>% 
#'   tibble::as_tibble() %>% 
#'   dplyr::select(-died, 
#'                 -dstr, 
#'                 -obsmonths) %>% 
#'   dplyr::mutate(dead = as.numeric(dead),
#'                 sex = as.factor(sex),
#'                 dgn = as.factor(dgn),
#'                 coma = as.factor(coma),
#'                 diab = as.factor(diab),
#'                 minf = as.factor(minf),
#'                 han = as.factor(han)) %>% 
#'   tidyr::drop_na() 
#' vec <- colnames(dat)[colnames(dat) != "dead"]
#' 
#' #' Boot strap sample
#' boot_dat <- bootstrap_data(dat, times = 500, include_original = TRUE)
#' 
#' #' Perform uni-variate pre-screen at 1% level followed by some backward
#' #' step-wise selection
# var_sel1 <-
#   analyse_backward(boot_dat,
#                      response = "dead",
#                      vars = vec,
#                      family = binomial())
# var_sel2 <-
#   analyse_univariate(boot_dat,
#                      response = "dead",
#                      vars = vec,
#                      family = binomial(),
#                      level = 0.05) |>
#   analyse_backward(family = binomial())
# 
# fig1 <- 
#   tetris_plot(var_sel1)+
#   patchwork::plot_annotation(
#     title = "Backward stepwise selection"
#   )
# 
# fig1 / fig2
# 
# fig2 <- 
#   tetris_plot(var_sel2)+
#   patchwork::plot_annotation(
#     title = "Uni-variate pre-screen followed by backward stepwise selection"
#   )
# 
# fig1 + fig2
################################################################################

