################################################################################
# devtools::load_all()
#' library(tidyverse)
#' 
#' vec <- colnames(datasets::mtcars)[-1]
#' 
#' #' Prepare some data for analysis
#' dat <-
#'   datasets::mtcars %>%
#'   mutate(cyl = as.factor(cyl), gear = as.factor(gear))
#' 
#' #' Boot strap sample
#' boot_dat <- bootstrap_data(dat, times = 100, include_original = TRUE)
#' 
#' #' Perform uni-variate pre-screen at 1% level
#' sel1 <- analyse_univariate(boot_dat,
#'                            response = "mpg",
#'                            vars = vec,
#'                            level = 0.01)
#' 
#' #' Can do some backward step-wise too
#' sel2 <- analyse_backward(sel1)
#' 
#' format_tetris(sel2)

################################################################################

