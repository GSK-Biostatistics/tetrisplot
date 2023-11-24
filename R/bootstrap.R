#' @title Bootstrapped data sets with replacement
#'
#' @description For a user provided data set a number of bootstrapped samples 
#'    are generated with replacement. 
#'    
#' @param data A data frame from which to generate bootstrap samples.
#' @param times A numeric input specifying how many bootstrap draws are to be 
#'    generated from `data`.
#' @param include_original A logical input. If `TRUE` the returned output will 
#'    contain a row with the original `data`. Default is `FALSE` (see details).
#' @param seed A numeric input specifying the seed to be used. Default is `NULL`
#' @param ... These dots are for future extensions and must be empty.
#'
#' @details Note that by design each bootstrapped sample is of the same size as 
#'    the original data set. The function generates a simple random sample with 
#'    replacement along the number of rows in the provided data. The suitability 
#'    of making any inference from the bootstrapped samples depend on a variety 
#'    of points (eg. size of original N, number of boot strap draws generated) 
#'    which are left to the user to decide if they are warranted. The 
#'    `include_original` argument can be useful to obtain apparent selection 
#'    from a given variable selection method.
#'
#' @return A nested `tibble` 
#' @export
#' @examples
#' 
#' test_dat <- data.frame(a = 1:5, b = rnorm(5))
#' bootstrap_data(test_dat, times = 2, seed = 123) 
#'
bootstrap_data <- function(data, 
                           times, 
                           include_original = FALSE, 
                           seed = NULL, 
                           ...){
  set.seed(seed)
  n <- NROW(data)
  samp <- purrr::map(seq_len(times), 
                     ~ sample(x = 1:n, size = n, replace = TRUE))
  boots <- purrr::map(samp, 
                      ~ data[.x, ])
  rslt <- tibble::tibble(boot_rep = 1:times, 
                         data = boots)
  if(include_original){
    rslt0 <- tibble::tibble(boot_rep = 0, data = list(data))
    rslt <- dplyr::bind_rows(rslt0, rslt)
  }
  return(rslt)
}
