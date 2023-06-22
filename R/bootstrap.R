#' Bootstrap a data set with replacement
#'
#' @param data A data frame from which to generate bootstrap samples.
#' @param times A numeric input specifying how many bootstrap draws are to be 
#'    made on `data`
#' @param include_original A logical input. If `TRUE` the returned output will 
#'    contain a row with the original `data`. See details. Default is `FALSE`.
#' @param seed A numeric input specifying the seed to be used. Default is `NULL`
#' @param ... These dots are for future extensions and must be empty
#'
#' @details 
#' @return A nested `tibble` 
#' @export
#' @examples
#'
#' test_dat <- data.frame(a = 1:10, b = rnorm(10))
#' bootstrap_data(test_dat, times = 5) 
#'
bootstrap_data <- function(data, times, include_original = FALSE, seed = NULL, ...){
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
