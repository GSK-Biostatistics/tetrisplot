#' Uni-variate variable selection
#'
#' @param x An output of the `bootstrap_data` function.
#' @param response a character argument denoting the outcome
#' @param vars a character vector denoting the covariates or features to be 
#'    screened
#' @param level a numeric scalar denoting a significance threshold to be used  
#'    against each univariate test performed. Default is `0.05`.
#' @param family the error distribution and link function passed to the `glm()` 
#'    call inside the `analyse_univariate` function.
#' @param method a character value of the chosen test to be used. Currently 
#'    only a Wald chi-squared test is implemented.
#'
#' @details 
#' @return A nested `tibble` 
#' @export
#' @examples
#'
#' datasets::mtcars %>% 
#'   bootstrap_data(10) %>% 
#'   analyse_univariate(response = "mpg",
#'                      vars = c("disp", "hp","drat", "wt","qsec"),
#'                      level = 0.05)
#'
analyse_univariate <- function(x, 
                               response, 
                               vars, 
                               level = 0.05,
                               family = stats::gaussian(), 
                               method = "Wald"){
  if(method != "Wald")
    stop("Only Wald tests are implemented in analyse_univariate()", call. = FALSE)
  
  mods_char <- purrr::map(vars, ~ glue::glue({response}, " ~ ", {.x}))
  names(mods_char) <- vars
  
  x %>% 
    dplyr::mutate(vars = purrr::map(
      .x = data, 
      .f = function(y){
        mods_fit <- purrr::map(mods_char,
                               ~ stats::glm(formula = stats::as.formula(.x),
                                            family = family,
                                            data = y))
        wald_tests <- purrr::map(mods_fit, 
                                 function(z){
                                   test <- aod::wald.test(Sigma = vcov(z), 
                                                          b = coef(z), 
                                                          Terms = 2)
                                   test$result$chi2[["P"]]
                                 })
        names(purrr::keep(wald_tests, ~ .x < level))
      }))
}


# stepwise_back
# 
# stepwise_forward
# 
# stepwise_both
# 
# ?stepAIC
# 
# 
# test <- analyse_univariate(iris, "Sepal.Length", c("Sepal.Width", "Species", "Petal.Width"))
# 
