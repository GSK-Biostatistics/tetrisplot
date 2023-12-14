#-------------------------------------------------------------------------------
#' @title Uni-variate variable selection
#' @description A univariate screening of covariates against some outcome. 
#' @param x An output of the `bootstrap_data` function.
#' @param response a character argument denoting the outcome
#' @param vars a character vector denoting the covariates or features to be 
#'    screened
#' @param level a numeric scalar denoting a significance threshold to be used  
#'    against each uni-variate test performed. Default is `0.05`.
#' @param family the error distribution and link function passed to the `glm()` 
#'    call inside the `analyse_univariate` function. Default is `gaussian`.
#' @param method a character value of the chosen test to be used. Currently 
#'    only a Wald Chi-squared test is implemented.
#'
#' @details A number of `analysis` wrappers have been created to illustrate 
#'    the challenge of final variable selection. These can be combined in 
#'    sequence as desired by the user to consider combinations of selection per 
#'    bootstrap replication. Although discouraged (Moons et al., 2012), 
#'    univariate selection is not uncommon, particularly as a way to pre-screen 
#'    many covariates prior to some multi-variable fit with possible further 
#'    feature selection thereafter. The present function is a simple wrapper for 
#'    the `aod::wald.test()` function, an implementation of the Wald Chi-squared 
#'    test under an assumption of asymptotic (multivariate) normality.
#' @return A nested `tibble` 
#' @references 
#' Moons, K. G. M., Kengne, A. P., Woodward, M., Royston, P., Vergouwe, Y., 
#'    Altman, D. G., & Grobbee, D. E. (2012). Risk prediction models: I. 
#'    Development, internal validation, and assessing the incremental value of 
#'    a new (bio)marker. Heart, 98(9), 683–690. 
#'    https://doi.org/10.1136/heartjnl-2011-301246
#' @export
#' @examples
#' data(iswr_stroke)
#' iswr_stroke %>% 
#'   bootstrap_data(10, seed = 1234) %>%
#'   analyse_univariate(response = "dead12",
#'                      vars = c("Gender", "Age", "Diagnosis", "Coma", 
#'                               "Diabetes", "MI", "Hypertension"),
#'                      level = 0.05)
#'
analyse_univariate <- function(x, 
                               response = NULL, 
                               vars = NULL, 
                               level = 0.05,
                               family = stats::gaussian(), 
                               method = "Wald"){
  if(method != "Wald")
    stop("Only Wald tests are implemented in analyse_univariate()", 
         call. = FALSE)
  
  if(!any(class(x) == "tetris_analysis")){
    mods_char <- purrr::map(vars, ~ glue::glue({response}, " ~ ", {.x}))
    names(mods_char) <- vars
    
    out <- dplyr::mutate(
      .data = x, 
      vars = purrr::map(data, 
                        function(y){
                          rslt <- 
                            purrr::map_chr(mods_char, 
                                           ~ step_univariate(data = y, 
                                                             model = .x, 
                                                             level = level,
                                                             family = family))
                          unname(rslt[!is.na(rslt)])
                        })
    )
  }
  
  if(any(class(x) == "tetris_analysis")){
    
    response <- attr(x, "response")
    
    out <- dplyr::mutate(
      .data = x, 
      vars = purrr::map2(
        data, 
        vars,
        function(y, z){
          mods_char <- purrr::map(z, ~ glue::glue({response}, " ~ ", {.x}))
          names(mods_char) <- z
          rslt <- 
            purrr::map_chr(mods_char, 
                           ~ step_univariate(data = y, 
                                             model = .x, 
                                             level = level,
                                             family = family))
          unname(rslt[!is.na(rslt)])
        })
    )
  }
  
  if(!is.null(vars))
    attr(out, "vars") <- vars
  
  attr(out, "response") <- response  
  class(out) <- unique(append("tetris_analysis", class(out)))
  out
}

#-------------------------------------------------------------------------------
#' @title Backward stepwise selection using AIC
#' @description An application of backward stepwise selection using AIC as the 
#'    criterion for stepping down (i.e., from a full model) through the 
#'    candidate set of variables.     
#' @param x An output of the `bootstrap_data` function.
#' @param response a character argument denoting the outcome
#' @param vars a character vector denoting the covariates or features to be 
#'    screened
#' @param family the error distribution and link function passed to the `glm()` 
#'    call inside the `analyse_univariate` function. Default is `gaussian`.
#' @param k as per the `stats::step` definition: "the multiple of the number of 
#'    degrees of freedom used for the penalty". 
#'    
#' @details The present function is a simple wrapper for the `stats::step()`
#'    function. There are various commentaries that have been published on the 
#'    use of stepwise selection, with a range of interpretations of its utility, 
#'    spanning from more generous proponents focussing on the simplicity with 
#'    which a reduced model with "insignificant" variables removed, through to 
#'    those that highlight how it invalidates most of any statistical inference 
#'    that can be drawn from the final fit. Irrespective of the stance taken, 
#'    this method is still common practice in many clinical prediction modelling 
#'    publications and is thus worth including for illustrating selection 
#'    instability. 
#' @return A nested `tibble` 
#' @references 
#' Harrell, F. E. (2015). Regression Modeling Strategies: With Applications to 
#'    Linear Models, Logistic and Ordinal Regression, and Survival Analysis. 
#'    Springer International Publishing. 
#'    https://books.google.co.uk/books?id=sQ90rgEACAAJ
#' @export
#' @example 
#' data(iswr_stroke)
#' iswr_stroke %>%
#'   bootstrap_data(10, seed = 1234) %>%
#'   analyse_backward(response = "dead12",
#'                    vars = c("Gender", "Age", "Diagnosis", "Coma",
#'                             "Diabetes", "MI", "Hypertension"))
analyse_backward <- function(x, 
                             response = NULL, 
                             vars = NULL, 
                             family = stats::gaussian(),
                             k = 2){
  
  if(!any(class(x) == "tetris_analysis")){
    mod_char <- glue::glue(response, " ~ ", paste0(vars, collapse = " + "))
    
    out <- dplyr::mutate(
      .data = x, 
      vars = purrr::map(data, 
                        ~ step_backward(data = .x, 
                                        model = mod_char, 
                                        family = family, 
                                        k = k)))
  }
  
  if(any(class(x) == "tetris_analysis")){
    
    response <- attr(x, "response")
    
    out <- dplyr::mutate(
      .data = x, 
      vars = purrr::map2(
        data, 
        vars,
        function(y, z){
          mod_char <- glue::glue(response, " ~ ", paste0(z, collapse = " + "))
          step_backward(data = y, 
                        model = mod_char, 
                        family = family, 
                        k = k) 
        }))
  }
  
  if(!is.null(vars))
    attr(out, "vars") <- vars
  
  attr(out, "response") <- response  
  class(out) <- unique(append("tetris_analysis", class(out)))
  out
}

#-------------------------------------------------------------------------------
#' @export
analyse_grlasso <- function(x, 
                            response = NULL, 
                            vars = NULL, 
                            family = "gaussian",
                            grpreg_list = list(NULL)){
  
  if(!any(class(x) == "tetris_analysis")){
    mod_char <- glue::glue(response, " ~ ", paste0(vars, collapse = " + "))
    
    out <- dplyr::mutate(
      .data = x, 
      vars = purrr::map(data, 
                        ~ step_grlasso(data = .x, 
                                       model = mod_char, 
                                       family = family, 
                                       grpreg_list = grpreg_list)))
  }
  
  if(any(class(x) == "tetris_analysis")){
    
    response <- attr(x, "response")
    
    out <- dplyr::mutate(
      .data = x, 
      vars = purrr::map2(
        data, 
        vars,
        function(y, z){
          mod_char <- glue::glue(response, " ~ ", paste0(z, collapse = " + "))
          step_grlasso(data = .x, 
                       model = mod_char, 
                       family = family, 
                       grpreg_list = grpreg_list)
        }))
  }
  
  if(!is.null(vars))
    attr(out, "vars") <- vars
  
  attr(out, "response") <- response  
  class(out) <- unique(append("tetris_analysis", class(out)))
  out
}
