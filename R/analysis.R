#' Uni-variate variable selection
#'
#' @param x An output of the `bootstrap_data` function.
#' @param response a character argument denoting the outcome
#' @param vars a character vector denoting the covariates or features to be 
#'    screened
#' @param level a numeric scalar denoting a significance threshold to be used  
#'    against each uni-variate test performed. Default is `0.05`.
#' @param family the error distribution and link function passed to the `glm()` 
#'    call inside the `analyse_univariate` function.
#' @param method a character value of the chosen test to be used. Currently 
#'    only a Wald chi-squared test is implemented.
#'
#' @details A number of `analysis_` wrappers have been created to illustrate 
#'    the challenge of final variable selection. These can be combined as 
#'    desired by the user.
#' @return A nested `tibble` 
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
      vars = purrr::map2(data, 
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

#' @export
analyse_backward <- function(x, 
                             response = NULL, 
                             vars = NULL, 
                             family = stats::gaussian(),
                             k = 2){
  
  if(!any(class(x) == "tetris_analysis")){
    mod_char <- glue::glue(response, " ~ ", paste0(vars, collapse = " + "))
    
    out <- dplyr::mutate(.data = x, 
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
      vars = purrr::map2(data, 
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

# stepwise_forward
# 
# stepwise_both
# 

#' @export
analyse_grlasso <- function(x, 
                            response = NULL, 
                            vars = NULL, 
                            family = "gaussian",
                            grpreg_list = list(NULL)){
  
  if(!any(class(x) == "tetris_analysis")){
    mod_char <- glue::glue(response, " ~ ", paste0(vars, collapse = " + "))
    
    out <- dplyr::mutate(.data = x, 
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
      vars = purrr::map2(data, 
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
