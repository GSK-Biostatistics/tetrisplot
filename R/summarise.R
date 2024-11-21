#' @title Summarise bootstrapped variable selection
#'
#' @description Alongside a plot, it can also be useful to extract the numbers 
#'    behind the "tetris plot".
#' @param x an output of an `analysis_` function of class `tetris_analysis` 
#' @param type a character input of either `"marginal"` (default) or `"joint"`
#' @param ... Currently unused.
#'
#' @details The summary method essentially derives two types of  empirical 
#'    results associated with the generated bootstrap analyses
#' @return A `tibble` with appended joint or marginal probabilities
#' @export
#' @examples
#' data(iswr_stroke)
#' iswr_stroke %>%
#'   bootstrap_data(10, seed = 1234) %>%
#'   analyse_univariate(response = "dead12",
#'                      vars = c("Gender", "Age", "Diagnosis", "Coma",
#'                               "Diabetes", "MI", "Hypertension"),
#'                      family = "binomial") %>% 
#'   summary()

summary <- function(x, type = "marginal", ...) {
  UseMethod("summary")
}

#' @rdname summary
#' @export
summary.tetris_analysis <- function(x, type = "marginal", ...){
  
  if(!type %in% c("marginal", "joint"))
    stop("The type of summary must be either 'marginal' or 'joint'")
  
  if(type == "marginal"){
    rslt <- summary_marg_selection(x)
  }
  
  if(type == "joint"){
    rslt <- summary_joint_selection(x)
  }
  return(rslt)
}

summary_joint_selection <- function(x){
  
  vec <- attr(x, "vars")
  
  combinations <- 
    x %>%
    tidyr::unnest(cols = c(vars)) %>%
    dplyr::select(-data) %>%
    dplyr::mutate(flag = 1,
                  vars = factor(vars, levels = vec)) %>%
    tidyr::complete(boot_rep, vars, fill = list(flag = 0)) %>%
    dplyr::arrange(boot_rep, vars) %>%
    tidyr::pivot_wider(names_from = vars,
                       values_from = flag) %>% 
    dplyr::filter(boot_rep != 0) %>%
    tidyr::nest(data = -boot_rep) %>% 
    dplyr::mutate(comb = purrr::map_chr(data, 
                                        ~ colnames(.x)[as.numeric(.x)==1] %>% 
                                          sort() %>% 
                                          paste0(collapse = ", "))) 
  
  freq_count <- 
    combinations %>% 
    dplyr::group_by(comb) %>% 
    dplyr::summarise(n = dplyr::n())
  
  rslt <- 
    combinations %>% 
    dplyr::left_join(freq_count, by = "comb") %>% 
    tidyr::unnest(cols = data) %>% 
    dplyr::mutate(prop = n/max(boot_rep))
  
  obs_sel <-
    x %>%
    dplyr::filter(boot_rep == 0) %>%
    tidyr::unnest(c(vars))
  
  out <- dplyr::distinct(rslt, comb, prop)
  attr(out, "data") <- rslt
  attr(out, "obs_select") <- obs_sel
  attr(out, "max_rep") <- max(x$boot_rep)
  return(out)
  
}

summary_marg_selection <- function(x){
  
  max_rep <- max(x$boot_rep)
  
  x %>% 
    tidyr::unnest(cols = c(vars)) %>% 
    dplyr::filter(boot_rep != 0) %>% 
    dplyr::group_by(vars) %>% 
    dplyr::summarise(prop = dplyr::n()/max_rep)
  
}
