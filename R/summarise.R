#' Summarise bootstrapped data
#'
#' @param x An output of the workflow
#'
#' @details 
#' @return A `tibble` with appended joint or marginal probabilities
#' @export
#' @examples
#'
#' datasets::mtcars %>% 
#'   bootstrap_data(10) %>% 
#'   analyse_univariate(response = "mpg",
#'                      vars = c("disp", "hp","drat", "wt","qsec"),
#'                      level = 0.05) %>% 
#'  summary_joint_selection()                    
#'
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

#' @export
summary_marg_seletion <- function(x){
  
  max_rep <- max(x$boot_rep)
  
  x %>% 
    tidyr::unnest(cols = c(vars)) %>% 
    dplyr::filter(boot_rep != 0) %>% 
    dplyr::group_by(vars) %>% 
    dplyr::summarise(prop = dplyr::n()/max_rep)
  
}
