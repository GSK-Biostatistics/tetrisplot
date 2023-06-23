#' Tetris data formater
#'
#' @param x An output of the workflow
#'
#' @details 
#' @return A `tibble` with appended 
#' @export
#' @examples
#'
#' datasets::mtcars %>% 
#'   bootstrap_data(10) %>% 
#'   analyse_univariate(response = "mpg",
#'                      vars = c("disp", "hp","drat", "wt","qsec"),
#'                      level = 0.05) %>% 
#'  format_tetris()                    
#'
format_tetris <- function(x){
  
  vec <- attr(x, "vars")
  
  combinations <- 
    x %>%
    unnest(cols = c(vars)) %>%
    select(-data) %>%
    mutate(flag = 1,
           vars = factor(vars, levels = vec)) %>%
    complete(boot_rep, vars, fill = list(flag = 0)) %>%
    arrange(boot_rep, vars) %>%
    pivot_wider(names_from = vars,
                values_from = flag) %>% 
    filter(boot_rep != 0) %>% 
    nest(data = -boot_rep) %>% 
    mutate(comb = map_chr(data, 
                          ~ colnames(.x)[as.numeric(.x)==1] %>% 
                            sort() %>% 
                            paste0(collapse = ", "))) 
  
  freq_count <- 
    combinations %>% 
    group_by(comb) %>% 
    summarise(n = n())
  
  rslt <- 
    combinations %>% 
    left_join(freq_count, by = "comb") %>% 
    unnest(cols = data) 
  return(rslt)
  
}