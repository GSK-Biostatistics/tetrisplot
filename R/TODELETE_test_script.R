################################################################################
# library(tidyverse)
# vec <- c("qsec", "cyl")
# test <-
#   datasets::mtcars %>%
#   mutate(cyl = as.factor(cyl),
#          gear = as.factor(gear)) %>%
#   bootstrap_data(200, include_original = TRUE) %>%
#   analyse_univariate(response = "mpg",
#                      vars = vec,
#                      level = 0.01)
# 
# test %>%
#   unnest(cols = c(vars)) %>%
#   select(-data) %>%
#   mutate(flag = 1,
#          vars = factor(vars, levels = vec)) %>%
#   complete(boot_rep, vars, fill = list(flag = 0)) %>%
#   arrange(boot_rep, vars) %>%
#   pivot_wider(names_from = vars,
#               values_from = flag)
################################################################################