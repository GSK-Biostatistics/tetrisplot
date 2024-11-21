library(readr)

# iswr_stroke <-
#   ISwR::stroke |>
#   tibble::as_tibble() |>
#   dplyr::select(-died,
#                 -dstr) |>
#   dplyr::mutate(dead = as.numeric(dead),
#                 sex = as.factor(sex),
#                 dgn = as.factor(dgn),
#                 coma = as.factor(coma),
#                 diab = as.factor(diab),
#                 minf = as.factor(minf),
#                 han = as.factor(han),
#                 dead12 = dplyr::case_when(obsmonths <= 12 & dead == 1 ~ 1,
#                                           obsmonths > 12 & dead == 1 ~ 0,
#                                           obsmonths > 12 & dead == 0 ~ 0,
#                                           TRUE ~ NA_real_)) |>
#   tidyr::drop_na() |>
#   dplyr::rename(`Age` = age,
#                 `Gender` = sex,
#                 `Hypertension` = han,
#                 `MI` = minf,
#                 `Diabetes` = diab,
#                 `Diagnosis` = dgn,
#                 `Coma` = coma) |>
#   dplyr::select(-obsmonths, 
#                 -dead)

iswr_stroke <- readr::read_rds("./data-raw/iswr_stroke.rds")
usethis::use_data(iswr_stroke, overwrite = TRUE)
