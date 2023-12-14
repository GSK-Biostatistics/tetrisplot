#' @title Plot of bootstrapped variable selections
#' @description Produces a "tetris plot" highlighting the uncertainty in the 
#'    selection of variables chosen under repeat sampling. 
#' @param x an output of an `analysis_` function of class `tetris_analysis`.
#' @param k a numeric input used to truncate the number of columns in the 
#'    "tetris plot". Defaults to `0` meaning no restriction.
#' @param ... Currently unused.
#'
#' @details In some cases there may be a large number of combinations along 
#'    the columns of the plot (joint selection probabilities). Such scenarios 
#'    can occur when there are many joint selection combinations of empirical 
#'    size `1/times` where `times` are the number of bootstrap draws.  Under 
#'    those circumstances it might be desired to restrict to a "top set" which 
#'    ignores those less frequent combinations - effectively "zooming" in on the 
#'    left-hand-side of the "tetris plot". 
#' @return A `patchwork` of two `ggplot` outputs.
#' @export
#' @examples
#' data(iswr_stroke)
#' iswr_stroke %>%
#'   bootstrap_data(10, seed = 1234) %>%
#'   analyse_univariate(response = "dead12",
#'                      vars = c("Gender", "Age", "Diagnosis", "Coma",
#'                               "Diabetes", "MI", "Hypertension"),
#'                      family = "binomial") %>% 
#'   plot()

plot <- function(x, ...) {
  UseMethod("plot")
}

#' @rdname plot
#' @export

plot.tetris_analysis <- function(x, k = 0, ...){
  
  y_j <- summary_joint_selection(x)
  y_m <- summary_marg_selection(x)
  ord <- dplyr::arrange(y_m, dplyr::desc(prop)) %>% dplyr::pull(vars)
  obs_select <- attr(y_j, "obs_select")
  max_rep <- attr(y_j, "max_rep")
  data <- attr(y_j, "data")
  
  plot_dat <-
    data %>%
    dplyr::select(-boot_rep) %>%
    dplyr::distinct() %>%
    dplyr::arrange(desc(n)) %>%
    tibble::rowid_to_column() %>%
    tidyr::pivot_longer(cols = -c(rowid, comb, n, prop), 
                        names_to = "vars") %>%
    dplyr::mutate(rowid = forcats::fct_inorder(comb) %>% as.numeric(),
                  value = factor(value),
                  vars = ordered(vars, ord)) %>%
    dplyr::filter(n > k)
  
  obs_sel <-
    obs_select %>%
    dplyr::mutate(flag = 1) %>%
    dplyr::right_join(dplyr::filter(plot_dat, rowid == 1), by = c("vars")) %>%
    tidyr::replace_na(list(flag = 0)) %>%
    dplyr::mutate(flag = factor(flag)) %>%
    dplyr::select(rowid, vars, flag) %>%
    dplyr::filter(flag == 1)
  
  col_vec <- grDevices::hcl.colors(palette = "Dark Mint", 
                                   n = length(unique(plot_dat$n)), 
                                   rev = TRUE)
  
  fig_hist <- 
    y_m %>% 
    dplyr::mutate(vars = ordered(vars, ord),
                  str = paste0(round(prop*100), "%")) %>% 
    ggplot2::ggplot(ggplot2::aes(vars, prop)) +
    ggplot2::geom_col(width = .7, fill = 'gray80') +
    ggplot2::geom_text(ggplot2::aes(label = str), hjust = -0.5, size = 10 / ggplot2::.pt) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(.0, .8))) +
    ggplot2::theme_void() +
    ggplot2::coord_flip()
  
  fig_joint <- 
    plot_dat %>%
    ggplot2::ggplot(ggplot2::aes(x = vars, y = rowid))+
    ggplot2::geom_tile(ggplot2::aes(fill = 100*prop))+
    ggplot2::geom_tile(data = plot_dat %>% dplyr::filter(value != 1),
                       mapping = ggplot2::aes(),
                       fill = "white",
                       col = NA)+
    ggplot2::geom_tile(data = plot_dat %>% dplyr::filter(value == 1),
                       mapping = ggplot2::aes(),
                       fill = NA,
                       col = "grey")+
    ggplot2::geom_text(data = obs_sel,
                       mapping = ggplot2::aes(x = vars, y = 0, label = "\u2713"))+
    ggplot2::scale_fill_gradientn(colours = col_vec)+
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(size = 9),
                   legend.position = "bottom",
                   plot.title = ggplot2::element_text(hjust = 0),
                   plot.subtitle = ggplot2::element_text(hjust = 0),
                   plot.caption = ggplot2::element_text(hjust = 0),
                   legend.box = "horizontal",
                   legend.box.just = "left",
                   legend.justification = c(0,0),
                   plot.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(size = 1, fill = NA))+
    ggplot2::labs(x = "", y = "", fill = "Proportion (%)")+
    ggplot2::coord_flip()  
  
  patchwork::wrap_plots(fig_joint, fig_hist,
                        ncol = 2, widths = c(2, 0.5)) 
  
}
