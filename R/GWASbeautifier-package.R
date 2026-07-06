#' GWASbeautifier
#'
#' Tools to format and visualize GWAS results from GWASpoly, GAPIT, and related
#' pipelines.
#'
#' @keywords internal
#'
#' @importFrom dplyr all_of arrange bind_rows filter group_by if_any last_col left_join mutate relocate rename select summarise ungroup
#' @importFrom ggplot2 aes element_blank element_text expansion facet_wrap geom_blank geom_hline geom_point ggplot ggplot_build ggtitle guides scale_colour_manual scale_shape scale_x_continuous scale_y_continuous theme theme_bw ylab
#' @importFrom magrittr %>%
#' @importFrom methods new setClass
#' @importFrom rlang .data := .data
#' @importFrom tibble column_to_rownames
#' @importFrom tidyr pivot_longer replace_na
#' @importFrom utils read.csv
"_PACKAGE"
