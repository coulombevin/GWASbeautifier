#' Extract all/significant markers
#' @description
#' Extract markers information (trait, marker, chromosome, position, model LOD values) from GWASpoly.thresh or GAPIT.thresh data obtained with [GWASpoly::set.threshold()] or [get_formatted_gapit()] command.
#'
#' @param data 'package'.thresh : Variable of class GWASpoly.thresh or GAPIT.thresh
#' @param significant_only bool : Extract only markers above threshold.
#'
#' @returns data.frame object with all requested markers.
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' significant_markers <- extract_markers(data = data_with_threshold)
#' }
#' @seealso
#' [get_formatted_gapit()] for GAPIT output data conversion.
extract_markers <- function (data, significant_only = TRUE) {
  # Allow only data from GWASpoly::set.threshold(...) or
  # GWASbeautifier::get_formatted_gapit(...) command.
  stopifnot(inherits(data, 'GWASpoly.thresh') | inherits(data, "GAPIT.thresh"))
  # Define columns names
  col_names <- c(c('trait', 'Marker', 'Chrom', 'Position'), colnames(data@scores[[1]]))
  # Generate data frame
  data_markers <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  # Set columns names
  colnames(data_markers) <- col_names

  # Loop trough traits
  for (i in seq(from = 1, to = length(data@scores))) {

    current_trait <- names(data@scores[i])
    # Generate a data frame using marker, trait, scores, chrom and pos
    data_tmp <- data@scores[[i]] %>%
      tibble::rownames_to_column(var = "Marker") %>%
      dplyr::mutate(trait = current_trait, .before = 1) %>%
      dplyr::left_join(
        dplyr::select(data@map, .data$Marker, .data$Chrom, .data$Position),
        by = 'Marker'
      ) %>%
      dplyr::relocate(.data$Chrom, .after = .data$Marker) %>%
      dplyr::relocate(.data$Position, .after = .data$Chrom)

    if (significant_only) {
      # Get the threshold
      thr <- data@threshold[i]
      # Filter for significant markers
      data_tmp <- data_tmp %>%
        dplyr::filter(dplyr::if_any(dplyr::all_of(colnames(data@scores[[i]])), ~ .x >= thr))
    }
    # Append filtered data to significant data frame
    data_markers <- rbind(data_markers, data_tmp)
    # Reset row index
    rownames(data_markers) <- NULL
  }

  return(data_markers)
}
