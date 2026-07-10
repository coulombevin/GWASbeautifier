#' Renaming traits function
#' @description
#' Standardize and format raw traits name into publication-ready scientific labels.
#'
#' @param data           : 'package'.thresh  GWASpoly or GAPIT threshold
#'                                           formated data.
#' @param old_trait_name : vec(str) or str   Old trait(s) name
#' @param new_trait_name : vec(str) or str   New trait(s) name
#'
#' @returns {.code package.thresh} object with renamed traits
#' @export
#'
#' @examples
#' \dontrun{
#' renamed_data_with_threshold <- rename_traits(
#'    data = data_with_threshold,
#'    old_trait_name = "vine.maturity",
#'    new_trait_name = "Vine Maturity"
#' )
#'
#' renamed_data_with_threshold <- rename_traits(
#'    data = data_with_threshold,
#'    old_trait_name = c("h", "w"),
#'    new_trait_name = c("Height", "Weight")
#' )
#' }
rename_traits <- function(data, old_trait_name, new_trait_name) {
  # Allow only data from GWASpoly::set.threshold(...) or
  # GWASbeautifier::get_formatted_gapit(...) command.
  stopifnot(inherits(data, "GWASpoly.thresh") | inherits(data, "GAPIT.thresh"))
  # Stop if names count is different between old and new
  stopifnot(length(old_trait_name) == length(new_trait_name))
  # Stop if old names not found in data
  stopifnot(
    all(old_trait_name %in% names(data@scores)) &
    all(old_trait_name %in% rownames(data@threshold))
  )

  for (i in seq_along(old_trait_name)) {
    # Rename threshold rowname
    rownames(data@threshold)[rownames(data@threshold) == old_trait_name[i]] <- new_trait_name[i]
    # Rename score argument
    names(data@scores)[names(data@scores) == old_trait_name[i]] <- new_trait_name[i]
  }
  return(data)
}
