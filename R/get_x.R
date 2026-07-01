#' Get x position adapted from GWASpoly::manhattan.plot customized
#' and optimized for adding gap between chromosomes
#'
#' @param map      : data.frame  Data frame with markers, chromosomes and
#'                               positions
#' @param gap_size : double      Total size multiplier to define gap size
#'                               between chromosomes.
#'
#' @returns Vector of positions
#' @noRd
get_x <- function(map, gap_size = 0) {

  a <- tapply(map[,2], map[,1], max)
  n <- length(a)
  m <- tapply(map[,2], map[,1], length)
  b <- c(
    0,
    sapply(1:(n-1), function(k) {
      # Add gap size to current position based on number of chromosomes
      sum(a[1:k]) + (sum(a) * gap_size) * k
    })
  )

  x <- map[,2] + rep(b, times = m)
  return(x)
}
