#' @importFrom magrittr %>%
#' @export correallyPlot
#' @export validateHex
#' @export validateBaseColor
#' @export validateRColorBrewer
#' @export validateViridis
#'
#' @details
#' Description of what the package does in general
#'
#' @section correallyPlot:
#'
#' This is the main function of the package. It provides a wrapper to plotly to create an interactive visualization of a correlation matrix.
#' It provides a clean implementation for plotting correlation matrices, easy use of different color schemes
#' (Base R colors, Hex colors, [RColorBrewer] palettes as well as support for [viridisLite]). It also provides coloring options for backgrounds, grids and fonts
#' matching different [shinythemes].
#'
#' @section correallyPlotAnimated:
#' Not yet implemented
#'
#' @section Helper functions:
#' [correally] comes with a bunch of helper functions that facilitate the main [correally::correallyPlot] function.
#'
#' \enumerate{
#' \item Validation functions
#' \itemize{
#' \item [validateBaseColor]
#' \item [validateHex]
#' \item [validateRColorBrewer]
#' \item [validateViridis]
#' }
#' }
#'
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
