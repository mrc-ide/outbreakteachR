#' Plot multiplot of network at 24 hour intervals with the epidemic timeseries
#'
#' \code{combined_plot} takes output of \code{first_infection_list} and
#' plots multiplot of network at 24 hour intervals with the epidemic timeseries
#'
#' @param first_infection_list Infection list outputted by \code{first_infection_list}
#'
#' @export
#'
#'
#'

combined_plot <- function(first_infection_list){

  # Extra function for sorting layout
  lay_out = function(...) {
    x <- list(...)
    n <- max(sapply(x, function(x) max(x[[2]])))
    p <- max(sapply(x, function(x) max(x[[3]])))
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, p)))

    for (i in seq_len(length(x))) {
      print(x[[i]][[1]], vp = grid::viewport(layout.pos.row = x[[i]][[2]],
                                             layout.pos.col = x[[i]][[3]]))
    }
  }

 # Parameters for this layout will need to be played with, but something like this
  lay_out(list(outbreakteachR::daily_timeseries_plot(first_infection_list = first_infection_list), 1, 1:5),
          list(outbreakteachR::epidemic_timeseries_plot(first_infection_list = first_infection_list), 2, 1:5))


}
