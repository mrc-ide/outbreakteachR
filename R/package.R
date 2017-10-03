#' outbreakteachR: a platform for assisting in "paper outbreak" practical
#'
#' Importing and analysing data sheet created within teaching "paper
#' outbreak". Tools include visualisation and animation of the outbreak dynamics,
#' example simualtion methods. \cr
#'
#' The main functions of the package are:
#' \itemize{
#'
#' \item \code{\link{outbreak_dataset_read}}: read in outbreak dataset
#'
#' \item \code{\link{first_infection_list}}: convert outbreak dataset into linelist and contacts
#'
#' \item \code{\link{infection_network_plot}}: plot infection network as a tree
#'
#' \item \code{\link{combined_plot}}: plot multiplot of network at 24 hour intervals with the epidemic timeseries
#'
#' \item \code{\link{daily_timeseries_plot}}: 5x1 plot of infection network at 24 hour intervals
#'
#' \item \code{\link{animate_infection_network}}: html animation of the outbreak dynamics
#'
#' \item \code{\link{epidemic_timeseries_plot}}: plot SIR epidemics
#'
#' \item \code{\link{paramater_boxplots_plot}}: plot apidemiological parameter boxplots
#'
#' \item \code{\link{offspring_distribution_plot}}: plot R0 distribution
#'
#' \item \code{\link{bootstrap_simulated_plot}}: plot summary of simulation plots
#'
#' }
#'
#' Please see the remainder of the repository for example datasets of
#' outbreak practicals, and the tutorial link in the readme.
#'
#'
#' @author OJ Watson \email{o.watson15@@imperial.ac.uk}
#'
#' @name outbreakteachR_package
#'
#' @importFrom utils packageDescription head tail
#' @importFrom graphics par
#' @importFrom stats dpois quantile rbinom rpois runif
#' @importFrom grDevices hcl
#'
NULL
