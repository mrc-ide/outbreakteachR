#' Plot epidemic time series
#'
#' \code{epidemic_timeseries_plot} takes output of \code{outbreak_dataset_read} and
#' plots SIR epidemic time series. Returns ggplot object for plotting.
#'
#' @param outbreak.dataset Outbreak Dataset
#' @param title Plot title. If NULL then no title will be added. Defult = NULL
#'
#' @export
#'
#' @aliases epidemic_timeseries_plot
#'

epidemic_timeseries_plot <- function(outbreak.dataset,title=NULL){

  # Function to generate ggplot colours
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  # Create first infection list
  first_infection_list <- outbreakteachR::first_infection_list(outbreak.dataset)

  # Create vector of unique times when events occure, i.e. infection and recovery
  times <- c(0,sort(unique(c(first_infection_list$linelist$Infection_Hours.since.start,
                             first_infection_list$linelist$End_Infection_Hours.since.start))))

  # Now we have the finite times we can set those who were never infected to have infinite times of infections
  uninfected <- which(is.na(first_infection_list$linelist$Infection_Hours.since.start))
  first_infection_list$linelist[uninfected,]$Infection_Hours.since.start <- Inf
  first_infection_list$linelist[uninfected,]$End_Infection_Hours.since.start <- Inf

  # Work out size of population
  N <- dim(first_infection_list$linelist)[1]

  # Create data frame for results
  epidemics <- as.data.frame(list("Times"=times))

  # Calculate size of S, I and R at each time
  epidemics$S <- N - sapply(times,function(x){sum(first_infection_list$linelist$Infection_Hours.since.start<=x)})
  epidemics$R <- sapply(times,function(x){sum(first_infection_list$linelist$End_Infection_Hours.since.start<=x)})
  epidemics$I <- N - epidemics$R - epidemics$S

  # Reshape the data for ggploting
  melt <- reshape2::melt(epidemics,id="Times")

  # Load required fonts
  extrafont::loadfonts(device = "win",quiet=T)

  # Create plot
  res <- ggplot2::ggplot(melt) + ggplot2::geom_point(ggplot2::aes(x=.data$Times,y=.data$value,color=.data$variable),alpha=0.25,size=3) +
    ggplot2::geom_line(ggplot2::aes(x=.data$Times,y=.data$value,color=.data$variable),size=1) +
    ggplot2::scale_x_continuous(breaks=seq(0,24*5,24),limits = c(0,120),expand = c(0.01,0)) +
    ggplot2::xlab("Time (hours)") +
    ggplot2::ylab("Number of Students") +
    ggplot2::theme_classic() + ggplot2::theme_light() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 12, family = "Times New Roman"),
                   axis.title.y = ggplot2::element_text(margin=ggplot2::margin(c(0,10)),size = 14, family = "Times New Roman"),
                   axis.title.x = ggplot2::element_text(margin=ggplot2::margin(c(10)),size = 14, family = "Times New Roman"),
                   legend.text = ggplot2::element_text(size = 12, family = "Times New Roman"),
                   legend.title = ggplot2::element_text(size = 14, family = "Times New Roman"),
                   plot.title = ggplot2::element_text(size = 14, family = "Times New Roman",hjust = 0.5),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line()) +
    ggplot2::scale_color_manual(limit = c("S","I","R"),
                                  labels = c("S","I","R"),
                                  values = gg_color_hue(3)[c(3,1,2)],
                                  name = "Infection\nStatus")

  if(!is.null(title)){
    res <- res + ggplot2::ggtitle(title)
  }

  return(res)

}


