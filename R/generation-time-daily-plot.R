#' Plot Daily Generation Time
#'
#' \code{generation_time_daily_plot} takes output of \code{outbreak_dataset_read} and
#' plots dotplots for each day showing he generation times
#'
#' @param outbreak.dataset Outbreak dataset outputted by \code{outbreak_dataset_read}
#' @param title Plot title. Default = NULL
#' @param alpha Numeric for plot point transparency. Default = NA, i.e. no transparency
#' @param jitter.width Numeric for plot point jitter width. Default = 0.2
#' @param half.day Boolean detailing whether to break days into AM and PM. Default = FALSE
#'
#' @export
#'
#'
#'

generation_time_daily_plot <- function(outbreak.dataset,title=NULL,
                                    alpha = NA, jitter.width = 0.2,
                                    half.day = FALSE){

  # load windows plotting fonts
  extrafont::loadfonts(device="win",quiet = T)

  # if we want half days
  if(half.day == TRUE){

    # first create new datset variable
    outbreak.dataset$day_am <- cut(outbreak.dataset$Infection_Hours.since.start,
                                   breaks = c(0,seq(12,120,12)),
                                   labels = c("Day 1 AM","Day 1 PM","Day 2 AM",
                                              "Day 2 PM","Day 3 AM","Day 3 PM",
                                              "Day 4 AM","Day 4 PM","Day 5 AM",
                                              "Day 5 PM"))

    # create initial boxplot
    gg <- ggplot2::ggplot(outbreak.dataset[!is.na(outbreak.dataset$Generation_Time_Hours),],
                          aes(x = factor(day_am,
                                         levels = c("Day 1 AM","Day 1 PM","Day 2 AM",
                                                    "Day 2 PM","Day 3 AM","Day 3 PM",
                                                    "Day 4 AM","Day 4 PM","Day 5 AM",
                                                    "Day 5 PM")),
                              y = Generation_Time_Hours)) +
      geom_point(position = ggplot2::position_jitter(width = jitter.width)) +
      scale_x_discrete(drop=FALSE)

  } else {

    # create initial boxplot
    gg <- ggplot2::ggplot(outbreak.dataset[!is.na(outbreak.dataset$Generation_Time_Hours),],
                          ggplot2::aes(x = factor(Infection_Date),
                              y=Generation_Time_Hours)) +
      ggplot2::geom_point(position = ggplot2::position_jitter(width = jitter.width)) +
      ggplot2::scale_x_discrete(drop=FALSE)

  }


  # and make pretty
  res <- gg + ggplot2::ylab("Generation Time (hours)") + ggplot2::xlab("Day") +
    ggplot2::theme_classic() + ggplot2::theme_light() +
    ggplot2::theme(legend.position="none",
                   axis.title.x = ggplot2::element_text(margin=ggplot2::margin(c(10,0)),size = 14, family = "Times New Roman"),
                   axis.text.x = ggplot2::element_text(size = 12, family = "Times New Roman"),
                   axis.title.y = ggplot2::element_text(margin=ggplot2::margin(c(0,10)),size = 14, family = "Times New Roman"),
                   axis.text.y = ggplot2::element_text(size = 12, family = "Times New Roman"),
                   plot.title = ggplot2::element_text(size = 14, family = "Times New Roman",hjust = 0.5),
                   panel.border = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line())

  if(!is.null(title)){
    res <- res + ggplot2::ggtitle(title)
  }

  # return ggplot
  return(res)

}

