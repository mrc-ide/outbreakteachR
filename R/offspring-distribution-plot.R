#' Plot offspring distribution
#'
#' \code{offspring_distribution_plot} takes output of \code{outbreak_dataset_read} and
#' plots offspring distribution. Returns ggplot object for plotting.
#'
#' @param outbreak.dataset Outbreak dataset outputted by \code{outbreak_dataset_read}
#' @param include.reinfectons Boolean detailing whether to include reinfections. Default = TRUE
#' @param title Plot title. Default = NULL
#'
#' @export
#'
#' @aliases offspring_distribution_plot
#'

offspring_distribution_plot <- function(outbreak.dataset,
                                        include.reinfectons= TRUE,
                                        title=NULL) {

  # load windows plotting fonts
  extrafont::loadfonts(device="win",quiet = T)

  # tabulate the offspring distribution observed

  if(include.reinfectons==TRUE){
    offspring_distribution_df <- data.frame("R0" = as.numeric(
      table( factor(outbreak.dataset$Parent.ID, levels = min(outbreak.dataset$Parent.ID,na.rm = T):max(outbreak.dataset$Parent.ID,na.rm=T)))
    ))
  } else {
    offspring_distribution_df <- data.frame("R0" = as.numeric(
      table( factor(outbreak.dataset$Parent.ID[outbreak.dataset$Reinfection==FALSE], levels = min(outbreak.dataset$Parent.ID,na.rm = T):max(outbreak.dataset$Parent.ID,na.rm=T)))
    ))
  }

  # Create ggplot object
    dens <- "..density.."
  res <-ggplot2::ggplot(offspring_distribution_df, ggplot2::aes_string("R0",dens)) +
    ggplot2::geom_histogram(bins = length(-1:7),colour = "black", fill = "white") +
    ggplot2::geom_freqpoly(bins = length(-1:7), size = 1) +
    ggplot2::xlab(bquote('Offspring Distribution' ~ (R[0])))  +
    ggplot2::ylab("Frequency") +
    ggplot2::scale_x_continuous(breaks=-1:7,limits = c(-1,7),labels = c("",0:7)) +
    ggplot2::theme_classic() + ggplot2::theme_light() +
    ggplot2::theme(legend.position="none",
      axis.text.x = ggplot2::element_text(size = 12, family = "Times New Roman"),
      axis.title.y = ggplot2::element_text(margin=ggplot2::margin(c(0,10)),size = 14, family = "Times New Roman"),
      axis.text.y = ggplot2::element_text(size = 12, family = "Times New Roman"),
      plot.title = ggplot2::element_text(size = 14, family = "Times New Roman",hjust = 0.5),
      panel.border = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line())

   # add title if required
  if(!is.null(title)){

    res <- res + ggplot2::ggtitle(bquote(atop(.(title),'Mean' ~ R[0] ~ '=' ~ .(round(mean(offspring_distribution_df$R0),digits=2)))))
  }

  # retrun ggplot object
  return(res)

}

