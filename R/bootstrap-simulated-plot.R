#' Bootstrap and plot simulated epidemic
#'
#' \code{bootstrap_simulated_plot} takes output of \code{first_infection_list} and
#' \code{outbreak_dataset_read} and uses \code{discrete_SIR_simulator} to simulate
#' a number of SIR epidemics, which are then summarised and plotted. Returns ggplot
#' object for plotting.
#'
#' @param R0 Reproductive number. Default = 1.8
#' @param I Number of initial seed infections. Default = 3
#' @param first_infection_list Infection list outputted by \code{first_infection_list}
#' @param outbreak.dataset Outbreak dataset outputted by \code{outbreak_dataset_read}
#' @param replicates Numerical describing number of bootstrap replicates. Default = 2000
#' @param sampling Boolean determining if recovery and generation times should be sampled
#' from the observed or drawn fro a poisson with mean equal to mean of the observed.
#' Default = FALSE (poisson draws used)
#' @param lower.quantile Numeric between 0 and 0.5 describing the lower quantile for
#' each trace. Default = 0.25
#' @param upper.quantile Numeric between 0.5 and 1 describing the lower quantile for
#' each trace. Default = 0.75
#' @param title Plot title. If NULL then no title will be added. Defult = NULL
#' @param alpha Translucency of quantiles. Default = 0.2
#' @param size Line width. Default = 1
#' @param include.line Boolean describing whether to plot with the line or just the
#' quantiles. Default = TRUE
#' @param include.observed Boolean describing whether to include the observed epidemic
#' alongisde the simulated epidemic. If this is true, N will be calculated from outbreak.dataset
#' @param exponential Boolean determining if infection is exponential or not within
#' the simulation. If FALSE (Default) then the number of secondary infections from an individual is
#' takes into account S/N
#'
#' @export
#'
#'


bootstrap_simulated_plot <- function(R0 = 1.8, I = 3,
                                     first_infection_list, outbreak.dataset,
                                     replicates = 2000, sampling = FALSE,
                                     lower.quantile=0.25,
                                     upper.quantile=0.75, title=NULL,
                                     alpha = 0.2, size = 1, include.line = TRUE,
                                     include.observed = FALSE, exponential = FALSE){


  ## Parameter handling and checking
  if(lower.quantile>=0.5 || upper.quantile<=0.5) stop("Incorrect quantiles provided")

  if(include.observed){
    N = max(outbreak.dataset$ID)
  }


  ## EXTRA FUNCTIONS ##
  #######################################################
  ## Extra function required to calculate discrete time mean and quantile estimations
  data_summary <- function(data, varname, grps, lower.quantile, upper.quantile){

    ## summary function describing the quantile calculation
    summary_func <- function(x, col){
      c(mean = mean(x[[col]], na.rm=TRUE),
        sd = quantile(x[[col]], na.rm=TRUE,probs=c(lower.quantile,upper.quantile)))
    }

    ## Calculate summary output
    data_sum <- plyr::ddply(data, grps, .fun=summary_func, varname)
    data_sum <- plyr::rename(data_sum, c("mean" = varname))
    names(data_sum)[c(3,4)] <- c("min","max")
    return(data_sum)
  }

  ## Function to generate ggplot colours
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }


  ## START MAIN ##
  #######################################################

  ## Prepare bootstrap result list
  data <- list()

  ## Create list of replicate simulations

  # Message beginning of replicates
  message(paste("Starting ", replicates," replicates",sep=""))
  for(i in 1:replicates)
  {

    data[[i]] <- outbreakteachR::discrete_SIR_simulator(R0 = R0,
                                               I = I,
                                               first_infection_list = first_infection_list,
                                               outbreak.dataset = outbreak.dataset,
                                               sampling = sampling,
                                               exponential = exponential)
  }

  # Message beginning of replicates
  message(paste("Finished ", replicates," replicates",sep=""))

  ## shape the output into a standard format for plotting
  bootstrap.melt <- reshape2::melt(data,id="times")

  ## Calculate summary data for each infection state
  melt_summary_sv <- data_summary(bootstrap.melt[bootstrap.melt$variable=="Sv",], varname="value", grps= "times", lower.quantile, upper.quantile)
  melt_summary_iv <- data_summary(bootstrap.melt[bootstrap.melt$variable=="Iv",], varname="value", grps= "times", lower.quantile, upper.quantile)
  melt_summary_rv <- data_summary(bootstrap.melt[bootstrap.melt$variable=="Rv",], varname="value", grps= "times", lower.quantile, upper.quantile)

  # Load required fonts
  extrafont::loadfonts(device = "win",quiet=T)

  # Create plot
  res <- ggplot2::ggplot(melt_summary_rv) +
    ggplot2::geom_ribbon(data = melt_summary_rv, ggplot2::aes(x=times,ymin=min, ymax=max),col=gg_color_hue(3)[2],alpha=alpha,fill=gg_color_hue(3)[2]) +
    ggplot2::geom_ribbon(data = melt_summary_sv, ggplot2::aes(x=times,ymin=min, ymax=max),col=gg_color_hue(3)[3],alpha=alpha,fill=gg_color_hue(3)[3]) +
    ggplot2::geom_ribbon(data = melt_summary_iv, ggplot2::aes(x=times,ymin=min, ymax=max),col=gg_color_hue(3)[1],alpha=alpha,fill=gg_color_hue(3)[1])

  if(include.line){
    res <- res + ggplot2::geom_line(data = melt_summary_sv, ggplot2::aes(x=times,y=value,color="S"),linetype = "dashed",size=size) +
      ggplot2::geom_line(data = melt_summary_rv,ggplot2::aes(x=times,y=value,color="R"),linetype = "dashed",size=size) +
      ggplot2::geom_line(data = melt_summary_iv, ggplot2::aes(x=times,y=value,color="I"),linetype = "dashed",size=size)
  }

  res <- res +  ggplot2::scale_x_continuous(breaks=seq(0,24*5,24),limits = c(0,120),expand = c(0.01,0)) +
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

  ## If include.observed is true then actually create a ggplot that combines real and simulated
  if(include.observed){

    ## Create observed
    res <- outbreakteachR::epidemic_timeseries_plot(first_infection_list = first_infection_list)

    ## Combine simulated
    res <- res + ggplot2::geom_ribbon(data = melt_summary_rv, ggplot2::aes(x=times,ymin=min, ymax=max),col=gg_color_hue(3)[2],alpha=alpha,fill=gg_color_hue(3)[2]) +
      ggplot2::geom_ribbon(data = melt_summary_sv, ggplot2::aes(x=times,ymin=min, ymax=max),col=gg_color_hue(3)[3],alpha=alpha,fill=gg_color_hue(3)[3]) +
      ggplot2::geom_ribbon(data = melt_summary_iv, ggplot2::aes(x=times,ymin=min, ymax=max),col=gg_color_hue(3)[1],alpha=alpha,fill=gg_color_hue(3)[1])

    ## Add the mean line from simulated if required
    if(include.line){
      res <- res + ggplot2::geom_line(data = melt_summary_sv, ggplot2::aes(x=times,y=value,color="S"),linetype = "dashed",size=size) +
        ggplot2::geom_line(data = melt_summary_rv, ggplot2::aes(x=times,y=value,color="R"),linetype = "dashed",size=size) +
        ggplot2::geom_line(data = melt_summary_iv, ggplot2::aes(x=times,y=value,color="I"),linetype = "dashed",size=size)
    }


  }


  ## Add title if provided
  if(!is.null(title)){
    res <- res + ggplot2::ggtitle(title)
  }

  return(res)
}
