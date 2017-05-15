#' Creates interactive html file with d3 network animation
#'
#' \code{animate_infection_network} takes output of \code{first_infection_list} and
#' produces a network animation of the outbreak using d3. Output is saved to file
#' as an html page
#'
#' N.B. Function
#'
#' @param first_infection_list Infection list outputted by \code{first_infection_list}.
#' @param file Full file path where html is to be saved to.
#' @param year Year the outbreak relates to for title plotting. Default = NULL
#' @param detach Boolean that detaches the packages loaded within the library.
#' Deafult = FALSE, i.e. packages remain in Global Environment.
#'
#' @export
#'
#'
#'

animate_infection_network <- function(first_infection_list, file, year=NULL, detach = FALSE){

  # if user has not provided .html file ending add ".html"
  if(tail(unlist(strsplit(file,".",fixed=T)),1) != "html"){
    file <- paste(file,".html",sep="")
  }

  # first we have to ammend those who were never infected to have timing of start and end infection to be infinity
  uninfected <- which(is.na(first_infection_list$linelist$Infection_Hours.since.start))
  first_infection_list$linelist[uninfected,]$Infection_Hours.since.start <- Inf
  first_infection_list$linelist[uninfected,]$End_Infection_Hours.since.start <- Inf


  # create initially network so that node colors can be added to the vertices
  paper.net <- network::network(first_infection_list$contacts,  vertex.attr=first_infection_list$linelist, matrix.type="edgelist",
                                loops=F, multiple=F, ignore.eval = F)

  # Now add the vertices that have no contacts
  network::add.vertices(paper.net, nv = length(uninfected))

  # And set their ID attributes
  network::set.vertex.attribute(paper.net,"vertex.names",value = uninfected,v = uninfected)

  # add vertex color attributes related to onset of infection and recovery
  networkDynamic::activate.vertex.attribute(paper.net,"color","blue",onset=-Inf,terminus=Inf)
  networkDynamic::activate.vertex.attribute(paper.net,"color","red",
                                            onset=first_infection_list$linelist$Infection_Hours.since.start,terminus=Inf)
  networkDynamic::activate.vertex.attribute(paper.net,"color","green",
                                            onset=first_infection_list$linelist$End_Infection_Hours.since.start,terminus=Inf)

  # create network dynamics
  paper.net.dyn <- networkDynamic::networkDynamic(
    base.net = paper.net,
    edge.spells=data.frame("onset" = first_infection_list$contacts$Infection_Hours.since.start,
                           "terminus" = range(first_infection_list$linelist$End_Infection_Hours.since.start,finite=T)[2]+0.5,
                           "tail"=first_infection_list$contacts$From,
                           "head"=first_infection_list$contacts$To),
    vertex.spells=data.frame(onset=0,
                             terminus=range(first_infection_list$linelist$End_Infection_Hours.since.start,finite=T)[2]+0.5,
                             vertex.id=1:dim(first_infection_list$linelist)[1])
  )

  # precompute animation slices
  ndtv::compute.animation(paper.net.dyn, animation.mode = "kamadakawai",
                          slice.par=list(start=0,end=range(first_infection_list$linelist$End_Infection_Hours.since.start,finite=T)[2],
                                         interval=1, aggregate.dur=1, rule='latest'),
                          vertex.col="color")

  # render d3 movie and save to file paramter
  ndtv::render.d3movie(paper.net.dyn, usearrows = T,
                       displaylabels = T,
                       vertex.col = "color",
                       launchBrowser=T, filename=file,
                       render.par=list(tween.frames = 1, show.time = T),
                       plot.par=list(mar=c(0,0,0,0)),
                       main = paste("Outbreak Dynamics",year,". Susceptible (blue), Infected (red), Recovered (green)"),
                       cex.main = 2)

}
