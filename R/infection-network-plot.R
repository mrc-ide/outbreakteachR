#' Plots infection network
#'
#' \code{infection_network_plot} takes output of \code{first_infection_list} and
#' plots either a time accurate network, or a "tree" network with equal branches.
#'
#' @param first_infection_list Infection list outputted by \code{first_infection_list}
#' @param time Boolean dictating is time accurate network is output. If FALSE then
#' "tree" network with equal branches is plotted. Default = TRUE
#' @param log Boolean that determines if the times of infection are log transformed to
#' improve the visualisation of the non-tree plot. Default = TRUE
#' @param iterations Numeric detailing the number igraph iterations run when calculating
#' optimum layout. Default = 10000
#'
#' @export
#'
#'
#'

infection_network_plot <- function(first_infection_list, time = TRUE,log=TRUE,iterations = 10000){

  # convert into graph
  outbreak.graph <- igraph::graph_from_data_frame(first_infection_list$contacts,directed = TRUE,
    vertices = first_infection_list$linelist)


  if(time){

    ## for the sake of clean plotting let's allocate the individuals who were not infected an infection
    ## time that is 15 larger than the max infection, so that they appear on the graph cleanly:

    first_infection_list$linelist$Infection_Hours.since.start[is.na(first_infection_list$linelist$Infection_Hours.since.start)] <-
      max(first_infection_list$linelist$Infection_Hours.since.start,na.rm=TRUE) + 15

    if(log){
      # time orientated layout
      lay <- igraph::layout_with_fr(outbreak.graph, minx=log(first_infection_list$linelist$Infection_Hours.since.start),
        maxx=log(first_infection_list$linelist$Infection_Hours.since.start),niter=iterations,
        miny=rep(-Inf,length(first_infection_list$linelist$Infection_Hours.since.start)),
        maxy=rep(Inf,length(first_infection_list$linelist$Infection_Hours.since.start)))
    } else {
      lay <- igraph::layout_with_fr(outbreak.graph, minx=(first_infection_list$linelist$Infection_Hours.since.start),
        maxx=(first_infection_list$linelist$Infection_Hours.since.start),niter=iterations,
        miny=rep(-Inf,length(first_infection_list$linelist$Infection_Hours.since.start)),
        maxy=rep(Inf,length(first_infection_list$linelist$Infection_Hours.since.start)))

    }
    # create igraph using graph and layout
    res <- igraph::plot.igraph(outbreak.graph, layout=lay,vertex.size=6,edge.arrow.width=1,edge.arrow.size=0.2,vertex.label=NA,
      vertex.color="red",edge.color="black")

  } else {

    # tree orientated layout

    # first find out which individuals were the seeds - these will be the only NAs on the first day for parentID
    first.days <- which(first_infection_list$linelist$Infection_Date == unique(first_infection_list$linelist$Infection_Date)[1])
    seeds <- which(is.na(first_infection_list$linelist$Parent.ID[first.days]))

    # next because thus layout algorithm is not very good we will loop it until it correctly levels the tree
    # the correct number of levels can be calculated as follows:
    correct.levels <- max(unlist(lapply(igraph::all_simple_paths(outbreak.graph,from = seeds[1]),length)))
    for(i in 2:length(seeds)){
    correct.levels <- max(correct.levels,max(unlist(lapply(igraph::all_simple_paths(outbreak.graph,from = seeds[i]),length))))
    }

    tree <- cbind(c(0,0),c(0,0))
    while(length(unique(tree[,2]))<=correct.levels){
      tree <- igraph::layout_as_tree(outbreak.graph,root = seeds,rootlevel = rep(1,3))
      tree[is.na(tree[,1]),1] <- min(tree[,1],na.rm=T)
      tree[(max(first_infection_list$contacts[,1:2])+1):dim(tree)[1],1] <- 0
    }
    if(sum(is.na(first_infection_list$linelist$Parent.ID))>0){
      title = "Note: All uninfected individuals \nare represented by one node"
    }

    # create igraph using graph and tree layout
    res <-  igraph::plot.igraph(outbreak.graph, layout=tree,vertex.size=6,edge.arrow.width=1,edge.arrow.size=0.2,vertex.label=NA,
      vertex.color="red",edge.color="black",main=title)

    # view in tkplot to alter as needed before exporting and editing in inkscape
    # Only to be used if the iteration in the plot above is unsatisfactory for limiting cross over
    # igraph::tkplot(outbreak.graph, layout=lay,vertex.size=6,edge.arrow.width=1,edge.arrow.size=0.4,vertex.label=NA)

  }



}
