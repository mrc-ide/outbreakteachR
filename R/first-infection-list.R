#' Format read outbreak dataset into first infections
#'
#' \code{first_infection_list} takes output of \code{outbreak_dataset_read} and
#' splits into linelist and contacts for further use, removing reinfections.
#'
#' @param outbreak.dataset Outbreak dataset outputted by \code{outbreak_dataset_read}
#'
#' @export
#'
#'
#'

first_infection_list <- function(outbreak.dataset){

  # convert into linelist and contacts
  linelist <- outbreak.dataset[outbreak.dataset$Reinfection==FALSE,]
  contacts <- outbreak.dataset[,c("Parent.ID","ID","Infection_Hours.since.start")]
  contacts <- contacts[-c(which(is.na(contacts$Parent.ID)),which(outbreak.dataset$Reinfection==TRUE)),]

  # label accordingly
  colnames(contacts) <- c("From","To","Infection_Hours.since.start")

  # combine into list of outbreak dataframes
  res <- list("linelist"=linelist,"contacts"=contacts)

  return(res)

}
