##'  Paper outbreak practical data
##'
##'  These datasets represent the data collected by the Imperial College Masters paper outbreak practical.
##'
##'  This dataset is meant for teaching purposes; it  does not represent a real outbreak, but a simulated one
##'  for teaching purposes.
##'
##'
##' @docType data
##'
##' @format A dataframe of 17 variables:
##'
##'   \code{$paper.outbreak.2016}: A dataframe of cases and their attributes.
##'     \itemize{
##'       \item ID: Unique identifier
##'       \item Parent.ID: Unique identifier of infector
##'       \item Reinfection: Reinfection boolean
##'       \item Infection_Date: Date of infection
##'       \item Infection_Time: Time of infection
##'       \item Infection_Hours.since.start: Total hours since start of outbreak till infection
##'       \item Symptoms_Date: Date of symptoms
##'       \item Symptoms_Time: Time of symptoms
##'       \item Symptoms_Hours.since.start:  Total hours since start of outbreak till symptomatic
##'       \item End_Infection_Date: Date of end of infection
##'       \item End_Infection_Time: Time of end of infection
##'       \item End_Infection_Hours.since.start: Total hours since start of outbreak till end of infection
##'       \item Onward_Infection_Hours.since.start: Total hours since start of outbreak till onward infection
##'       \item Latent_Period_Hours: Latent period in hours
##'       \item Incubation_Period_Hours: Incubation period in hours
##'       \item Infectious_Period_Hours: Infectious period in hours
##'       \item Generation_Time_Hours: Generation time in hours
##'     }
##'
##' @rdname paper.outbreak.2016
##'
##' @aliases paper.outbreak.2016
##'
##' @author Data collected by Dr Ide Cremin (\email{ide.cremin05@imperial.ac.uk}), and collated by
##' Mr Oliver Watson (\email{o.watson15@imperial.ac.uk})
##'
##' @references More information on the outbreak in the following reference:
##' TBA
##'
##' @examples
##' ## show the line list describing paper outbreak cases and their attributes
##' str(paper.outbreak.2016)
##'
##'
"paper.outbreak.2016"
