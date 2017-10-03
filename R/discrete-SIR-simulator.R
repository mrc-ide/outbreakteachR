#' Discrete in time SIR simulator
#'
#' \code{discrete_SIR_simulator} takes output of \code{first_infection_list} and
#' \code{outbreak_dataset_read} and simulates a discrete time SIR epidemic time
#' series using a user-provided R0, population size (N), number of initial seed
#' infections (I) and time of seeding (seed.hour). The simulation uses a poisson
#' distribrution to describe the number of secondary cases, the generation times
#' and recovery times. The mean for the secondary cases is equal to R0, and the
#' mean for the generation and recovery times is calculated from the user-provided
#' first_infection_list and outbreak.dataset.
#'
#' @param R0 Reproductive number. Default = 1.8
#' @param N Total population size. If NULL (default) then the total population
#' is the same as the provided datasets.
#' @param I Number of initial seed infections. Default = 3
#' @param seed.hour Hour at which seeding of epidemic begins. Default = 9
#' @param first_infection_list Infection list outputted by \code{first_infection_list}
#' @param outbreak.dataset Outbreak dataset outputted by \code{outbreak_dataset_read}
#' @param sampling Boolean determining if recovery and generation times should be sampled
#' from the observed or drawn fro a poisson with mean equal to mean of the observed.
#' Default = FALSE (poisson draws used)
#' @param exponential Boolean determining if infection is exponential or not. If
#' FALSE (Default) then the number of secondary infections from an individual is
#' takes into account S/N
#'
#' @export
#'
#' @aliases discrete_SIR_simulator
#'

discrete_SIR_simulator <- function(R0 = 1.8, N = NULL, I = 3, seed.hour = NULL, first_infection_list,
                                   outbreak.dataset, sampling = FALSE, exponential = FALSE)
{

  # Initial conditions If no N is provided, then the total population is the same
  # as the provided datasets
  if (is.null(N))
  {
    N <- max(outbreak.dataset$ID)
    seed.hour <- min(first_infection_list$linelist$Infection_Hours.since.start,
                     na.rm = T)
  }

  # Set S and R
  S <- N - I
  R <- 0

  # Create vector of all discrete times from 0 to end of infection list
  times <- sort(c(0:max(first_infection_list$linelist$End_Infection_Hours.since.start,
                        na.rm = TRUE), seed.hour))

  # Create result vectors
  Sv <- rep(S, length(times))
  Iv <- rep(I, length(times))
  Rv <- rep(R, length(times))

  # Create vector of generation times from the outbreak dataset
  Generation_Times <- outbreak.dataset$Generation_Time_Hours
  # Remove the NAs
  Generation_Times <- Generation_Times[!is.na(Generation_Times)]

  # Create vector of infectious period hours
  Infectious_Periods <- outbreak.dataset$Infectious_Period_Hours
  # Remove the NAs
  Infectious_Periods <- Infectious_Periods[!is.na(Infectious_Periods)]

  # Create vector of Recovery Times
  Recovery_Times <- outbreak.dataset$End_Infection_Hours.since.start - outbreak.dataset$Infection_Hours.since.start
  # Remove the NAs
  Recovery_Times <- Recovery_Times[!is.na(Recovery_Times)]

  ## Distribtuion means for recovery, generation and infectious times
  mean.generation.time <- mean(Generation_Times, na.rm = T)  ## mean for poisson distribution describing average generation time in hours
  mean.infectious.period <- mean(Infectious_Periods, na.rm = T)  ## mean for poisson distribution describing average infectious period in hours
  mean.recovery.time <- mean(Recovery_Times, na.rm = T)  ## mean for poisson distribution describing average recovery time in hours

  ## Handle seed time if provided start at start seed time
  if (is.numeric(seed.hour))
  {
    start <- seed.hour
    seed.vp <- match(seed.hour, times)
    Sv[1:(seed.vp - 1)] <- N
    Iv[1:(seed.vp - 1)] <- 0
  } else {
    start <- 0
  }

  ## End time for simulation
  end <- max(times)

  ## Initialisation

  ## If exponential we want to weight the probability of an infection occurring by
  ## the proportion of uninfected contacts
  if (exponential == FALSE)
  {

    # First let's draw how many infections could occur from the number of infected
    # seeds
    possible.new.infections <- rpois(n = I, lambda = R0)

    # Then let's work out the probability that those infections occur, as S/N and
    # then remove any that are negative, i.e. S < 0
    prob_of_succesful_contact <- (S:(S - sum(possible.new.infections) + 1))/N
    prob_of_succesful_contact[prob_of_succesful_contact < 0] <- 0

    # Now for each probability of infection let's draw whether the infection happened
    new.infections <- sapply(X = prob_of_succesful_contact, function(x)
    {
      return(rbinom(n = 1, size = 1, prob = x))
    })

    ## We will now need to readjust the possible new infections in light of any failed
    ## succesful contacts.  For this we will create a cumulative infections counter,
    ## and then work along the probability of succesful contacts vector increasing the
    ## cumulative infections counter as we go along

    cum.infs <- 0
    # looping through the possible infections vector
    for (inf in 1:length(possible.new.infections))
    {
      # if this individual had infections let's decide how many were succesful
      if (possible.new.infections[inf] > 0)
      {
        # update the possible infections to be the sum of the new.infections vector
        # positions that relate to the possible infections, i.e. if the first
        # possible.ne.infections vector element is 2 we will want new.infections[1:2],
        # and then next time we will look to new.infections[3:...]
        possible.new.infections[inf] <- sum(new.infections[(cum.infs + 1):(cum.infs + possible.new.infections[inf])])

        # update our cumulative counter
        cum.infs <- cum.infs + possible.new.infections[inf]
      }
    }
    ## If it's not exponential then simply draw the infections
  } else {
    possible.new.infections <- rpois(n = I, lambda = R0)
  }

  ## If we are sampling then draw infection and recovery times (here defined as the
  ## time from infectiousness to recovery) from the observed
  if (sampling == TRUE)
  {
    # loop through the infections and generate generation times for each set of infections from those who were infected today
    infection.times <- lapply(possible.new.infections, function(x)
    {
      return(sort(round(sample(Generation_Times, size = x, replace = T) + ceiling(start))))
    })

    # loop through the infection times and generate recovery times as the infectious period plus the last, i.e. max, infection
    # if there were no infections from an infected individual then add the infectious period to the current time
    recovery.times <- unlist(lapply(infection.times, function(x)
    {
      recovery.time.greater.than.last.infection.check <- 0
      while(recovery.time.greater.than.last.infection.check==0){

        # did they not infect anyone, because if not then draw a recovery time
        if(length(x)==0){
          rec.time <- round(sample(x = Recovery_Times, size = 1, replace = T) + ceiling(start))
          recovery.time.greater.than.last.infection.check <- 1
          # if they infected anyone then draw their infectious period
        } else {

          # if we're sampling we have to catch if the infection times are too far apart to be captured by the infectious period
          # so we first draw an infectious period, but if the sampled infection times are too far apart that the recorded infectious
          # periods do not capture this then we resort to sampling a recovery time

          if(diff(range(x)) > max(Infectious_Periods)){
            rec.time <- round(sample(x = Recovery_Times[round(Recovery_Times)>=diff(range(x))], size = 1, replace = T) + ceiling(start))
          } else {
            rec.time <- round(sample(x = Infectious_Periods[Infectious_Periods>diff(range(x))], size = 1, replace = T) + min(x))
          }

          if(rec.time >= max(x)){
            recovery.time.greater.than.last.infection.check <- 1
          }
        }

      }
      return(rec.time)
    }))

    infection.times <- unlist(infection.times)
    ## If not sampling then we simply draw from the poisson with the mean equal to the
    ## mean of the observed
  } else {
    infection.times <- lapply(possible.new.infections, function(x)
    {
      return(sort(round(rpois(x, lambda = mean.generation.time) + ceiling(start))))
    })

    recovery.times <- unlist(lapply(infection.times, function(x)
    {
      recovery.time.greater.than.last.infection.check <- 0
      while(recovery.time.greater.than.last.infection.check==0){

        # did they not infect anyone, because if not then draw a recovery time
        if(length(x)==0){
          rec.time <- round(rpois(n = 1,lambda = mean.recovery.time) + ceiling(start))
          recovery.time.greater.than.last.infection.check <- 1
          # if they infected anyone then draw their infectious period
        } else {
          rec.time <- round(rpois(n=1, lambda = mean.infectious.period) + min(x))
          if(rec.time >= max(x)){
            recovery.time.greater.than.last.infection.check <- 1
          }
        }
      }
      return(rec.time)
    }))

    infection.times <- unlist(infection.times)
  }

  ## Calculate the next event and update start timer
  next.event <- min(c(infection.times, recovery.times))
  if (next.event != ceiling(start))
  {
    start <- start + 1
  }

  # create boolean to end simulation early if it is finished
  stop_simulation <- FALSE

  ## Main loop
  for (current.hour in ceiling(start):end)
  {

    ## for reproducibility this is done in case non discrete time steps are to be used
    vp <- match(current.hour, times)

    ## Copy last hour initially
    Sv[vp] <- Sv[vp - 1]
    Iv[vp] <- Iv[vp - 1]
    Rv[vp] <- Rv[vp - 1]

    if (sum(Sv[vp], Iv[vp], Rv[vp]) != N)
    {
      stop("N not constant")
    }

    ## if there is an event for the day
    if (next.event == current.hour)
    {

      ## handle infection next event
      ## -----------------------------------------------------------------------------------------------
      while (is.element(current.hour, infection.times))
      {

        # How many people are being infected now
        now.infections <- sum(infection.times == current.hour)

        # Remove the current hour infection times
        infection.times <- infection.times[!infection.times == current.hour]

        # Update the S and I according to number of infections in this hour
        Sv[vp] <- Sv[vp] - now.infections
        Iv[vp] <- Iv[vp] + now.infections

        ############## If exponential we want to weight the probability of an infection occurring by
        ############## the proportion of uninfected contacts
        if (exponential == FALSE)
        {

          # First let's draw how many infections could occur from the number of infected
          # seeds
          possible.new.infections <- rpois(n = now.infections, lambda = R0)

          # Then let's work out the probability that those infections occur, as S/N and
          # then remove any that are negative, i.e. S < 0
          prob_of_succesful_contact <- (Sv[vp]:(Sv[vp] - sum(possible.new.infections) + 1))/N
          prob_of_succesful_contact[prob_of_succesful_contact < 0] <- 0

          # Now for each probability of infection let's draw whether the infection happened
          new.infections <- sapply(X = prob_of_succesful_contact, function(x)
          {
            return(rbinom(n = 1, size = 1, prob = x))
          })

          ## We will now need to readjust the possible new infections in light of any failed
          ## succesful contacts.  For this we will create a cumulative infections counter,
          ## and then work along the probability of succesful contacts vector increasing the
          ## cumulative infections counter as we go along

          cum.infs <- 0
          # looping through the possible infections vector
          for (inf in 1:length(possible.new.infections))
          {
            # if this individual had infections let's decide how many were succesful
            if (possible.new.infections[inf] > 0)
            {
              # update the possible infections to be the sum of the new.infections vector
              # positions that relate to the possible infections, i.e. if the first
              # possible.ne.infections vector element is 2 we will want new.infections[1:2],
              # and then next time we will look to new.infections[3:...]
              possible.new.infections[inf] <- sum(new.infections[(cum.infs + 1):(cum.infs + possible.new.infections[inf])])

              # update our cumulative counter
              cum.infs <- cum.infs + possible.new.infections[inf]
            }
          }
          ## If it's not exponetial then simply draw the infections
        } else {
          possible.new.infections <- rpois(n = now.infections, lambda = R0)
        }

        ## If we are sampling then draw infection and recovery times (here defined as the
        ## time from infectiousness to recovery) from the observed
        if (sampling == TRUE)
        {
          # loop through the infections and generate generation times for each set of infections from those who were infected today
          infection.times.new <- lapply(possible.new.infections, function(x)
          {
            return(sort(round(sample(Generation_Times, size = x, replace = T) + current.hour)))
          })


          # loop through the infection times and generate recovery times as the infectious period plus the last, i.e. max, infection
          # if there were no infections from an infected individual then add the infectious period to the current time
          recovery.times.new <- unlist(lapply(infection.times.new, function(x)
          {
            recovery.time.greater.than.last.infection.check <- 0
            while(recovery.time.greater.than.last.infection.check==0){

              # did they not infect anyone, because if not then draw a recovery time
              if(length(x)==0){
                rec.time <- round(sample(x = Recovery_Times, size = 1, replace = T) + current.hour)
                recovery.time.greater.than.last.infection.check <- 1
                # if they infected anyone then draw their infectious period
              } else {

                # if we're sampling we have to catch if the infection times are too far apart to be captured by the infectious period
                # so we first draw an infectious period, but if the sampled infection times are too far apart that the recorded infectious
                # periods do not capture this then we resort to sampling a recovery time

                if(diff(range(x)) > max(Infectious_Periods)){
                  rec.time <- round(sample(x = Recovery_Times[round(Recovery_Times)>=diff(range(x))], size = 1, replace = T) + current.hour)
                } else {
                  rec.time <- round(sample(x = Infectious_Periods[Infectious_Periods>diff(range(x))], size = 1, replace = T) + min(x))
                }

                if(rec.time >= max(x)){
                  recovery.time.greater.than.last.infection.check <- 1
                }
              }

            }

            return(rec.time)

          }))

          infection.times <- c(infection.times, unlist(infection.times.new))
          recovery.times <- c(recovery.times, recovery.times.new)

          ## If not sampling then we simply draw from the poisson with the mean equal to the
          ## mean of the observed
        } else {

          # loop through the infections and generate generation times for each set of infections from those who were infected today
          infection.times.new <- lapply(possible.new.infections, function(x)
          {
            return(sort(round(rpois(x, lambda = mean.generation.time) + current.hour)))
          })


          # loop through the infection times and generate recovery times as the infectious period plus the last, i.e. max, infection
          # if there were no infections from an infected individual then add the infectious period to the current time
          recovery.times.new <- unlist(lapply(infection.times.new, function(x)
          {

            # We have to genearate a recovery time that is greater than the last infection time
            # This can take very long sometimes so there is a basic catch in to stop after 10 failed attempts
            recovery.time.greater.than.last.infection.check <- 0
            infinite_catch <- 0
            while(recovery.time.greater.than.last.infection.check==0){

              # did they not infect anyone, because if not then draw a recovery time
              if(length(x)==0){
                rec.time <- round(rpois(n = 1,lambda = mean.recovery.time) + current.hour)
                recovery.time.greater.than.last.infection.check <- 1
                # if they infected anyone then draw their infectious period
              } else {
                rec.time <- round(rpois(n = 1,lambda = mean.infectious.period) + min(x))
                if(rec.time >= max(x)){
                  recovery.time.greater.than.last.infection.check <- 1
                }

                infinite_catch <- infinite_catch + 1
                if(infinite_catch>10){
                    rec.time <- max(x)
                    recovery.time.greater.than.last.infection.check <- 1
                }
              }
            }

            return(rec.time)

          }))

          infection.times <- c(infection.times, unlist(infection.times.new))
          recovery.times <- c(recovery.times, recovery.times.new)
        }
        ##############

        # If there are more infection times than people to infect, then sort the
        # infection times and take the earliest x, where x is the number of susceptibles
        # left
        if (length(infection.times) > Sv[vp])
        {
          infection.times <- head(sort(infection.times), Sv[vp])
        }

      }

      ## handle recovery next event
      ## -----------------------------------------------------------------------------------------------
      if (is.element(current.hour, recovery.times))
      {

        # How many people are recovering now
        new.recoveries <- sum(recovery.times == current.hour)

        # Remove those times that are recovering in this hour
        recovery.times <- recovery.times[!recovery.times == current.hour]

        # Update recovered and infected accordingly
        Rv[vp] <- Rv[vp] + new.recoveries
        Iv[vp] <- Iv[vp] - new.recoveries

      }

      ## update next event or break out of sim loop if no more events
      if (length(c(infection.times, recovery.times)) == 0)
      {

        ## if we are on the last time step then we don't do this
        if (vp != length(times))
        {
          Sv[(vp + 1):length(Sv)] <- Sv[vp]
          Iv[(vp + 1):length(Iv)] <- Iv[vp]
          Rv[(vp + 1):length(Rv)] <- Rv[vp]
        }
        stop_simulation <- TRUE

      }
      if (stop_simulation) break

      ## find out next event time
      next.event <- min(c(infection.times, recovery.times))

    }


  }

  # return results
  return(data.frame(Sv, Iv, Rv, times))

}
