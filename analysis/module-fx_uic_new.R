
##
## Simple Cost-effectiveness Model (Simple SI model with cost and utility tracking)
## EpiModel Gallery (https://github.com/statnet/EpiModel-Gallery)
##
## Author: Gregory Knowlton (University of Minnesota)
## Date: October 2021
##

# Cost/Utility Tracking Module ------------------------------------------------


# Updated Departure Module -----------------------------------------------------

dfunc <- function(dat, at) {

  ## Attributes ##
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")
  ART.status <- get_attr(dat, "ART.status")
  stage <- get_attr(dat, "stage")
  stage.time <- get_attr(dat, "stage.time")
  exitTime <- get_attr(dat, "exitTime")


  ## Parameters ##
  departure.rate <- get_param(dat, "departure.rate")
  ART.Progression.Reduction.Rate <- get_param(dat, "ART.Progression.Reduction.Rate")
  AIDSToDepart.Rate <- get_param(dat, "AIDSToDepart.Rate")
  Departure.rates <- rep(departure.rate, network.size(dat$nw[[1]]))

  ## Initialize vectors ##
  vecDeparture <- vector()
  idsDeparture <- vector()
  vecDepartAIDSART <- vector()
  idsDepartAIDSART <- vector()
  vecDepartAIDSNoART <- vector()
  idsDepartAIDSNoART <- vector()

  ## Query active individuals not in the AIDS stage of HIV ##
  idsEligDepartStandard <- which(active == 1 & ((stage != "AIDS" &
                                                   !is.na(stage)) |
                                                  status == "s"))


  ## Departure process for individuals not in the AIDS stage of HIV
  if (length(idsEligDepartStandard) > 0) {
    Departure_rates_of_elig <- Departure.rates[idsEligDepartStandard]
    vecDeparture <- which(rbinom(idsEligDepartStandard, 1,
                                 Departure_rates_of_elig) == 1)

    if (length(vecDeparture) > 0) {
      idsDeparture <- idsEligDepartStandard[vecDeparture]
    }
  }


  ## Departure process for individuals in the AIDS stage of HIV on ART
  idsEligDepartAIDSART <- which(active == 1 & stage == "AIDS" &
                                  stage.time != 0 & ART.status == 1 &
                                  !is.na(ART.status))

  if (length(idsEligDepartAIDSART) > 0) {
    vecDepartAIDSART <- which(rbinom(length(idsEligDepartAIDSART), 1,
                                     AIDSToDepart.Rate *
                                       ART.Progression.Reduction.Rate) == 1)

    if (length(vecDepartAIDSART) > 0) {
      idsDepartAIDSART <- idsEligDepartAIDSART[vecDepartAIDSART]
    }
  }


  ## Departure process for individuals in the AIDS stage of HIV not on ART
  idsEligDepartAIDSNoART <- which(active == 1 & stage == "AIDS" &
                                    stage.time != 0 & ART.status == 0 &
                                    !is.na(ART.status))

  if (length(idsEligDepartAIDSNoART) > 0) {
    vecDepartAIDSNoART <- which(rbinom(length(idsEligDepartAIDSNoART), 1,
                                       AIDSToDepart.Rate) == 1)

    if (length(vecDepartAIDSNoART) > 0) {
      idsDepartAIDSNoART <- idsEligDepartAIDSNoART[vecDepartAIDSNoART]
    }
  }


  ##Save departure summary statistics
  dat$epi$depart.standard.ART.flow[at] <-
    ifelse(length(idsEligDepartStandard) > 0 & length(vecDeparture) > 0,
           length(which(ART.status[idsDeparture] == 1)), 0)
  dat$epi$depart.standard.NoART.flow[at] <-
    ifelse(length(idsEligDepartStandard) > 0 & length(vecDeparture) > 0,
           length(which(ART.status[idsDeparture] == 0 |
                          is.na(ART.status[idsDeparture]))), 0)
  dat$epi$depart.AIDS.ART.flow[at] <-
    ifelse(length(idsEligDepartAIDSART) > 0 & length(vecDepartAIDSART) > 0,
           length(idsDepartAIDSART), 0)
  dat$epi$depart.AIDS.NoART.flow[at] <-
    ifelse(length(idsEligDepartAIDSNoART) > 0 & length(vecDepartAIDSART) > 0,
           length(idsDepartAIDSNoART), 0)


  ## Update nodal attributes on attr and networkDynamic object ##
  if (length(idsDeparture) > 0 || length(idsDepartAIDSART) > 0 ||
      length(idsDepartAIDSNoART) > 0) {
    idsDeparted <- c(idsDeparture, idsDepartAIDSART, idsDepartAIDSNoART)
    active[idsDeparted] <- 0
    exitTime[idsDeparted] <- at
  }

  ## Save updated status attribute ##
  dat <- set_attr(dat, "active", active)
  dat <- set_attr(dat, "exitTime", exitTime)

  return(dat)
}


# Updated Arrivals Module ----------------------------------------------------

afunc <- function(dat, at) {

  ## Parameters
  n <- sum(get_attr(dat, "active") == 1)
  a.rate <- get_param(dat, "arrival.rate")

  ## Process
  nArrivalsExp <- n * a.rate
  nArrivals <- rpois(1, nArrivalsExp)

  ## Update attributes
  if (nArrivals > 0) {
    dat <- append_core_attr(dat, at, nArrivals)
    dat <- append_attr(dat, "status", "s", nArrivals)
    dat <- append_attr(dat,"ART.status", 0, nArrivals)
    dat <- append_attr(dat, "infTime", NA, nArrivals)
   # dat <- append_attr(dat, "age", 16, nArrivals)
    dat <- append_attr(dat,"ART.time", NA, nArrivals)
    dat <- append_attr(dat, "poppers", sample(1:2,nArrivals,replace=TRUE), nArrivals)
  }

  poppers <- get_attr(dat, "poppers")
  
  ## Summary statistics
  dat <- set_epi(dat, "a.flow", at, nArrivals)
  
  return(dat)
}

# Replacement infection/transmission module -------------------------------

infect <- function(dat, at) {

  ## Attributes ##
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")
  ART.status <- get_attr(dat, "ART.status")
  stage <- get_attr(dat, "stage")
  stage.time <- get_attr(dat, "stage.time")
  ART.time <- get_attr(dat, "ART.time")
  poppers <- get_attr(dat, "poppers")

  ## Parameters ##
  inf.prob.chronic <- get_param(dat, "inf.prob.chronic")
  relative.inf.prob.acute <- get_param(dat, "relative.inf.prob.acute")
  relative.inf.prob.AIDS <- get_param(dat, "relative.inf.prob.AIDS")
  relative.inf.prob.ART <- get_param(dat, "relative.inf.prob.ART")
  act.rate <- get_param(dat, "act.rate")

  ## Find infected nodes ##
  idsInf <- which(active == 1 & status == "i")
  nActive <- sum(active == 1)
  nElig <- length(idsInf)


  ## Initialize default incidence at 0 ##
  nInf <- 0


  ## If any infected nodes, proceed with transmission ##
  if (nElig > 0 && nElig < nActive) {
    ## Look up discordant edgelist ##
    del <- discord_edgelist(dat, at)

    ## If any discordant pairs, proceed ##
    if (!(is.null(del))) {

      # Set parameters on discordant edgelist data frame
      # dependent on HIV status and ART status

      #Create Empty transProb Column
      del$transProb <- rep(NA, length(del$inf))

      #Trans Prob - Acute, No ART
      idsAcuteNoART <- which(stage[del$inf] == "acute" & ART.status[del$inf] == 0)
      del$transProb[idsAcuteNoART] <- inf.prob.chronic * relative.inf.prob.acute

      #Trans Prob - Chronic, No ART
      idsChronicNoART <- which(stage[del$inf] %in%
                                 c("chronic1", "chronic2") & ART.status[del$inf] == 0)
      del$transProb[idsChronicNoART] <- inf.prob.chronic

      #Trans Prob - AIDS, No ART
      idsAIDSNoART <- which(stage[del$inf] == "AIDS" &
                              ART.status[del$inf] == 0)
      del$transProb[idsAIDSNoART] <- inf.prob.chronic * relative.inf.prob.AIDS

      #Trans Prob - Acute, ART
      idsAcuteART <- which(stage[del$inf] == "acute" & ART.status[del$inf] == 1)
      del$transProb[idsAcuteART] <- inf.prob.chronic * relative.inf.prob.acute *
        relative.inf.prob.ART

      #Trans Prob - Chronic, ART
      idsChronicART <- which(stage[del$inf] %in% c("chronic1", "chronic2") &
                               ART.status[del$inf] == 1)
      del$transProb[idsChronicART] <- inf.prob.chronic * relative.inf.prob.ART

      #Trans Prob - AIDS, ART
      idsAIDSART <- which(stage[del$inf] == "AIDS" & ART.status[del$inf] == 1)
      del$transProb[idsAIDSART] <- inf.prob.chronic *
        relative.inf.prob.AIDS * relative.inf.prob.ART
      del$actRate <- act.rate
      del$finalProb <- 1 - (1 - del$transProb)^del$actRate

      # Stochastic transmission process
      transmit <- rbinom(nrow(del), 1, del$finalProb)

      # Keep rows where transmission occurred
      del <- del[which(transmit == 1), ]

      # Look up new ids if any transmissions occurred
      idsNewInf <- unique(del$sus)
      nInf <- length(idsNewInf)

      # Set new attributes for those newly infected
      if (nInf > 0) {
        stage[idsNewInf] <- "acute"
        status[idsNewInf] <- "i"
        stage.time[idsNewInf] <- 0
        ART.status[idsNewInf] <- 0
        ART.time[idsNewInf] <- 0

      }

      # Track new infected 
      dat <- set_epi(dat, "si.flow", at, nInf)
      dat <- set_epi(dat, "si.flow.poppers1", at, sum(poppers[idsNewInf]==1, na.rm = TRUE))
      dat <- set_epi(dat, "si.flow.poppers2", at, sum(poppers[idsNewInf]==2, na.rm = TRUE))
      
    }
  }

  #Increment ART time by 1
  ART.time <- ifelse(!is.na(ART.time), ART.time + 1, ART.time)

  #Increment status time by 1
  stage.time <- ifelse(!is.na(stage.time), stage.time + 1, stage.time)


  ## Update attributes
  dat <- set_attr(dat, "stage", stage)
  dat <- set_attr(dat, "status", status)
  dat <- set_attr(dat, "stage.time", stage.time)
  dat <- set_attr(dat, "ART.status", ART.status)
  dat <- set_attr(dat, "ART.time", ART.time)

  ## Save summary statistics
  dat <- set_epi(dat, "acute.flow", at, nInf)
  dat <- set_epi(dat, "s.num", at, sum(active == 1 & status == "s"))
  dat <- set_epi(dat, "acute.ART.num", at, sum(active == 1 & stage == "acute" &
                                           ART.status == 1, na.rm = TRUE))
  dat <- set_epi(dat, "acute.NoART.num", at, sum(active == 1 & stage == "acute" &
                                             status == 0, na.rm = TRUE))
  dat <- set_epi(dat, "chronic1.ART.num", at, sum(active == 1 & stage == "chronic1" &
                                                    ART.status == 1, na.rm = TRUE))
  dat <- set_epi(dat, "chronic1.NoART.num", at, sum(active == 1 & stage == "chronic1" &
                                                      ART.status == 0, na.rm = TRUE))
  dat <- set_epi(dat, "chronic2.ART.num", at, sum(active == 1 & stage == "chronic2" &
                                                    ART.status == 1, na.rm = TRUE))
  dat <- set_epi(dat, "chronic2.NoART.num", at, sum(active == 1 & stage == "chronic2" &
                                                      ART.status == 0, na.rm = TRUE))
  dat <- set_epi(dat, "AIDS.ART.num", at, sum(active == 1 & stage == "AIDS" &
                                                ART.status == 1, na.rm = TRUE))
  dat <- set_epi(dat, "AIDS.NoART.num", at, sum(active == 1 & stage == "AIDS" &
                                                  ART.status == 0, na.rm = TRUE))
  return(dat)

}


# HIV progression module ------------------------------------------
# (Replaces the recovery module)

progress <- function(dat, at) {

  ## Attributes ##
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")

  ## Initiating indicators to track stage of HIV and ART status ##
  if (at == 2) {

    dat <- set_attr(dat, "stage", ifelse(dat$attr$status == "i", "acute", NA))
    dat <- set_attr(dat, "ART.status", ifelse(dat$attr$status == "i", 0, NA))
    dat <- set_attr(dat, "ART.time", ifelse(!is.na(dat$attr$ART.status) &
                                              dat$attr$ART.status == 0, 1, NA))
    dat <- set_attr(dat, "stage.time", ifelse(dat$attr$status == "i", 1, NA))
  }

  ## Attributes ##
  ART.status <- get_attr(dat, "ART.status")
  stage <- get_attr(dat, "stage")
  ART.time <- get_attr(dat, "ART.time")
  stage.time <- get_attr(dat, "stage.time")
  poppers <- get_attr(dat, "poppers")

  ## Progression Parameters ##

  AcuteToChronic1.Rate <- get_param(dat, "AcuteToChronic1.Rate")
  Chronic1ToChronic2.Rate <- get_param(dat, "Chronic1ToChronic2.Rate")
  Chronic2ToAIDS.Rate <- get_param(dat, "Chronic2ToAIDS.Rate")
  ART.Treatment.Rate.P1 <- get_param(dat, "ART.Treatment.Rate.P1")
  ART.Treatment.Rate.P2 <- get_param(dat, "ART.Treatment.Rate.P2")
  ART.Discontinuance.Rate <- get_param(dat, "ART.Discontinuance.Rate")
  ART.Progression.Reduction.Rate <- get_param(dat, "ART.Progression.Reduction.Rate")


  ## Initialize vectors ##
  vecART <- vector()
  vecARTDisc <- vector()
  idsART <- vector()
  idsARTDisc <- vector()
  vecChronic1ART <- vector()
  idsChronic1ART <- vector()
  vecChronic1NoART <- vector()
  idsChronic1NoART <- vector()
  vecChronic2ART <- vector()
  idsChronic2ART <- vector()
  vecChronic2NoART <- vector()
  idsChronic2NoART <- vector()
  vecAIDSART <- vector()
  idsAIDSART <- vector()
  vecAIDSNoART <- vector()
  idsAIDSNoART <- vector()

  ## ART Treatment ##
  idsEligART.P1 <- which(poppers == 1 & active == 1 & status == "i" & ART.time != 0 &
                        ART.status == 0 & !is.na(ART.status))

  idsEligART.P2 <- which(poppers == 2 & active == 1 & status == "i" & ART.time != 0 &
                        ART.status == 0 & !is.na(ART.status))

  if (length(idsEligART.P1) > 0) {
    vecART.P1 <- which(rbinom(length(idsEligART.P1), 1, ART.Treatment.Rate.P1) == 1)
    if (length(vecART.P1) > 0) {
      idsART.P1 <- idsEligART.P1[vecART.P1]
      ART.status[idsART.P1] <- 1
      ART.time[idsART.P1] <- 0
    }
  }

  if (length(idsEligART.P2) > 0) {
    vecART.P2 <- which(rbinom(length(idsEligART.P2), 1, ART.Treatment.Rate.P2) == 1)
    if (length(vecART.P2) > 0) {
      idsART.P2 <- idsEligART.P2[vecART.P2]
      ART.status[idsART.P2] <- 1
      ART.time[idsART.P2] <- 0
    }
  }

  ## ART Discontinuance ##
  idsEligARTDisc <- which(active == 1 & status == "i" &
                            ART.time != 0 &
                            ART.status == 1 & !is.na(ART.status))

  if (length(idsEligARTDisc) > 0) {
    vecARTDisc <- which(rbinom(length(idsEligARTDisc), 1,
                               ART.Discontinuance.Rate) == 1)
    if (length(vecARTDisc) > 0) {
      idsARTDisc <- idsEligARTDisc[vecARTDisc]
      ART.status[idsARTDisc] <- 0
      ART.time[idsARTDisc] <- 0
    }
  }


  ##ART Treatment/Discontinuance Flows within HIV Status Subcompartment
  acute.ART.treatment.flow <- length(intersect(idsART, which(stage == "acute")))
  acute.ART.discontinuance.flow <- length(intersect(idsARTDisc, which(stage == "acute")))
  chronic1.ART.treatment.flow <- length(intersect(idsART, which(stage == "chronic1")))
  chronic1.ART.discont.flow <- length(intersect(idsARTDisc, which(stage == "chronic1")))
  chronic2.ART.treatment.flow <- length(intersect(idsART, which(stage == "chronic2")))
  chronic2.ART.discont.flow <- length(intersect(idsARTDisc, which(stage == "chronic2")))
  AIDS.ART.treatment.flow <- length(intersect(idsART, which(stage == "AIDS")))
  AIDS.ART.discontinuance.flow <- length(intersect(idsARTDisc, which(stage == "AIDS")))


  ## Acute to chronic 1 stage progression process ##
  idsEligChronic1ART <- which(active == 1 & stage == "acute" &
                                stage.time != 0 & ART.status == 1 & !is.na(ART.status))
  idsEligChronic1NoART <- which(active == 1 & stage == "acute" &
                                  stage.time != 0 & ART.status == 0 & !is.na(ART.status))

  if (length(idsEligChronic1ART) > 0) {
    vecChronic1ART <- which(rbinom(length(idsEligChronic1ART), 1,
                                   AcuteToChronic1.Rate * ART.Progression.Reduction.Rate) == 1)

    if (length(vecChronic1ART) > 0) {
      idsChronic1ART <- idsEligChronic1ART[vecChronic1ART]
      stage[idsChronic1ART] <- "chronic1"
      stage.time[idsChronic1ART] <- 0
    }
  }

  if (length(idsEligChronic1NoART) > 0) {
    vecChronic1NoART <- which(rbinom(length(idsEligChronic1NoART), 1,
                                     AcuteToChronic1.Rate) == 1)

    if (length(vecChronic1NoART) > 0) {
      idsChronic1NoART <- idsEligChronic1NoART[vecChronic1NoART]
      stage[idsChronic1NoART] <- "chronic1"
      stage.time[idsChronic1NoART] <- 0
    }
  }


  ## Chronic 1 to chronic 2 stage progression process ##
  idsEligChronic2ART <- which(active == 1 & stage == "chronic1" &
                                stage.time != 0 & ART.status == 1 & !is.na(ART.status))
  idsEligChronic2NoART <- which(active == 1 & stage == "chronic1" &
                                  stage.time != 0 & ART.status == 0 & !is.na(ART.status))

  if (length(idsEligChronic2ART) > 0) {
    vecChronic2ART <- which(rbinom(length(idsEligChronic2ART), 1,
                                   Chronic1ToChronic2.Rate * ART.Progression.Reduction.Rate) == 1)

    if (length(vecChronic2ART) > 0) {
      idsChronic2ART <- idsEligChronic2ART[vecChronic2ART]
      stage[idsChronic2ART] <- "chronic2"
      stage.time[idsChronic2ART] <- 0
    }
  }

  if (length(idsEligChronic2NoART) > 0) {
    vecChronic2NoART <- which(rbinom(length(idsEligChronic2NoART), 1,
                                     Chronic1ToChronic2.Rate) == 1)

    if (length(vecChronic2NoART) > 0) {
      idsChronic2NoART <- idsEligChronic2NoART[vecChronic2NoART]
      stage[idsChronic2NoART] <- "chronic2"
      stage.time[idsChronic2NoART] <- 0
    }
  }


  ## Chronic 2 to AIDS stage progression process ##
  idsEligAIDSART <- which(active == 1 & stage == "chronic2" &
                            stage.time != 0 & ART.status == 1 & !is.na(ART.status))
  idsEligAIDSNoART <- which(active == 1 & stage == "chronic2" &
                              stage.time != 0 & ART.status == 0 & !is.na(ART.status))

  if (length(idsEligAIDSART) > 0) {
    vecAIDSART <- which(rbinom(length(idsEligAIDSART), 1, Chronic2ToAIDS.Rate *
                                 ART.Progression.Reduction.Rate) == 1)

    if (length(vecAIDSART) > 0) {
      idsAIDSART <- idsEligAIDSART[vecAIDSART]
      stage[idsAIDSART] <- "AIDS"
      stage.time[idsAIDSART] <- 0
    }
  }

  if (length(idsEligAIDSNoART) > 0) {
    vecAIDSNoART <- which(rbinom(length(idsEligAIDSNoART), 1, Chronic2ToAIDS.Rate) == 1)

    if (length(vecAIDSNoART) > 0) {
      idsAIDSNoART <- idsEligAIDSNoART[vecAIDSNoART]
      stage[idsAIDSNoART] <- "AIDS"
      stage.time[idsAIDSNoART] <- 0
    }
  }


  #Save attributes
  dat <- set_attr(dat, "stage", stage)
  dat <- set_attr(dat, "stage.time", stage.time)
  dat <- set_attr(dat, "ART.status", ART.status)
  dat <- set_attr(dat, "ART.time", ART.time)

  #Update summary statistics
  dat <- set_epi(dat, "acute.ART.treatment.flow", at, acute.ART.treatment.flow)
  dat <- set_epi(dat, "acute.ART.discontinuance.flow", at, acute.ART.discontinuance.flow)
  dat <- set_epi(dat, "chronic1.ART.treatment.flow", at, chronic1.ART.treatment.flow)
  dat <- set_epi(dat, "chronic1.ART.discont.flow", at,
    chronic1.ART.discont.flow)
  dat <- set_epi(dat, "chronic2.ART.treatment.flow", at, chronic2.ART.treatment.flow)
  dat <- set_epi(dat, "chronic2.ART.discont.flow", at,
    chronic2.ART.discont.flow)
  dat <- set_epi(dat, "AIDS.ART.treatment.flow", at, AIDS.ART.treatment.flow)
  dat <- set_epi(dat, "AIDS.ART.discontinuance.flow", at, AIDS.ART.discontinuance.flow)
  dat <- set_epi(dat, "chronic1.ART.flow", at, ifelse(length(idsEligChronic1ART) > 0 &
                                            length(vecChronic1ART) > 0,
                                          length(idsChronic1ART), 0))
  dat <- set_epi(dat, "chronic1.NoART.flow", at, ifelse(length(idsEligChronic1NoART) > 0 &
                                              length(vecChronic1NoART) > 0,
                                            length(idsChronic1NoART), 0))
  dat <- set_epi(dat, "chronic2.ART.flow", at, ifelse(length(idsEligChronic2ART) > 0 &
                                            length(vecChronic2ART) > 0,
                                          length(idsChronic2ART), 0))
  dat <- set_epi(dat, "chronic2.NoART.flow", at, ifelse(length(idsEligChronic2NoART) > 0 &
                                                          length(vecChronic2NoART) > 0,
                                                        length(idsChronic2NoART), 0))
  dat <- set_epi(dat, "AIDS.ART.flow", at, ifelse(length(idsEligAIDSART) > 0 & length(vecAIDSART) > 0,
                                                  length(idsAIDSART), 0))
  dat <- set_epi(dat, "AIDS.NoART.flow", at, ifelse(length(idsEligAIDSNoART) > 0 &
                                          length(vecAIDSNoART) > 0,
                                        length(idsAIDSNoART), 0))

  return(dat)
}

## TRACKERS ##

epi_s_num <- function(dat) {
  needed_attributes <- c("status")
  output <- with(get_attr_list(dat, needed_attributes), {
    sum(status == "s", na.rm = TRUE)
  })
  return(output)
}

# epi_infected_poppers <- function(dat) {
#   # we need two attributes for our calculation: `status` and `active`
#   needed_attributes <- c("status", "active","poppers")
#   # we use `with` to simplify code
#   output <- with(EpiModel::get_attr_list(dat, needed_attributes), {
#     pop <- active == 1    # we only look at active nodes
#     pop2 <- poppers == 2  # Get 
#     cond <- status == "i" # which are infected
    
#     # how many are `infected` among the `active`
#     sum(cond & pop & pop2, na.rm = TRUE)
#   })
#   return(output)
# }

# epi_infected_nopoppers <- function(dat) {
#   # we need two attributes for our calculation: `status` and `active`
#   needed_attributes <- c("status", "active","poppers")
#   # we use `with` to simplify code
#   output <- with(EpiModel::get_attr_list(dat, needed_attributes), {
#     pop <- active == 1    # we only look at active nodes
#     pop2 <- poppers == 1  # Get 
#     cond <- status == "i" # which are infected
    
#     # how many are `infected` among the `active`
#     sum(cond & pop & pop2, na.rm = TRUE)
#   })
#   return(output)
# }

# epi_num_poppers <- function(dat) {
#   # we need two attributes for our calculation: `status` and `active`
#   needed_attributes <- c("active","poppers")
#   # we use `with` to simplify code
#   output <- with(EpiModel::get_attr_list(dat, needed_attributes), {
#     pop <- active == 1    # we only look at active nodes
#     pop2 <- poppers == 2  # Get     

#     # How many are on poppers #
#     sum(pop & pop2, na.rm = TRUE)
#   })
#   return(output)
# }

# epi_num_nonpoppers <- function(dat) {
#   # we need two attributes for our calculation: `status` and `active`
#   needed_attributes <- c("active","poppers")
#   # we use `with` to simplify code
#   output <- with(EpiModel::get_attr_list(dat, needed_attributes), {
#     pop <- active == 1    # we only look at active nodes
#     pop2 <- poppers == 1  # Get     

#     # How many are on poppers #
#     sum(pop & pop2, na.rm = TRUE)
#   })
#   return(output)
# }

all.trackers <- list(
  # infected_nopoppers = epi_infected_poppers,
  # infected_poppers = epi_infected_nopoppers,
  # num_nonpoppers = epi_num_nonpoppers,
  # num_poppers = epi_num_poppers,
  s_num = epi_s_num
)
