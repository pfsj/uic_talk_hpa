
##
## Simple Cost-effectiveness Model (Simple SI model with cost and utility tracking)
## EpiModel Gallery (https://github.com/statnet/EpiModel-Gallery)
##
## Author: Gregory Knowlton (University of Minnesota)
## Date: October 2021
##

# Load EpiModel
suppressMessages(library(EpiModel))

# Set seed to ensure resulting CEA output has meaningful interpretation
# (i.e. the intervention is non-dominated)
set.seed(120792)

# Standard Gallery unit test lines
rm(list = ls())
eval(parse(text = print(commandArgs(TRUE)[1])))

  nsims <-10
  ncores <- 6
  nsteps <- 500


# Vital Dynamics Setup ----------------------------------------------------

# Yearly mortality rates by age: (1-84: 0.02, 85-99: 1.00, 100+: 2.00)
ages <- 0:100
dr <- 0.02
death_rate_pp_pw <- c(dr / 52,
                      50 * dr / 52,
                      100 * dr / 52)

# Build out a mortality rate vector
age_spans <- c(85, 15, 1)
dr_vec <- rep(death_rate_pp_pw, times = age_spans)
data.frame(ages, dr_vec)

# Network Model Estimation ------------------------------------------------

# Initialize the network
n <- 500
init.ages <- 16:85
ageVec <- sample(init.ages, n, replace = TRUE)

# Throughout simulation, individuals over age 65 are sexually inactive
active.sVec <- ifelse(ageVec <= 65, 1, 0)

# Initial population used to fit network must contain
# a mixture of sexually active and inactive individuals
# Here we assume age 65+ is inactive.
n.active.s <- length(which(ageVec <= 65))
n.inactive.s <- length(which(ageVec > 65))

poppers.Vec <- sample(1:2,n,replace=TRUE)

# Initialize network
nw <- network_initialize(n)

# Set up age attribute
nw <- set_vertex_attribute(nw, "age", ageVec)

# Set up age attribute
nw <- set_vertex_attribute(nw, "poppers", poppers.Vec)

# active.s denotes which individuals are sexually active
nw <- set_vertex_attribute(nw, "active.s", active.sVec)

# Define the formation model: edges
# level 1 of the active.s corresponds to a value of 0,
# indicating non-participation in the sexual network
formation <- ~ edges + absdiff("age") + nodefactor("active.s", levels = 1) +  nodematch("poppers") + nodefactor("poppers")

# What if there was no homophily #
formation2 <- ~ edges + absdiff("age") + nodefactor("active.s", levels = 1) + nodefactor("poppers")


# Input the appropriate target statistics for each term
mean_degree <- 0.8
edges <- mean_degree * (n.active.s / 2)
avg.abs.age.diff <- 1.5
absdiff <- edges * avg.abs.age.diff

# No edges should contain a sexually inactive ego
inactive.s.edges <- 0

# Differences across drug users #
# Assign degree differences 
# From partnership paper:
# 0.6566899 = non popper users mean degree
# 1.504304 = popper users mean degree during times when using
# Difference times edges
deg.pop <- edges * 0.8476141 

# Homophily #
# Change of mixing BY CHANGE
# p <- deg.pop/(edges*2)
# q <- 1 - p
# nn <- p^2
# np <- 2*p*q
# pp <- q^2
# round(nn + pp, 3)

# Add to change # 
#  0.577 (From Above)
# exp(0.30572)| 1.357602  * 0.577  = 0.78537
nodematch.stat <- edges * 0.78537

target.stats <- c(edges, absdiff, inactive.s.edges,nodematch.stat, deg.pop)
target.stats2 <- c(edges, absdiff, inactive.s.edges, deg.pop)

# Parameterize the dissolution model
coef.diss <- dissolution_coefs(~ offset(edges), 60, mean(dr_vec))
coef.diss

# Fit the model
est <- netest(nw, formation, target.stats, coef.diss)
est2 <- netest(nw, formation2, target.stats2, coef.diss)

# Model diagnostics
dx <- netdx(est2,
            nsims = nsims, ncores = ncores, nsteps = nsteps,
            nwstats.formula = ~ edges + absdiff("age") + isolates + degree(0:5) +
              nodefactor("active.s", levels = 1) + nodematch("poppers") + nodefactor("poppers"))
print(dx)
plot(dx)


# Epidemic model simulation -----------------------------------------------

# Epidemic model parameters for prophylaxis intervention scenario
end.horizon <- 250
# param_inter <- param.net(inf.prob = 0.15,
#                          death.rates = dr_vec,
#                          cea.start = 14,
#                          end.horizon = end.horizon,
#                          arrival.rate = dr / 52,

#                          # Intervention effectiveness/start time
#                          inter.eff = 0.50,
#                          inter.start = 14,
#                          # Intervention Cost
#                          inter.cost = 500000,

#                          # Weekly costs by health status
#                          sus.cost = 150,
#                          inf.cost = 300,
#                          # QALYs by health status
#                          sus.qaly = 1.00,
#                          inf.qaly = 0.75,

#                          # QALY reduction per year of age
#                          age.decrement = -0.003,
#                          # Discount rate for costs and QALYs
#                          disc.rate = 0.03)

# Initial conditions
init <- init.net(i.num = 50)

# Read in the module functions
if (interactive()) {
  source("/Users/pfj185/Library/CloudStorage/OneDrive-NorthwesternUniversity/Jobs/UIC 2023/uic_talk/Analysis/module-fx_uic_new.R", echo = TRUE)
} else {
  source("module-fx.R")
}

# At the time step that marks the start of the end horizon, network
# resimulation will cease. Disease transmission is disabled at this point.
control.updater.list <- list(
  list(
    at = end.horizon,
    control = list(
      resimulate.network = FALSE
    )
  )
)

# Control settings
control <- control.net(type = NULL,
                       nsims = 1,
                       ncores = 1,
                       nsteps = 500,
                       aging.FUN = aging,
                       infection.FUN = infect,
                       progress.FUN = progress,
                       departures.FUN = dfunc,
                       arrivals.FUN = afunc,
                       resim_nets.FUN = resim_nets,
                       resimulate.network = TRUE,
                       control.updater.list = control.updater.list,
                       verbose = TRUE,
                       .tracker.list = all.trackers)


#Set departure rate
departure_rate <- 0.003 
arrival_rate <- 0.00325

# Epidemic model parameters for no-intervention baseline (bl) scenario
param_bl <- param.net(inf.prob.chronic = 0.01,
                      relative.inf.prob.acute = 10,
                      relative.inf.prob.AIDS = 5,
                      relative.inf.prob.ART = 0.05,
                      act.rate = 4,
                      AcuteToChronic1.Rate = 1 / 12,
                      Chronic1ToChronic2.Rate = 1 / 260,
                      Chronic2ToAIDS.Rate = 1 / 260,
                      AIDSToDepart.Rate = 1 / 104,
                      ART.Treatment.Rate = 0.85, # OG 0.01
                      ART.Discontinuance.Rate = 0.05,
                      ART.Progression.Reduction.Rate = 0.5,
                      arrival.rate = arrival_rate,
                      departure.rate = departure_rate
                      )

# Run the network model simulation with netsim for no intervention scenario
sim_bl1 <- netsim(est, param_bl, init, control)
sim_bl2 <- netsim(est2, param_bl, init, control)

# Look at epidemic curve #
plot(sim, y = "i.num", popfrac = TRUE,legend=TRUE)

blData1 <- as.data.frame(sim_bl1)
blData2 <- as.data.frame(sim_bl2)

mean(blData1$prop_infected_nopoppers[blData2$time>254])
mean(blData2$prop_infected_nopoppers[blData2$time>254])

mean(blData1$prop_infected_poppers[blData2$time>254])
mean(blData2$prop_infected_poppers[blData2$time>254])

# Need better than this #
plot(blData2$time,blData2$prop_infected_poppers)

# Run the network INTERVENTION model simulation with netsim 
# sim_inter <- netsim(est, param_inter, init, control)