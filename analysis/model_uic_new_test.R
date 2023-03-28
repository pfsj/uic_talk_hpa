
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

# Network Model Estimation ------------------------------------------------

# Initialize the network
n <- 500

# Poppers vector 
poppers.Vec <- sample(1:2,n,replace=TRUE)

# Initialize network
nw <- network_initialize(n)

# Set up age attribute
nw <- set_vertex_attribute(nw, "poppers", poppers.Vec)

# Define the formation model: edges
# level 1 of the active.s corresponds to a value of 0,
# indicating non-participation in the sexual network
formation <- ~ edges +  nodematch("poppers") + nodefactor("poppers")

# What if there was no homophily #
formation2 <- ~ edges + nodefactor("poppers")

# Input the appropriate target statistics for each term
# mean_degree <- (0.6566899 + 1.504304)/72 # Weekly
# mean_degree <- (0.6566899 + 1.504304)/6 # Monthly

percent_popper2 <- 0.38

mean_degree_poppers1 <- 0.996798
mean_degree_poppers2 <- 2.492226

degree_poppers1 <- mean_degree_poppers1 * 500 * (1-percent_popper2) # 6 Months
degree_poppers2 <- mean_degree_poppers2 * 500 * (percent_popper2) # 6 Months

edges <- (degree_poppers1 + degree_poppers2)/2

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
nodematch.stat <- edges * 0.9

target.stats <- c(edges, nodematch.stat, degree_poppers2)
target.stats2 <- c(edges, degree_poppers2)

# First Parameters #
#Set departure rate
departure_rate <- 0.003 
arrival_rate <- 0.00325

# Parameterize the dissolution model
coef.diss <- dissolution_coefs(dissolution = ~offset(edges),
                               duration = 52, d.rate = departure_rate)

# Fit the model
est <- netest(nw, formation, target.stats, coef.diss)
est2 <- netest(nw, formation2, target.stats2, coef.diss)

# Model diagnostics
dx1 <- netdx(est,
            nsims = nsims, ncores = ncores, nsteps = nsteps,
            nwstats.formula = ~ edges + isolates + degree(0:5) +
            nodematch("poppers") + nodefactor("poppers",levels=NULL))
plot(dx1)

dx2 <- netdx(est2,
            nsims = nsims, ncores = ncores, nsteps = nsteps,
            nwstats.formula = ~ edges + isolates + degree(0:5) +
            nodematch("poppers") + nodefactor("poppers",levels=NULL))
plot(dx2)


# Epidemic model simulation -----------------------------------------------

# Initial conditions
init <- init.net(i.num = 75)

# Read in the module functions
if (interactive()) {
  source("/Users/pfj185/Library/CloudStorage/OneDrive-NorthwesternUniversity/Jobs/UIC 2023/uic_talk/Analysis/module-fx_uic_new.R", echo = TRUE)
} else {
  source("module-fx.R")
}

# Control settings
control <- control.net(type = NULL,
                       nsims = 12,
                       ncores = 6,
                       nsteps = 520,
                       #aging.FUN = aging,
                       infection.FUN = infect,
                       progress.FUN = progress,
                       departures.FUN = dfunc,
                       arrivals.FUN = afunc,
                       resim_nets.FUN = resim_nets,
                       resimulate.network = TRUE,
                       verbose = TRUE,
                       epi.by = "poppers",
                       .tracker.list = all.trackers)

# Epidemic model parameters for no-intervention baseline (bl) scenario
param_bl <- param.net(inf.prob.chronic = 0.0035,
                      relative.inf.prob.acute = 10,
                      relative.inf.prob.AIDS = 5,
                      relative.inf.prob.ART = 0.05,
                      act.rate = 4,
                      AcuteToChronic1.Rate = 1 / 12,
                      Chronic1ToChronic2.Rate = 1 / 260,
                      Chronic2ToAIDS.Rate = 1 / 260,
                      AIDSToDepart.Rate = 1 / 104,
                      ART.Treatment.Rate.P1 = 0.80, # OG 0.01
                      ART.Treatment.Rate.P2 = 0.20,
                      ART.Discontinuance.Rate = 0.005,
                      ART.Progression.Reduction.Rate = 0.5,
                      arrival.rate = arrival_rate,
                      departure.rate = departure_rate
                      )

# Run the network model simulation with netsim for no intervention scenario
sim1 <- netsim(est, param_bl, init, control)
sim2 <- netsim(est2, param_bl, init, control)

# Look at epidemic curve #
# plot(sim1, y = c("i.num","s.num"), popfrac = TRUE,legend=FALSE)
# plot(sim1, y = c("i.num"), popfrac = TRUE,legend=FALSE)
# plot(sim1, y = c("i.num.poppers1","i.num.poppers2"), popfrac = TRUE,legend=FALSE, add=TRUE)
# plot(sim2, y = c("i.num","s.num"), popfrac = TRUE,legend=FALSE)
# plot(sim2, y = c("i.num.poppers1","i.num.poppers2"), popfrac = TRUE,legend=FALSE)

# For Presentation # 
plot(sim1, y = c("i.num","i.num.poppers1","i.num.poppers2"), popfrac = TRUE,legend=FALSE,ylim=c(0,.5))
plot(sim2, y = c("i.num","i.num.poppers1","i.num.poppers2"), popfrac = TRUE,legend=FALSE,ylim=c(0,.5))

plot(sim1, y = c("i.num","i.num.poppers1","i.num.poppers2"), legend=FALSE,ylim=c(0,150))
plot(sim2, y = c("i.num","i.num.poppers1","i.num.poppers2"), legend=FALSE,ylim=c(0,150))



# Change 5200 for scale
sim1 <- mutate_epi(sim1, ir100 = 5200*(si.flow.poppers1 + si.flow.poppers2) / (((num.poppers1 - i.num.poppers1) + (num.poppers2 - i.num.poppers2))))
sim1 <- mutate_epi(sim1, ir100.poppers1 = 5200*(si.flow.poppers1) / s.num.poppers1)
sim1 <- mutate_epi(sim1, ir100.poppers2 = 5200*(si.flow.poppers2) / s.num.poppers2)

# 
sim2 <- mutate_epi(sim2, ir100 = 5200*(si.flow.poppers1 + si.flow.poppers2) / (((num.poppers1 - i.num.poppers1) + (num.poppers2 - i.num.poppers2))))
sim2 <- mutate_epi(sim2, ir100.poppers1 = 5200*(si.flow.poppers1) / s.num.poppers1)
sim2 <- mutate_epi(sim2, ir100.poppers2 = 5200*(si.flow.poppers2) / s.num.poppers2)

# Convert to data frames 
data1 <- as.data.frame(sim1)
data2 <- as.data.frame(sim2)

# Overall Prevalence 
mean(data1$i.num[data1$time==52*10])/mean(data1$num[data1$time==52*10])
# Prevalence among non users w/Homophily 
mean(data1$i.num.poppers1[data1$time==52*10])/mean(data1$num.poppers1[data1$time==52*10])
# Prevalence among popper users w/Homophily 
mean(data1$i.num.poppers2[data1$time==52*10])/mean(data1$num.poppers2[data1$time==52*10])

# Overall Prevalence
mean(data2$i.num[data2$time==520])/mean(data2$num[data2$time==520])
# Prevalence among non users w/o Homophily 
mean(data2$i.num.poppers1[data2$time==52*10])/mean(data2$num.poppers1[data2$time==52*10])
# Prevalence among popper users w/o Homophily 
mean(data2$i.num.poppers2[data2$time==52*10])/mean(data2$num.poppers2[data2$time==52*10])

mean(data1$ir100.poppers1,na.rm=TRUE)
mean(data1$ir100.poppers2,na.rm=TRUE)

mean(data2$ir100.poppers1,na.rm=TRUE)
mean(data2$ir100.poppers2,na.rm=TRUE)

### FOR TESTING ### 

control_test <- control.net(type = NULL,
                       nsims = 1,
                       ncores = 1,
                       nsteps = 200,
                       #aging.FUN = aging,
                       infection.FUN = infect,
                       progress.FUN = progress,
                       departures.FUN = dfunc,
                       arrivals.FUN = afunc,
                       resim_nets.FUN = resim_nets,
                       resimulate.network = TRUE,
                       verbose = TRUE,
                       epi.by = "poppers",
                       .tracker.list = all.trackers)

testSim <- netsim(est, param_bl, init, control_test)                  
plot(testSim, y = c("i.num","i.num.poppers1","i.num.poppers2"), popfrac = TRUE,legend=TRUE)

testSimData <- as.data.frame(testSim)

# # Prevalence among popper users in tests
# mean(testData$i.num.poppers2[testData$time==250])/mean(testData$num.poppers2[testData$time==250])
# # Prevalence among non users in tests
# mean(testData$i.num.poppers1[testData$time==250])/mean(testData$num.poppers1[testData$time==250])

# plot(test, y = c("i.num","s.num"), popfrac = TRUE,legend=TRUE)

# Make dataset for figures for talk # 

plotData1.poppers1 <- data1[data1$time==520,c("i.num.poppers1","num.poppers1")]
plotData1.poppers1$drug <- "No poppers"
colnames(plotData1.poppers1) <- c("i.num","num","drug")

plotData1.poppers2 <- data1[data1$time==520,c("i.num.poppers2","num.poppers2")]
plotData1.poppers2$drug <- "Poppers"
colnames(plotData1.poppers2) <- c("i.num","num","drug")

plotData1.all <- data1[data1$time==520,c("i.num","num")]
plotData1.all$drug <- "All"
colnames(plotData1.all) <- c("i.num","num","drug")

plotData1 <- rbind(plotData1.poppers1,plotData1.all,plotData1.poppers2)
plotData1$Experiment <- 1

plotData2.poppers1 <- data2[data2$time==520,c("i.num.poppers1","num.poppers1")]
plotData2.poppers1$drug <- "No poppers"
colnames(plotData2.poppers1) <- c("i.num","num","drug")

plotData2.poppers2 <- data2[data2$time==520,c("i.num.poppers2","num.poppers2")]
plotData2.poppers2$drug <- "Poppers"
colnames(plotData2.poppers2) <- c("i.num","num","drug")

plotData2.all <- data2[data2$time==520,c("i.num","num")]
plotData2.all$drug <- "All"
colnames(plotData2.all) <- c("i.num","num","drug")

plotData2 <- rbind(plotData2.poppers1,plotData2.all,plotData2.poppers2)
plotData2$Experiment <- 2

prevalenceData <- rbind(plotData1,plotData2)

prevalenceData$Prevalence <- prevalenceData$i.num/prevalenceData$num
 


# Make incidence figures # 
sims <- c(1:12)
incidence1.poppers1 <- c(mean(data1$ir100.poppers1[data1$sim==1],na.rm=TRUE),
                        mean(data1$ir100.poppers1[data1$sim==2],na.rm=TRUE),
                        mean(data1$ir100.poppers1[data1$sim==3],na.rm=TRUE),
                        mean(data1$ir100.poppers1[data1$sim==4],na.rm=TRUE),
                        mean(data1$ir100.poppers1[data1$sim==5],na.rm=TRUE),
                        mean(data1$ir100.poppers1[data1$sim==6],na.rm=TRUE),
                        mean(data1$ir100.poppers1[data1$sim==7],na.rm=TRUE),
                        mean(data1$ir100.poppers1[data1$sim==8],na.rm=TRUE),
                        mean(data1$ir100.poppers1[data1$sim==9],na.rm=TRUE),
                        mean(data1$ir100.poppers1[data1$sim==10],na.rm=TRUE),
                        mean(data1$ir100.poppers1[data1$sim==11],na.rm=TRUE),
                        mean(data1$ir100.poppers1[data1$sim==11],na.rm=TRUE))
incidence1.poppers2 <- c(mean(data1$ir100.poppers2[data1$sim==1],na.rm=TRUE),
                        mean(data1$ir100.poppers2[data1$sim==2],na.rm=TRUE),
                        mean(data1$ir100.poppers2[data1$sim==3],na.rm=TRUE),
                        mean(data1$ir100.poppers2[data1$sim==4],na.rm=TRUE),
                        mean(data1$ir100.poppers2[data1$sim==5],na.rm=TRUE),
                        mean(data1$ir100.poppers2[data1$sim==6],na.rm=TRUE),
                        mean(data1$ir100.poppers2[data1$sim==7],na.rm=TRUE),
                        mean(data1$ir100.poppers2[data1$sim==8],na.rm=TRUE),
                        mean(data1$ir100.poppers2[data1$sim==9],na.rm=TRUE),
                        mean(data1$ir100.poppers2[data1$sim==10],na.rm=TRUE),
                        mean(data1$ir100.poppers2[data1$sim==11],na.rm=TRUE),
                        mean(data1$ir100.poppers2[data1$sim==11],na.rm=TRUE))
                        
incidence1 <- c(mean(data1$ir100[data1$sim==1],na.rm=TRUE),
                        mean(data1$ir100[data1$sim==2],na.rm=TRUE),
                        mean(data1$ir100[data1$sim==3],na.rm=TRUE),
                        mean(data1$ir100[data1$sim==4],na.rm=TRUE),
                        mean(data1$ir100[data1$sim==5],na.rm=TRUE),
                        mean(data1$ir100[data1$sim==6],na.rm=TRUE),
                        mean(data1$ir100[data1$sim==7],na.rm=TRUE),
                        mean(data1$ir100[data1$sim==8],na.rm=TRUE),
                        mean(data1$ir100[data1$sim==9],na.rm=TRUE),
                        mean(data1$ir100[data1$sim==10],na.rm=TRUE),
                        mean(data1$ir100[data1$sim==11],na.rm=TRUE),
                        mean(data1$ir100[data1$sim==11],na.rm=TRUE))
incidence2 <- c(mean(data2$ir100[data2$sim==1],na.rm=TRUE),
                        mean(data2$ir100[data2$sim==2],na.rm=TRUE),
                        mean(data2$ir100[data2$sim==3],na.rm=TRUE),
                        mean(data2$ir100[data2$sim==4],na.rm=TRUE),
                        mean(data2$ir100[data2$sim==5],na.rm=TRUE),
                        mean(data2$ir100[data2$sim==6],na.rm=TRUE),
                        mean(data2$ir100[data2$sim==7],na.rm=TRUE),
                        mean(data2$ir100[data2$sim==8],na.rm=TRUE),
                        mean(data2$ir100[data2$sim==9],na.rm=TRUE),
                        mean(data2$ir100[data2$sim==10],na.rm=TRUE),
                        mean(data2$ir100[data2$sim==11],na.rm=TRUE),
                        mean(data2$ir100[data2$sim==11],na.rm=TRUE))

incidence2.poppers1 <- c(mean(data2$ir100.poppers1[data2$sim==1],na.rm=TRUE),
                        mean(data2$ir100.poppers1[data2$sim==2],na.rm=TRUE),
                        mean(data2$ir100.poppers1[data2$sim==3],na.rm=TRUE),
                        mean(data2$ir100.poppers1[data2$sim==4],na.rm=TRUE),
                        mean(data2$ir100.poppers1[data2$sim==5],na.rm=TRUE),
                        mean(data2$ir100.poppers1[data2$sim==6],na.rm=TRUE),
                        mean(data2$ir100.poppers1[data2$sim==7],na.rm=TRUE),
                        mean(data2$ir100.poppers1[data2$sim==8],na.rm=TRUE),
                        mean(data2$ir100.poppers1[data2$sim==9],na.rm=TRUE),
                        mean(data2$ir100.poppers1[data2$sim==10],na.rm=TRUE),
                        mean(data2$ir100.poppers1[data2$sim==11],na.rm=TRUE),
                        mean(data2$ir100.poppers1[data2$sim==11],na.rm=TRUE))
incidence2.poppers2 <- c(mean(data2$ir100.poppers2[data2$sim==1],na.rm=TRUE),
                        mean(data2$ir100.poppers2[data2$sim==2],na.rm=TRUE),
                        mean(data2$ir100.poppers2[data2$sim==3],na.rm=TRUE),
                        mean(data2$ir100.poppers2[data2$sim==4],na.rm=TRUE),
                        mean(data2$ir100.poppers2[data2$sim==5],na.rm=TRUE),
                        mean(data2$ir100.poppers2[data2$sim==6],na.rm=TRUE),
                        mean(data2$ir100.poppers2[data2$sim==7],na.rm=TRUE),
                        mean(data2$ir100.poppers2[data2$sim==8],na.rm=TRUE),
                        mean(data2$ir100.poppers2[data2$sim==9],na.rm=TRUE),
                        mean(data2$ir100.poppers2[data2$sim==10],na.rm=TRUE),
                        mean(data2$ir100.poppers2[data2$sim==11],na.rm=TRUE),
                        mean(data2$ir100.poppers2[data2$sim==11],na.rm=TRUE))

incidenceData <- data.frame(incidence = c(incidence1.poppers1,incidence1.poppers2,
                                          incidence1,incidence2,
                                          incidence2.poppers1,incidence2.poppers2),
                            drug = c(rep("No poppers",12),rep("Poppers",12),rep("All",12),rep("All",12),rep("No poppers",12),rep("Poppers",12)),
                            Experiment = c(rep(1,36),rep(2,36)))

library(ggplot2)

prevalencePlot <- ggplot(prevalenceData, aes(x=factor(drug), y=Prevalence, linetype = factor(Experiment), fill = drug))+
                    geom_boxplot() + theme_classic() +
                    theme(legend.position="none",axis.title.x=element_blank(),
                          axis.text.x=element_text(size=24),
                          axis.text.y=element_text(size=24),
                          axis.title.y=element_text(size=24)) +
                    scale_fill_manual(values = c("#8bbbe9","#e89eab","#abe4a0"))


incidencePlot <- ggplot(incidenceData, aes(x=factor(drug), y=incidence, linetype = factor(Experiment), fill = drug))+
                    geom_boxplot()+
                    theme( legend.position = "none" )  + theme_classic() + 
                    theme(legend.position="none",axis.title.x=element_blank(),
                          axis.text.x=element_text(size=24),
                          axis.text.y=element_text(size=24),
                          axis.title.y=element_text(size=24)) +
                    scale_fill_manual(values = c("#8bbbe9","#e89eab","#abe4a0")) + ylab("Incidence (Per 100PY)")