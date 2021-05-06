library(ggplot2)
library(plotly)
library(data.table)
library(mizer)

# Define function that will be used to calculate Balanced Harvesting (BH)
# fishing mortality.
productionFMort <- function(params, n, effort, e_growth, ...) {
  usual_f_mort <- colSums(mizerFMortGear(params, effort))
  production_density <- sweep(e_growth * n, 2, params@w, "*")
  production_density * usual_f_mort
}

# Define function that adds up yield of all species at each time step.
TotalYield <- function(sim) {
  y <- getYield(sim)
  y_total <- rowSums(y)
  return(y_total)
}

# Define function to add up biomass of all species at each time step. 
TotalBiomass <- function(sim) {
  b <- getBiomass(sim)
  b_total <- rowSums(b)
  return(b_total)
}

# Define function that adds up SSB of all species at each time step.
TotalSSB <- function(sim) {
  s <- getSSB(sim)
  s_total <- rowSums(s)
  return(s_total)
}

# Define function the adds up abundance spectrum of all species at each time step.
TotalAbundance <- function(sim) {
  n <- getN(sim)
  n_total <- rowSums(n)
  return(n_total)
}


TP02 <- newTraitParams(no_sp = 12, gear_names = "None", knife_edge_size = NA, min_w_inf = 30, max_w_inf = 40000)
TP02St <- projectToSteady(TP02)
TP02Pr <- project(TP02St, t_max = 300, effort = 0)


BHMinTP02 <- TP02St
BHMinTP02@gear_params$knife_edge_size <- 0.001
BHMinTP02@species_params$knife_edge_size <- 0.001
gear_params(BHMinTP02)$gear <- "balanced"
species_params(BHMinTP02)$gear <- "balanced"
BHMinTP02 <- setRateFunction(BHMinTP02, "FMort", "productionFMort")
BHMinTP02 <- setFishing(BHMinTP02, initial_effort = 8.58e2)
BHMinTP02Pr <- project(BHMinTP02, t_max = 300)

TotalYield(BHMinTP02Pr)[301] #0.01000258
TotalYield(BHMinTP02Pr)[201] #0.01000258 

BHMatTP02 <- TP02St
BHMatTP02@gear_params$knife_edge_size <- BHMatTP02@species_params$w_mat
BHMatTP02@species_params$knife_edge_size <- BHMatTP02@species_params$w_mat
gear_params(BHMatTP02)$gear <- "balanced"
species_params(BHMatTP02)$gear <- "balanced"
BHMatTP02 <- setRateFunction(BHMatTP02, "FMort", "productionFMort")
BHMatTP02 <- setFishing(BHMatTP02, initial_effort = 2.465e3)
BHMatTP02Pr <- project(BHMatTP02, t_max = 300)

TotalYield(BHMatTP02Pr)[301] #0.0100047
TotalYield(BHMatTP02Pr)[201] #0.0100047

BHFixTP02 <- TP02St
BHFixTP02@gear_params$knife_edge_size <- 10
BHFixTP02@species_params$knife_edge_size <- 10
gear_params(BHFixTP02)$gear <- "balanced"
species_params(BHFixTP02)$gear <- "balanced"
BHFixTP02 <- setRateFunction(BHFixTP02, "FMort", "productionFMort")
BHFixTP02 <- setFishing(BHFixTP02, initial_effort = 1.259e3)
BHFixTP02Pr <- project(BHFixTP02, t_max = 300)

TotalYield(BHFixTP02Pr)[301] #0.01000667 
TotalYield(BHFixTP02Pr)[201] #0.01000667 

SAEMatTP02 <- TP02St
SAEMatTP02@gear_params$knife_edge_size <- SAEMatTP02@species_params$w_mat
SAEMatTP02@species_params$knife_edge_size <- SAEMatTP02@species_params$w_mat
gear_params(SAEMatTP02)$gear <- "maturity"
species_params(SAEMatTP02)$gear <- "maturity"
SAEMatTP02Pr <- project(SAEMatTP02, t_max = 300, effort = 0.6765)

TotalYield(SAEMatTP02Pr)[301] #0.01000488
TotalYield(SAEMatTP02Pr)[201] #0.00999171

SAEFixTP02 <- TP02St
gear_params(SAEFixTP02)$gear <- "fixed"
species_params(SAEFixTP02)$gear <- "fixed"
gear_params(SAEFixTP02)$knife_edge_size <- 10
species_params(SAEFixTP02)$knife_edge_size <- 10
SAEFixTP02Pr <- project(SAEFixTP02, effort = 0.497, t_max = 300)

TotalYield(SAEFixTP02Pr)[301] #0.01000396
TotalYield(SAEFixTP02Pr)[201] #0.01000694 


plotFMort(SAEMatTP02Pr, time_range = 300)
plotFMort(SAEFixTP02Pr, time_range = 300)
plotFMort(BHMinTP02Pr, time_range = 300)
plotFMort(BHMatTP02Pr, time_range = 300)
plotFMort(BHFixTP02Pr, time_range = 300)


plotFMort(BHMatTP02Pr, time_range = 0)
plotFMort(BHMatTP02Pr, time_range = 300)


par(xpd = TRUE, mar = par()$mar + c(0,0,0,0), mfrow = c(1,1))
TotYielSAEMatTP <- TotalYield(SAEMatTP02Pr)
TotYielSAEFixTP <- TotalYield(SAEFixTP02Pr)
TotYielBHMinTP <- TotalYield(BHMinTP02Pr)
TotYielBHMatTP <- TotalYield(BHMatTP02Pr)
TotYielBHFixTP <- TotalYield(BHFixTP02Pr)
matplot(cbind(TotYielSAEMatTP, TotYielSAEFixTP, TotYielBHMinTP, TotYielBHMatTP, TotYielBHFixTP), 
        log = "xy", type = "l", #main = "Total yield of all trait-based species over time",  
        xlab = "Year", ylab = "Yield [g m^-3 year^-1]", col = c(2,3,4,5,6), lty=c(4,2,1,5,3), 
        lwd = 3, bty = "l",cex.axis=1.2,cex.lab=1.2)
legend(x = "topright", inset = c(0.1, 0.025),legend = c("SAE(Mat)","SAE(Fix)","BH(Min)","BH(Mat)","BH(Fix)"),
      lty = c(4,2,1,5,3), col = c(2,3,4,5,6), lwd = 3, cex = 1.2, title = "Fishing method")
par(mar=c(5, 4, 4, 2) + 0.1, xpd = FALSE)


#par(xpd = TRUE, mar = par()$mar + c(0,0,0,0), mfrow = c(2,2))
#TotYielSAEMatTP <- TotalYield(SAEMatTP02Pr)
#TotYielSAEFixTP <- TotalYield(SAEFixTP02Pr)
#TotYielBHMinTP <- TotalYield(BHMinTP02Pr)
#TotYielBHMatTP <- TotalYield(BHMatTP02Pr)
#TotYielBHFixTP <- TotalYield(BHFixTP02Pr)
#matplot(cbind(TotYielSAEMatTP, TotYielSAEFixTP, TotYielBHMinTP, TotYielBHMatTP, TotYielBHFixTP),  
#        log = "x", type = "l", main = "Total Yield",  
#        xlab = "Year", ylab = "Yield [g/m^3]", col = c(2,3,4,5,6), lty=c(4,2,1,5,3), 
#        lwd = 4, bty = "l",cex.main=1.8,cex.axis=1.8,cex.lab=1.5)
par(mar=c(4,5,2,1))
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
TotBiomTP02 <- TotalBiomass(TP02Pr)
TotBiomSAEMatTP <- TotalBiomass(SAEMatTP02Pr)
TotBiomSAEFixTP <- TotalBiomass(SAEFixTP02Pr)
TotBiomBHMinTP <- TotalBiomass(BHMinTP02Pr)
TotBiomBHMatTP <- TotalBiomass(BHMatTP02Pr)
TotBiomBHFixTP <- TotalBiomass(BHFixTP02Pr)
matplot(cbind(TotBiomTP02,TotBiomSAEMatTP,TotBiomSAEFixTP,TotBiomBHMinTP,TotBiomBHMatTP,TotBiomBHFixTP), 
        log = "xy", type = "l", main = "Total Biomass",  
        xlab = "Year", ylab = "Biomass [g m^-3 year^-1]", col = c(1,2,3,4,5,6), lty=c(2,4,2,1,5,3), 
        lwd = 4, bty = "l",cex.main=1.6,cex.axis=1.5,cex.lab=1.5)
TotSSBTP02 <- TotalSSB(TP02Pr)
TotSSBSAEMatTP <- TotalSSB(SAEMatTP02Pr)
TotSSBSAEFixTP <- TotalSSB(SAEFixTP02Pr)
TotSSBBHMinTP <- TotalSSB(BHMinTP02Pr)
TotSSBBHMatTP <- TotalSSB(BHMatTP02Pr)
TotSSBBHFixTP <- TotalSSB(BHFixTP02Pr)
matplot(cbind(TotSSBTP02,TotSSBSAEMatTP,TotSSBSAEFixTP,TotSSBBHMinTP,TotSSBBHMatTP,TotSSBBHFixTP), 
        log = "xy", type = "l", main = "Total SSB",  
        xlab = "Year", ylab = "SSB [g m^-3 year^-1]", col = c(1,2,3,4,5,6), lty=c(2,4,2,1,5,3), 
        lwd = 4, bty = "l",cex.main=1.6,cex.axis=1.5,cex.lab=1.5)
par(mar=c(5,14,2,14))
TotAbunTP02 <- TotalAbundance(TP02Pr)
TotAbunSAEMatTP <- TotalAbundance(SAEMatTP02Pr)
TotAbunSAEFixTP <- TotalAbundance(SAEFixTP02Pr)
TotAbunBHMinTP <- TotalAbundance(BHMinTP02Pr)
TotAbunBHMatTP <- TotalAbundance(BHMatTP02Pr)
TotAbunBHFixTP <- TotalAbundance(BHFixTP02Pr)
matplot(cbind(TotAbunTP02,TotAbunSAEMatTP,TotAbunSAEFixTP,TotAbunBHMinTP,TotAbunBHMatTP,TotAbunBHFixTP), 
        log = "xy", type = "l", main = "Total Abundance",  
        xlab = "Year", ylab = "Abundance [m^-3 year^-1]", col = c(1,2,3,4,5,6), lty=c(2,4,2,1,5,3), 
        lwd = 4, bty = "l",cex.main=1.6,cex.axis=1.5,cex.lab=1.5)

#par(mar=c(5, 4, 4, 2) + 0.1, xpd = FALSE)

#par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
#plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
#legend(x = "bottomright", inset = c(0,0),legend = c("No fishing", "SAE(Mat)","SAE(Fix)","BH(Min)", "BH(Mat)","BH(Fix)"),
#       lty = c(2,4,2,1,5,3), col = c(1,2,3,4,5,6), lwd = 4, cex = 1.4, title = "Fishing method", 
#       horiz = FALSE, bty = "o", text.font = 1,text.width = 0.15, title.adj = 0)


BiomPropTable3 = data.table(Species = c(TP02@species_params$species[1:12]), 
                            None = c((getBiomass(TP02Pr)[301,1:12]/TotalBiomass(TP02Pr)[301])*100),
                            SAEMat = c((getBiomass(SAEMatTP02Pr)[301,1:12]/TotalBiomass(SAEMatTP02Pr)[301])*100),
                            SAEFix = c((getBiomass(SAEFixTP02Pr)[301,1:12]/TotalBiomass(SAEFixTP02Pr)[301])*100),
                            BHMin = c((getBiomass(BHMinTP02Pr)[301,1:12]/TotalBiomass(BHMinTP02Pr)[301])*100),
                            BHMat = c((getBiomass(BHMatTP02Pr)[301,1:12]/TotalBiomass(BHMatTP02Pr)[301])*100),
                            BHFix = c((getBiomass(BHFixTP02Pr)[301,1:12]/TotalBiomass(BHFixTP02Pr)[301])*100))
BiomPropTable3


par(mfrow = c(4,3),oma = c(4,1,3,1), mar = c(1,1,1,1), mai = c(0.55,0.55,0.3,0))
for(i in 1:12){
  SSBRatTP02 <- getProportionOfLargeFish(TP02Pr, species = i, threshold_w = TP02@species_params$w_mat[i])
  SSBRatSAEMatTP <- getProportionOfLargeFish(SAEMatTP02Pr, species = i, threshold_w = TP02@species_params$w_mat[i])
  SSBRatSAEFixTP <- getProportionOfLargeFish(SAEFixTP02Pr, species = i, threshold_w = TP02@species_params$w_mat[i])
  SSBRatBHMinTP <- getProportionOfLargeFish(BHMinTP02Pr, species = i, threshold_w = TP02@species_params$w_mat[i])
  SSBRatBHMatTP <- getProportionOfLargeFish(BHMatTP02Pr, species = i, threshold_w = TP02@species_params$w_mat[i])
  SSBRatBHFixTP <- getProportionOfLargeFish(BHFixTP02Pr, species = i, threshold_w = TP02@species_params$w_mat[i])
  matplot(cbind(SSBRatTP02,SSBRatSAEMatTP,SSBRatSAEFixTP,SSBRatBHMinTP,SSBRatBHMatTP,SSBRatBHFixTP),
          log = "x", type = "l", main = paste0(TP02St@species_params$species[i]),
          xlab = "Year", ylab = "Proportion of biomass",col = c(1,2,3,4,5,6), lty=c(2,4,2,1,5,3),lwd = 2.5,
          bty = "l",cex.axis=1.4,cex.main = 1.8,cex.lab=1.2)
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend(x = "bottom", inset = c(0,0),legend = c("No Fishing","SAE(Mat)","SAE(Fix)","BH(Min)","BH(Mat)","BH(Fix)"),
       lty = c(2,4,2,1,5,3), col = c(1,2,3,4,5,6), lwd = 2, cex = 1, title = "Fishing Method", 
       horiz = TRUE, bty = "n", text.font = 2, title.adj = -0.01)


RelBiomTable3 = data.table( Species = c(TP02@species_params$species[1:12]),
                            SAEMat = c((getBiomass(SAEMatTP02Pr)[301,1:12]/getBiomass(SAEMatTP02Pr)[1,1:12])),
                            SAEFix = c((getBiomass(SAEFixTP02Pr)[301,1:12]/getBiomass(SAEFixTP02Pr)[1,1:12])),
                            BHMin = c((getBiomass(BHMinTP02Pr)[301,1:12]/getBiomass(BHMinTP02Pr)[1,1:12])),
                            BHMat = c((getBiomass(BHMatTP02Pr)[301,1:12]/getBiomass(BHMatTP02Pr)[1,1:12])),
                            BHFix = c((getBiomass(BHFixTP02Pr)[301,1:12]/getBiomass(BHFixTP02Pr)[1,1:12])))
RelBiomTable3




NS_params02 <- NS_params
gear_params(NS_params02)$gear <- "None"
species_params(NS_params02)$gear <- "None"
NSP02St <- projectToSteady(NS_params02, effort = 0)
NSP02Pr <- project(NSP02St, t_max = 300, effort = 0)


NSP02BHMin <- NSP02St
gear_params(NSP02BHMin)$gear <- "balanced"
species_params(NSP02BHMin)$gear <- "balanced"
gear_params(NSP02BHMin)$knife_edge_size <- species_params(NSP02BHMin)$w_min
NSP02BHMin <- setFishing(NSP02BHMin, initial_effort = 1.6137e-12)
NSP02BHMin <- setRateFunction(NSP02BHMin, "FMort", "productionFMort")
NSP02BHMinPr <- project(NSP02BHMin, t_max = 300)

TotalYield(NSP02BHMinPr)[301] #1.500037e+12
TotalYield(NSP02BHMinPr)[51] #1.500391e+12

NSP02BHMat <- NSP02St
gear_params(NSP02BHMat)$gear <- "balanced"
species_params(NSP02BHMat)$gear <- "balanced"
gear_params(NSP02BHMat)$knife_edge_size <- NSP02BHMat@species_params$w_mat
species_params(NSP02BHMat)$knife_edge_size <- NSP02BHMat@species_params$w_mat
NSP02BHMat <- setFishing(NSP02BHMat, initial_effort = 5.032e-12)
NSP02BHMat <- setRateFunction(NSP02BHMat, "FMort", "productionFMort")
NSP02BHMatPr <- project(NSP02BHMat, t_max = 300)

TotalYield(NSP02BHMatPr)[301] #1.500011e+12
TotalYield(NSP02BHMatPr)[51] #1.502019e+12

NSP02BHFix <- NSP02St
gear_params(NSP02BHFix)$gear <- "balanced"
species_params(NSP02BHFix)$gear <- "balanced"
gear_params(NSP02BHFix)$knife_edge_size <- 10
species_params(NSP02BHFix)$knife_edge_size <- 10
NSP02BHFix <- setFishing(NSP02BHFix, initial_effort = 1.7679e-12)
NSP02BHFix <- setRateFunction(NSP02BHFix, "FMort", "productionFMort")
NSP02BHFixPr <- project(NSP02BHFix, t_max = 300)

TotalYield(NSP02BHFixPr)[301] #1.500029e+12 
TotalYield(NSP02BHFixPr)[51] #1.50174e+12

NSP02SAEMat <- NSP02St
gear_params(NSP02SAEMat)$gear <- "maturation"
species_params(NSP02SAEMat)$gear <- "maturation"
gear_params(NSP02SAEMat)$knife_edge_size <- species_params(NSP02SAEMat)$w_mat
NSP02SAEMatPr <- project(NSP02SAEMat, effort = 0.2924, t_max = 300)

TotalYield(NSP02SAEMatPr)[301] #1.500028e+12 
TotalYield(NSP02SAEMatPr)[51] #1.500029e+12 

NSP02SAEFix <- NSP02St
gear_params(NSP02SAEFix)$gear <- "fixed"
species_params(NSP02SAEFix)$gear <- "fixed"
gear_params(NSP02SAEFix)$knife_edge_size <- 10
species_params(NSP02SAEFix)$knife_edge_size <- 10
NSP02SAEFixPr <- project(NSP02SAEFix, effort = 0.19896, t_max = 300)

TotalYield(NSP02SAEFixPr)[301] #1.500004e+12
TotalYield(NSP02SAEFixPr)[51] #1.500005e+12

plotFMort(NSP02SAEMatPr, time_range = 300)
plotFMort(NSP02SAEFixPr, time_range = 300)
plotFMort(NSP02BHMinPr, time_range = 300)
plotFMort(NSP02BHMatPr, time_range = 300)
plotFMort(NSP02BHFixPr, time_range = 300)


plotFMort(NSP02BHMinPr, time_range = 1)
plotFMort(NSP02BHMinPr, time_range = 300)


par(xpd = TRUE, mar = par()$mar + c(0,0,0,0), mfrow = c(1,1))
TotYielNSPSAEMat <- TotalYield(NSP02SAEMatPr)
TotYielNSPSAEFix <- TotalYield(NSP02SAEFixPr)
TotYielNSPBHMin <- TotalYield(NSP02BHMinPr)
TotYielNSPBHMat <- TotalYield(NSP02BHMatPr)
TotYielNSPBHFix <- TotalYield(NSP02BHFixPr)
matplot(cbind(TotYielNSPSAEMat, TotYielNSPSAEFix, TotYielNSPBHMin, TotYielNSPBHMat, TotYielNSPBHFix),  
        log = "xy", type = "l", #main = "Total yield of all NS species over time",  
        xlab = "Year", ylab = "Yield [g m^-3 year^-1]", col = c(2,3,4,5,6), lty=c(4,2,1,5,3), 
        lwd = 3, bty = "l",cex.axis=1.2,cex.lab=1.2)
legend(x = "topright", inset = c(0.1, 0.025),legend = c("SAE(Mat)","SAE(Fix)","BH(Min)","BH(Mat)","BH(Fix)"),
       lty = c(4,2,1,5,3), col = c(2,3,4,5,6), lwd = 3, cex = 1.2, title = "Fishing method")
par(mar=c(5, 4, 4, 2) + 0.1, xpd = FALSE)

#par(xpd = TRUE, mar = par()$mar + c(0,0,0,0), mfrow = c(2,2))
#TotYielNSPSAEMat <- TotalYield(NSP02SAEMatPr)
#TotYielNSPSAEFix <- TotalYield(NSP02SAEFixPr)
#TotYielNSPBHMin <- TotalYield(NSP02BHMinPr)
#TotYielNSPBHMat <- TotalYield(NSP02BHMatPr)
#TotYielNSPBHFix <- TotalYield(NSP02BHFixPr)
#matplot(cbind(TotYielNSPSAEMat, TotYielNSPSAEFix, TotYielNSPBHMin, TotYielNSPBHMat, TotYielNSPBHFix), 
#        log = "xy", type = "l", main = "Total Yield",  
#        xlab = "Year", ylab = "Yield [g]", col = c(2,3,4,5,6), lty=c(4,2,1,5,3), 
#        lwd = 4, bty = "l",cex.main=1.8,cex.axis=1.8,cex.lab=1.5)
par(mar=c(4,5,2,1))
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
TotBiomNSP02 <- TotalBiomass(NSP02Pr)
TotBiomNSPSAEMat <- TotalBiomass(NSP02SAEMatPr)
TotBiomNSPSAEFix <- TotalBiomass(NSP02SAEFixPr)
TotBiomNSPBHMin <- TotalBiomass(NSP02BHMinPr)
TotBiomNSPBHMat <- TotalBiomass(NSP02BHMatPr)
TotBiomNSPBHFix <- TotalBiomass(NSP02BHFixPr)
matplot(cbind(TotBiomNSP02,TotBiomNSPSAEMat,TotBiomNSPSAEFix,TotBiomNSPBHMin,TotBiomNSPBHMat,TotBiomNSPBHFix), 
        log = "xy", type = "l", main = "Total Biomass",  
        xlab = "Year", ylab = "Biomass [g year^-1]", col = c(1,2,3,4,5,6), lty=c(2,4,2,1,5,3), 
        lwd = 4, bty = "l",cex.main=1.6,cex.axis=1.5,cex.lab=1.5)
TotSSBNSP02 <- TotalSSB(NSP02Pr)
TotSSBNSPSAEMat <- TotalSSB(NSP02SAEMatPr)
TotSSBNSPSAEFix <- TotalSSB(NSP02SAEFixPr)
TotSSBNSPBHMin <- TotalSSB(NSP02BHMinPr)
TotSSBNSPBHMat <- TotalSSB(NSP02BHMatPr)
TotSSBNSPBHFix <- TotalSSB(NSP02BHFixPr)
matplot(cbind(TotSSBNSP02,TotSSBNSPSAEMat,TotSSBNSPSAEFix,TotSSBNSPBHMin,TotSSBNSPBHMat,TotSSBNSPBHFix), 
        log = "xy", type = "l", main = "Total SSB",  
        xlab = "Year", ylab = "SSB [g year^-1]", col = c(1,2,3,4,5,6), lty=c(2,4,2,1,5,3), 
        lwd = 4, bty = "l",cex.main=1.6,cex.axis=1.5,cex.lab=1.5)
par(mar=c(5,14,2,14))
TotAbunNSP02 <- TotalAbundance(NSP02Pr)
TotAbunNSPSAEMat <- TotalAbundance(NSP02SAEMatPr)
TotAbunNSPSAEFix <- TotalAbundance(NSP02SAEFixPr)
TotAbunNSPBHMin <- TotalAbundance(NSP02BHMinPr)
TotAbunNSPBHMat <- TotalAbundance(NSP02BHMatPr)
TotAbunNSPBHFix <- TotalAbundance(NSP02BHFixPr)
matplot(cbind(TotAbunNSP02,TotAbunNSPSAEMat,TotAbunNSPSAEFix,TotAbunNSPBHMin,TotAbunNSPBHMat,TotAbunNSPBHFix), 
        log = "xy", type = "l", main = "Total Abundance",  
        xlab = "Year", ylab = "Abundance [year^-1]", col = c(1,2,3,4,5,6), lty=c(2,4,2,1,5,3), 
        lwd = 4, bty = "l",cex.main=1.6,cex.axis=1.5,cex.lab=1.5)
par(mar=c(5, 4, 4, 2) + 0.1, xpd = FALSE)


BiomPropTable4 = data.table(Species = c(NSP02St@species_params$species[1:12]), 
                           None = c((getBiomass(NSP02Pr)[301,1:12]/TotalBiomass(NSP02Pr)[301])*100),
                           SAEMat = c((getBiomass(NSP02SAEMatPr)[301,1:12]/TotalBiomass(NSP02SAEMatPr)[301])*100),
                           SAEFix = c((getBiomass(NSP02SAEFixPr)[301,1:12]/TotalBiomass(NSP02SAEFixPr)[301])*100),
                           BHMin = c((getBiomass(NSP02BHMinPr)[301,1:12]/TotalBiomass(NSP02BHMinPr)[301])*100),
                           BHMat = c((getBiomass(NSP02BHMatPr)[301,1:12]/TotalBiomass(NSP02BHMatPr)[301])*100),
                           BHFix = c((getBiomass(NSP02BHFixPr)[301,1:12]/TotalBiomass(NSP02BHFixPr)[301])*100))
BiomPropTable4


par(mfrow = c(4,3),oma = c(4,1,3,1), mar = c(1,1,1,1), mai = c(0.55,0.55,0.3,0))
for(i in 1:12){
  SSBRatNS02 <- getProportionOfLargeFish(NSP02Pr, species = i, threshold_w = NSP02St@species_params$w_mat[i])
  SSBRatNSSAEMat <- getProportionOfLargeFish(NSP02SAEMatPr, species = i, threshold_w = NSP02St@species_params$w_mat[i])
  SSBRatNSSAEFix <- getProportionOfLargeFish(NSP02SAEFixPr, species = i, threshold_w = NSP02St@species_params$w_mat[i])
  SSBRatNSBHMin <- getProportionOfLargeFish(NSP02BHMinPr, species = i, threshold_w = NSP02St@species_params$w_mat[i])
  SSBRatNSBHMat <- getProportionOfLargeFish(NSP02BHMatPr, species = i, threshold_w = NSP02St@species_params$w_mat[i])
  SSBRatNSBHFix <- getProportionOfLargeFish(NSP02BHFixPr, species = i, threshold_w = NSP02St@species_params$w_mat[i])
  matplot(cbind(SSBRatNS02,SSBRatNSSAEMat,SSBRatNSSAEFix,SSBRatNSBHMin,SSBRatNSBHMat,SSBRatNSBHFix),
          log = "x", type = "l", main = paste0(NSP02St@species_params$species[i]),
          xlab = "Year", ylab = "Proportion of biomass",col = c(1,2,3,4,5,6), lty=c(2,4,2,1,5,3),lwd = 2.5,
          bty = "l",cex.axis=1.4,cex.main = 1.4,cex.lab=1.2)
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend(x = "bottom", inset = c(0,0),legend = c("No Fishing","SAE(Mat)","SAE(Fix)","BH(Min)","BH(Mat)","BH(Fix)"),
       lty = c(2,4,2,1,5,3), col = c(1,2,3,4,5,6), lwd = 2, cex = 1, title = "Fishing Method", 
       horiz = TRUE, bty = "n", text.font = 2, title.adj = -0.01)


RelBiomTable4 = data.table( Species = c(NSP02St@species_params$species[1:12]),
                           SAEMat = c((getBiomass(NSP02SAEMatPr)[301,1:12]/getBiomass(NSP02SAEMatPr)[1,1:12])),
                           SAEFix = c((getBiomass(NSP02SAEFixPr)[301,1:12]/getBiomass(NSP02SAEFixPr)[1,1:12])),
                           BHMin = c((getBiomass(NSP02BHMinPr)[301,1:12]/getBiomass(NSP02BHMinPr)[1,1:12])),
                           BHMat = c((getBiomass(NSP02BHMatPr)[301,1:12]/getBiomass(NSP02BHMatPr)[1,1:12])),
                           BHFix = c((getBiomass(NSP02BHFixPr)[301,1:12]/getBiomass(NSP02BHFixPr)[1,1:12])))
RelBiomTable4
