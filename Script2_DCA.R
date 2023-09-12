rm(list = ls())
install.packages("rmda")
library(rmda)
data(dcaData)

data_1<-read.csv("DCA test.csv",row.names = 1)
data_2 <- as.data.frame(t(data_1))
str(data_2)

write.csv(data_2,"transverse.csv")
# Load the data and remove NAs

data_2 <- na.omit(data_2)

# Inspect the data
sample_n(data_2, 3)

set.seed(123)
baseline.model <- decision_curve(Group~sCDCP1,
                                 data = data_2,
                                 thresholds = seq(0, 1, by = .005),
                                 bootstraps = 10)
plot_clinical_impact(baseline.model,population.size= 1000,
                     cost.benefit.axis = T,
                     n.cost.benefits= 8,
                     col =c('red','blue'),
                     confidence.intervals= T,
                     ylim=c(0,1000),
                     legend.position="topright")
baseline.mode2 <- decision_curve(Group~APRI,
                                 data = data_2,
                                 thresholds = seq(0, 1, by = .005),
                                 bootstraps = 10)
plot_clinical_impact(baseline.mode2,population.size= 1000,
                     cost.benefit.axis = T,
                     n.cost.benefits= 8,
                     col =c('red','blue'),
                     confidence.intervals= T,
                     ylim=c(0,1000),
                     legend.position="topright")
baseline.mode3 <- decision_curve(Group~FIB4,
                                 data = data_2,
                                 thresholds = seq(0, 1, by = .005),
                                 bootstraps = 10)
set.seed(123)
full.model <- decision_curve(Group~sCDCP1 + APRI,
                               data = data_2,
                               thresholds = seq(0, 1, by = .005),
                               bootstraps = 10)

plot_clinical_impact(full.model,population.size= 1000,
                     cost.benefit.axis = T,
                     n.cost.benefits= 8,
                     col =c('red','blue'),
                     confidence.intervals= T,
                     ylim=c(0,1000),
                     legend.position="topright")


plot_decision_curve(full.model,  
                    curve.names = "Full Model",
                    cost.benefit.axis =FALSE,
                    #col= c('red','blue'),
                    confidence.intervals=FALSE,
                    standardize = FALSE)
#plot using the defaults
plot_decision_curve(baseline.mode3,
                    curve.names = "FIB-4",
                    cost.benefit.axis =FALSE,
                    #col= c('red','blue'),
                    confidence.intervals=FALSE,
                    standardize = FALSE,)
plot_decision_curve( list(baseline.model, full.model),
                     curve.names = c("Baseline model", "Full model"),
                     col = c("blue", "red"),
                     confidence.intervals = FALSE,  #remove confidence intervals
                     cost.benefit.axis = FALSE, #remove cost benefit axis
                     legend.position = "topright") #add the legend

plot_decision_curve( list(baseline.model,baseline.mode2,baseline.mode3, full.model),
                     curve.names = c("sCDCP1","APRI","FIB-4", "sCDCP1+APRI"),
                     col = c("blue","red","black","green"),
                     confidence.intervals = FALSE,  #remove confidence intervals
                     cost.benefit.axis = FALSE, #remove cost benefit axis
                     legend.position = "topright") #add the legend

plot_clinical_impact(simple,population.size= 1000,
                     cost.benefit.axis = T,
                     n.cost.benefits= 8,
                     col =c('red','blue'),
                     confidence.intervals= T,
                     ylim=c(0,1000),
                     legend.position="topright")

plot_decision_curve(list(baseline.model,baseline.mode2,baseline.mode3,full.model), curve.names = c("baseline.model","baseline.mode2","baseline.mode3",
                                                                      "full.model"), col = c("blue", "red"), lty = c(1, 4), lwd = c(3, 2, 2, 1), legend.position = "bottomright")

