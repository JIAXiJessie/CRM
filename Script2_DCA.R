rm(list = ls())
install.packages("rmda")
library(rmda)

data_1 <- read.csv("DCA test.csv", row.names = 1)
data_2 <- as.data.frame(t(data_1))
str(data_2)

write.csv(data_2, "transverse.csv")

# Load the data and remove NAs
data_2 <- na.omit(data_2)

# Inspect the data
sample_n(data_2, 3)

set.seed(123)
baseline.model1 <- decision_curve(Group ~ sCDCP1,
                                 data = data_2,
                                 thresholds = seq(0, 1, by = .005),
                                 bootstraps = 10)
plot_decision_curve(baseline.model1 ,
                    curve.names = "sCDCP1",
                    cost.benefit.axis = FALSE,
                    col = c('red', 'blue'),
                    confidence.intervals = FALSE,
                    standardize = FALSE)

plot_clinical_impact(baseline.model1, population.size = 1000,
                     cost.benefit.axis = TRUE,
                     n.cost.benefits = 8,
                     col = c('red', 'blue'),
                     confidence.intervals = TRUE,
                     ylim = c(0, 1000),
                     legend.position = "topright")

baseline.model2 <- decision_curve(Group ~ Model,
                                 data = data_2,
                                 thresholds = seq(0, 1, by = .005),
                                 bootstraps = 10)

baseline.model3 <- decision_curve(Group ~ FIB,
                                  data = data_2,
                                  thresholds = seq(0, 1, by = .005),
                                  bootstraps = 10)

baseline.model4 <- decision_curve(Group ~ APRI,
                                  data = data_2,
                                  thresholds = seq(0, 1, by = .005),
                                  bootstraps = 10)

baseline.model5 <- decision_curve(Group ~ NFS,
                                  data = data_2,
                                  thresholds = seq(0, 1, by = .005),
                                  bootstraps = 10)

plot_decision_curve(baseline.model3 ,
                    curve.names = "CK18",
                    cost.benefit.axis = FALSE,
                    col = c('red', 'blue'),
                    confidence.intervals = FALSE,
                    standardize = FALSE)

plot_clinical_impact(baseline.model3, population.size = 1000,
                     cost.benefit.axis = TRUE,
                     n.cost.benefits = 8,
                     col = c('red', 'blue'),
                     confidence.intervals = TRUE,
                     ylim = c(0, 1000),
                     legend.position = "topright")

set.seed(123)
full.model <- decision_curve(Group ~ sCDCP1 + Combine,
                               data = data_2,
                               thresholds = seq(0, 1, by = .005),
                               bootstraps = 10)

plot_clinical_impact(full.model, population.size = 1000,
                     cost.benefit.axis = TRUE,
                     n.cost.benefits = 8,
                     col = c('red', 'blue'),
                     confidence.intervals = TRUE,
                     ylim = c(0, 1000),
                     legend.position = "topright")

plot_decision_curve(full.model,  
                    curve.names = "Full Model",
                    cost.benefit.axis = FALSE,
                    confidence.intervals = FALSE,
                    standardize = FALSE)

plot_decision_curve(baseline.model1,
                    curve.names = "sCDCP1",
                    cost.benefit.axis = FALSE,
                    col = c('red', 'blue'),
                    confidence.intervals = FALSE,
                    standardize = FALSE)

plot_decision_curve(list(baseline.model1, baseline.model2),
                     curve.names = c("Baseline model1", "Baseline model2"),
                     col = c("blue", "red"),
                     confidence.intervals = FALSE,
                     cost.benefit.axis = FALSE,
                     legend.position = "topright")

plot_decision_curve(list(baseline.model1, baseline.model2, baseline.model3, baseline.model4, baseline.model5),
                     curve.names = c("sCDCP1", "Model", "FIB-4", "APRI", "NFS"),
                     col = c("blue", "red", "green", "Black", "yellow"),
                     confidence.intervals = FALSE,
                     cost.benefit.axis = TRUE,
                     legend.position = "topright")

plot_clinical_impact(simple, population.size = 1000,
                     cost.benefit.axis = TRUE,
                     n.cost.benefits = 8,
                     col = c('red', 'blue'),
                     confidence.intervals = TRUE,
                     ylim = c(0, 1000),
                     legend.position = "topright")

plot_decision_curve(list(baseline.model1, baseline.model2, baseline.model3),
                     curve.names = c("sCDCP1", "Combine model", "CK18"),
                     col = c("blue", "red", "green"),
                     lty = c(1, 4),
                     lwd = c(3, 2, 2, 1),
                     legend.position = "bottomright")
