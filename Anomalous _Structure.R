library(tidyverse)
library(magrittr)
library(anomaly)
library(ggplot2)


library(readr)
ops_data <- read_csv("~/Projects/Water_Leak_detection-/ops_data.csv")
Leak_12_demand <- read_csv("~/Projects/LeakDB/CCWI-WDSA2018/Benchmarks/Hanoi_CMH/Scenario-10/Leaks/Leak_12_demand.csv")
Node_12 <- read_csv("~/Projects/LeakDB/CCWI-WDSA2018/Benchmarks/Hanoi_CMH/Scenario-10/Demands/Node_12.csv")


#Univariate Collective And Point Anomaly Detection (CAPA)

library("anomaly")
res <- capa.uv(Node_12$Value, type = "mean")
summary(res)
plot(res)

point_anomalies(res)

#Sequential Collective And Point Anomalies (SCAPA) 

trans <- . %>% tierney(1000)
res <- scapa.uv(x, transform = trans)
res

plot(res, epoch = 3000)

#Multivariate Collective And Point Anomalies (MVCAPA)

#Pressure 

ops_pressure <- as.matrix(ops_data[,90:99])
trans <- . %>% tierney(1000)
res_mult <- scapa.mv(ops_pressure, max_lag = 20, type = "mean", transform = trans)

summary(res_mult)

#Proportion Adaptive Segment Selection (PASS)

res <- pass(ops_pressure, max_seg_len = 20, alpha = 3)

summary(res)
collective_anomalies(res)
plot(res)





