# - - - - - - - - - - - - - - - - - - 
# MAIN SCRIPT
# - - - - - - - - - - - - - - - - - - 

library(igraph)
library(reshape)
library(magrittr)
library(stringdist)
library(dplyr)
library(RColorBrewer)
library(colorspace)

rm(list=ls())

# - - - - - - - - - - - - - - - - - - 
# Load functions and datasets
# - - - - - - - - - - - - - - - - - - 

setwd("~/Documents/GitHub/school-networks-15")
source("R/network_analysis_functions.R")
source("R/plot_summary_results.R")

schooltab=c("1","2","3","4")


# - - - 
# Output unique participants and contacts across all rounds

# - - - 
# Figure 1 - plot all directed graphs
par(mfrow = c(4,4))
for(school1 in 1:4){
  source("R/load_school_data.R",local = F)
  network.analysis(1,school1,mutualPick = 0,plotALL=F,plotClass=F,plotRound=T)
}

# - - - 
# Fig 2 - plot all connections
par(mfrow = c(4,2))
for(school1 in 1:4){
  source("R/load_school_data.R",local = F)
  network.analysis(1,school1,mutualPick = 0,plotALL=TRUE,plotClass=T)
}
dev.copy(pdf,paste("plots/Figure_3.pdf",sep=""),width=5,height=10)
dev.off()


# - - - 
# Table 1 - number of unique participants and contacts
summary_links = NULL
for(school1 in 1:4){
  source("R/load_school_data.R",local = F)
  summary_links = rbind(summary_links, c(school1,length(unique(dataset1a[,1]))-1,sum(dataset1a[,2:7]!="0_ID_NA")) ) # Subtract one from participants to remove NAs
}
summary_links = data.frame(summary_links)
names(summary_links) = c("School","Unique_part","Unique contacts") 
write.csv(summary_links,"plots/Table1.csv")


# - - - 
# Table S1 - number of participants and gender split in each round
summary_rounds = NULL
for(school1 in 1:4){
  source("R/load_school_data.R",local = F)
  stat_school = sapply(c(1:4),function(x){y = dataset1[dataset1$Round==x,]; c(school1,x,length(y[!is.na(y$MF),"Round"]),sum(y$MF==1,na.rm=T),sum(y$MF==2,na.rm=T))  } ) %>% t()
  summary_rounds = rbind(summary_rounds,stat_school)
}

summary_rounds = data.frame(summary_rounds)
names(summary_rounds) = c("School","Round","Unique_part","M","F") 
write.csv(summary_rounds,"plots/TableS1.csv")

# - - - 
# Table 2 - bootstrap network statistics
summary_boot = NULL
for(school1 in 1:4){
  source("R/load_school_data.R",local = F)
  output_boot = network.bootstrap(1,school1,mutualPick = 0,boostrap_runs=50)
  summary_boot = rbind(summary_boot,output_boot)
}

rownames(summary_boot) = c(1:4)
write.csv(t(summary_boot),"plots/Table2.csv")


# - - - 
# Compile network summary data
source("R/compile_summary_data.R")
for(school1 in 1:4){
  source("R/load_school_data.R",local = F)
  plot_consistency_pred(school1)
}

# - - - 
# Figure 2 - plot degree distribution
plot_degree_distn()

# - - - 
# Figure 4 and Table 3 - assess predictive power
plot_predictive_power()


