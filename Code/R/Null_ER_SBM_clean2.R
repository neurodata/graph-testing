
setwd("/Users/zeinab/Desktop/R_Network/GKTB")

source("GKTB_BD_sampling.R")
require("igraph")
library("lattice")
library("ggplot2")

source("functions2.R")

#generate distribution of null hypothesis 
#find scan_critical 
n_sample=1000  # number of samples for null distribution creation 
graph_sizes <- seq(100, 1000, 100)
null <- "er_gnp"  #["er_gnp", "er_gnm", "ds"]
p = 0.1
m_size_fraction = 1/20
q =0.5 

####ALTERNATE DISTRIBUTION: SBM k(n, p, m, q)####
##generate SBM(Alternate) scan distributions
#generate_sbm(n_sample, graph_sizes, p, m_size_fraction, q)
####


###Null Distributions: ER_GNP, ER_GNM, DS###
###################
###################
###################
##generate ER_gnp scan distributions and critical values
#null ="er_gnp"
#generate_null(n_sample, null, graph_sizes, p, m_size_fraction, q) 
##
##now calculate power ER_gnp vs SBM:
null = "er_gnp"
alternate ="sbm"
graph_sizes <- seq(100, 1000, 100)
power_test_graph_size_vary(null, alternate, graph_sizes)

er_gnp_vs_sbm <- read.csv("power_er_gnp_vs_sbmgraphsize_vary.csv")

par(mfrow=c(1,1))
plot(er_gnp_vs_sbm$graph_size_df, er_gnp_vs_sbm$power_df, xlab="graph size", ylab="test power", main="power test: ER_gnp vs SBM")

###################
###################
###################
##generate ER_gnm scan distributions and critical values
#null ="er_gnm"
#generate_null(n_sample, null, graph_sizes, p, m_size_fraction, q)
##
##now calculate power ER_gnm vs SBM:
null = "er_gnm"
alternate ="sbm"
graph_sizes <- seq(100, 1000, 100)
power_test_graph_size_vary(null, alternate, graph_sizes)

er_gnm_vs_sbm <- read.csv("power_er_gnm_vs_sbmgraphsize_vary.csv")

par(mfrow=c(1,1))
plot(er_gnm_vs_sbm$graph_size_df, er_gnm_vs_sbm$power_df, xlab="graph size", ylab="test power", main="power test: ER_gnm vs SBM")

###################
###################
###################
##generate DS scan distributions and critical values
null = "ds"
graph_sizes <- seq(300, 1000, 100)
n_sample=100
generate_null(n_sample, null, graph_sizes, p, m_size_fraction, q)
##
#now calculate power DS_Uniform vs SBM:
null = "ds"
alternate ="sbm"
graph_sizes <- c(100, 200, 400, 600)
power_test_graph_size_vary(null, alternate, graph_sizes)

ds_vs_sbm <- read.csv("power_ds_vs_sbmgraphsize_vary.csv")
plot(ds_vs_sbm$graph_size_df, ds_vs_sbm$power_df)










