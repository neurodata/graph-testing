rm(list=ls()); 

setwd("/Users/zeinab/Desktop/R_Network/GKTB")

source("GKTB_BD_sampling.R")
require("igraph")
library("lattice")
library("ggplot2")

source("functions2.R")


#p-value of the observed graph scan statistics:
p_value_scan <- function(g_observed, t, exp_scan_graph_norm){
    a <- max(local_scan(g_observed) )
    p_value <- exp_scan_graph_norm/exp(t*a)
    return(p_value)}

#p-value of the observed graph scan statistics:
p_value_rich <- function(g_observed, t, exp_rich_graph_norm){
    a <- richclub_coef_m(g_observed, m_size_fraction = m_size_fraction)[[1]] 
    p_value <- exp_rich_graph_norm/exp(t*a)
    return(p_value)}


n_sample <- 100 
graph_size <- 100 #seq(100, 1000, 100)
sample_method <- 2
m_size_fraction <-1/10
####
#sample a graph from a uniform distribution of graphs with same degree sequence
###

alpha = 0.05
t = 1
expectation_scan_graph <- 0 
expectation_rich_graph <- 0 
weight_sum <- 0

    reference_graph_ds <- rep(2, graph_size)
    #Now with the same degree sequence sample a graph approximately uniformly
    in.deg <- reference_graph_ds
    for (i in seq(1, n_sample, 1)){
    g_u = generate_graph_with_GKTB(in.deg, sampling=sample_method)
    sample_graph = g_u[[1]] 
    weight_graph = g_u[[2]]
    weight_sum <- weight_sum + weight_graph 
    X <-  max(local_scan(sample_graph) ) #scan 
    Y <- richclub_coef_m(sample_graph, m_size_fraction = m_size_fraction)[[1]] 
    expectation_scan_graph[i] <-  weight_graph * exp(t*X)
    expectation_rich_graph[i] <-  weight_graph * exp(t*Y)
    }

exp_scan_graph_norm <- sum(expectation_scan_graph)/weight_sum
exp_rich_graph_norm <- sum(expectation_rich_graph)/weight_sum


#2-regular graph
#g_ref <- make_ring(graph_size)
#plot(g_ref)

#generate graphs drawn from other distributions 
observed_graph = "simple generated 2-regular graph"
g_observed_deg <- rep(2, graph_size)
g_observed <- sample_degseq(g_observed_deg, method = "simple.no.multiple")
p_value_rich_obs <- p_value_rich(g_observed, t, exp_rich_graph_norm)
p_value_scan_obs <- p_value_scan(g_observed, t, exp_scan_graph_norm)
paste(observed_graph) 
paste(" p_value (rich) =", as.character(p_value_rich_obs))
paste(" p_value (scan) =", as.character(p_value_scan_obs))

observed_graph = "vl(Viger&Latapy) generated 2-regular graph"
g_observed_deg <- rep(2, graph_size)
g_observed <- sample_degseq(g_observed_deg, method = "simple.no.multiple")
p_value_rich_obs <- p_value_rich(g_observed, t, exp_rich_graph_norm)
p_value_scan_obs <- p_value_scan(g_observed, t, exp_scan_graph_norm)
paste(observed_graph) 
paste(" p_value (rich) =", as.character(p_value_rich_obs))
paste(" p_value (scan) =", as.character(p_value_scan_obs))

observed_graph = "3-regular graph" 
g_observed <- sample_k_regular(graph_size, 3) 
p_value_rich_obs <- p_value_rich(g_observed, t, exp_rich_graph_norm)
p_value_scan_obs <- p_value_scan(g_observed, t, exp_scan_graph_norm)
paste(observed_graph) 
paste(" p_value (rich) =", as.character(p_value_rich_obs))
paste(" p_value (scan) =", as.character(p_value_scan_obs))

observed_graph = "7-regular graph" 
g_observed <- sample_k_regular(graph_size, 7) 
p_value_rich_obs <- p_value_rich(g_observed, t, exp_rich_graph_norm)
p_value_scan_obs <- p_value_scan(g_observed, t, exp_scan_graph_norm)
paste(observed_graph) 
paste(" p_value (rich) =", as.character(p_value_rich_obs))
paste(" p_value (scan) =", as.character(p_value_scan_obs))


observed_graph = "full connected graph" 
g_observed <- make_full_graph(graph_size)
p_value_rich_obs <- p_value_rich(g_observed, t, exp_rich_graph_norm)
p_value_scan_obs <- p_value_scan(g_observed, t, exp_scan_graph_norm)
paste(observed_graph) 
paste(" p_value (rich) =", as.character(p_value_rich_obs))
paste(" p_value (scan) =", as.character(p_value_scan_obs))

observed_graph = "SBM graph"
p = 0.1
#m_size_fraction = 1/10/20
q =0.5
pm <- cbind( c(p, p), c(p, q) )
m_size <- floor(graph_size*m_size_fraction)
n_size <- (graph_size - m_size)
block_sizes = c(n_size,m_size)
g_observed <- sbm.game(sum(block_sizes), pref.matrix=pm, block.sizes=block_sizes, directed=FALSE,loops=FALSE)
p_value_rich_obs <- p_value_rich(g_observed, t, exp_rich_graph_norm)
p_value_scan_obs <- p_value_scan(g_observed, t, exp_scan_graph_norm)
paste(observed_graph) 
paste(" p_value (rich) =", as.character(p_value_rich_obs))
paste(" p_value (scan) =", as.character(p_value_scan_obs))














#g <- make_ring(10)
#m = str(rewire(g, with = keeping_degseq(niter = vcount(g) * 10)))
