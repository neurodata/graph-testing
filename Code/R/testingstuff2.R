

setwd("/Users/zeinab/Desktop/R_Network/GKTB")

source("GKTB_BD_sampling.R")
require("igraph")
library("lattice")

source("functions.R")


library(grid)
library(gridExtra)
source("multiplot.R")


n_sample <- 100

p = 0.1
m_size_fraction = 1/20
q =0.5 
pm <- cbind( c(p, p), c(p, q) )




#now repeat 

#for experiment let's sample a 1000-SBM
#lets generate ER(n=1000, m=51,000) find scan stats distribution
graph_size <-1000
m_size <- floor(graph_size*m_size_fraction)
n_size <- (graph_size - m_size)
block_sizes = c(n_size,m_size)
sbm_size_dist <- c(rep(0, n_sample))

sbm_sizes = c(50000, 50500, 51000)
scan_null <- c(rep(0, n_sample))
i=0
null_er_gnm_df <- data.frame(scan_df=numeric(0), type_df=numeric(0))
for (sbm_size in sbm_sizes){
    i=i+1
    for (z in seq(1, n_sample, 1)){
        
        g_null <- erdos.renyi.game(graph_size, sbm_size, type = "gnm")
        scan_null[z] = max(local_scan(g_null))
    }
    type <- paste("er_gnm_", as.character(sbm_size), sep="")
    df_temp<- data.frame(scan_df=scan_null, type_df=as.factor((rep(type, length(scan_null)))))
    null_er_gnm_df <- rbind(null_er_gnm_df, df_temp)
    
  
}
graph_title_1000 = paste("Graph_size: ", as.character(graph_size), " Null: gnm", sep="")

plot_1000 <- ggplot(null_er_gnm_df, aes(scan_df, fill = type_df)) +
    geom_density(alpha = 0.2) + xlim(min(null_er_gnm_df$scan_df), max(null_er_gnm_df$scan_df)) + ggtitle(graph_title_1000) 

print (plot)


#Do same with graphsize=100.I know the distribution. 
#sample from it and get ecounts of 450, 500, 550

#now sample 3 from distribution
graph_size <-100
m_size <- floor(graph_size*m_size_fraction)
n_size <- (graph_size - m_size)
block_sizes = c(n_size,m_size)
sbm_size_dist <- c(rep(0, n_sample))

sbm_sizes = c(450, 500, 550)
scan_null <- c(rep(0, n_sample))
i=0
null_er_gnm_df <- data.frame(scan_df=numeric(0), type_df=numeric(0))
for (sbm_size in sbm_sizes){
    i=i+1
    for (z in seq(1, n_sample, 1)){
        
        g_null <- erdos.renyi.game(graph_size, sbm_size, type = "gnm")
        scan_null[z] = max(local_scan(g_null))
    }
    type <- paste("er_gnm_", as.character(sbm_size), sep="")
    df_temp<- data.frame(scan_df=scan_null, type_df=as.factor((rep(type, length(scan_null)))))
    null_er_gnm_df <- rbind(null_er_gnm_df, df_temp)
    
}

graph_title_100 = paste("Graph_size: ", as.character(graph_size), " Null: gnm", sep="")

plot_100 <- ggplot(null_er_gnm_df, aes(scan_df, fill = type_df)) +
    geom_density(alpha = 0.2) + xlim(min(null_er_gnm_df$scan_df), max(null_er_gnm_df$scan_df)) + ggtitle(graph_title_100) 


multiplot(plot_100, plot_1000, cols=1)





sbm_sizes = c(450, 500, 550)
scan_null <- c(rep(0, n_sample))
scan_null_3 <- rbind(scan_null, scan_null, scan_null)
i=0
for (sbm_size in sbm_sizes){
    i=i+1
    for (z in seq(1, n_sample, 1)){
        
        g_null <- erdos.renyi.game(graph_size, sbm_size, type = "gnm")
        scan_null[z] = max(local_scan(g_null))
    }
    scan_null_3[i,]=scan_null
}


#now do same with size_graph = 100
#first see dist of sbm with graphsize=100
graph_size <-100
m_size <- floor(graph_size*m_size_fraction)
n_size <- (graph_size - m_size)
block_sizes = c(n_size,m_size)
sbm_size_dist <- c(rep(0, n_sample))
for (z in seq(1, 1000, 1)){
       #compute test statistic on the observed graph
    g_alternate <- sbm.game(sum(block_sizes), pref.matrix=pm, block.sizes=block_sizes, directed=FALSE,loops=FALSE)
    sbm_size[z] <- ecount(g_alternate)
}
hist(sbm_size)

#now sample 3 from distribution
sbm_sizes = c(450, 500, 550)
scan_null <- c(rep(0, n_sample))
scan_null_3 <- rbind(scan_null, scan_null, scan_null)
i=0
for (sbm_size in sbm_sizes){
    i=i+1
    for (z in seq(1, n_sample, 1)){
        
        g_null <- erdos.renyi.game(graph_size, sbm_size, type = "gnm")
        scan_null[z] = max(local_scan(g_null))
    }
    scan_null_3[i,]=scan_null
}

hist(scan_null_3[1,], col="red", xlim=c(min(scan_null_3[1,]), max(scan_null_3[3,])), prob=T)
hist(scan_null_3[2,], col="lightblue", add=T, prob=T)
hist(scan_null_3[3,], col="purple", add=T, prob=T)