####
#sample a graph from a uniform distribution of graphs with same degree sequence
###

sample_graph_ds <- function(graph_size, p, m_size_fraction, q, sample_method){
    pm <- cbind( c(p, p), c(p, q) )
    m_size <- floor(graph_size*m_size_fraction)
    n_size <- (graph_size - m_size)
    block_sizes = c(n_size,m_size)
    
    #generate degree sequence of SBM graph
    g_sbm <- sbm.game(sum(block_sizes), pref.matrix=pm, block.sizes=block_sizes, directed=FALSE,loops=FALSE)
    
    
    #Now also with the same degree sequence sample a graph randomly
    in.deg <- degree(g_sbm)
    g_u = generate_graph_with_GKTB(in.deg, sampling=sample_method)
    sample_graph = g_u[[1]] 
    #check that the algorithm gave the right degree sequence:
    sample_graph_ds <- degree(sample_graph)
    vertex_count = length(sample_graph_ds)
    if (vertex_count!=length(in.deg)){
        n_zero_deg= length(in.deg)-length(sample_graph_ds)
        sample_graph_ds_all=c(rep(0,  n_zero_deg), sample_graph_ds)
        sample_graph_ds <- sample_graph_ds_all
    }
    match_ds <- sort(sample_graph_ds) != sort(in.deg)
    error <- sum(match_ds)
    if (error !=0){
        error_count[length(error_count)+1] <- error
        print (error_count)
    }
    
    #pass test, get:
    if (error ==0){
        #scan_u[i] = max(local_scan(sample_graph))
        sample_graph
    }
    
}

#########################


generate_null <-function(n_sample, null, graph_sizes, p, m_size_fraction, q){
    scan_null_df = data.frame(graph_size_df=numeric(0), scan_critical_df=numeric(0))
    g=0
    for (graph_size in graph_sizes){
        scan_null <- c(rep(0, n_sample))
        g=g+1
        
        print (graph_size)
        if (null == "er_gnm"){
            print ("observe")
            #generate one observation 
            m_size <- floor(graph_size*m_size_fraction)
            n_size <- (graph_size - m_size)
            block_sizes = c(n_size,m_size)
            pm <- cbind( c(p, p), c(p, q) )
            g_observation <- sbm.game(sum(block_sizes), pref.matrix=pm, block.sizes=block_sizes, directed=FALSE,loops=FALSE)
            #g_observation <- generate_sbm(n_sample, graph_size, p, m_size_fraction, q)
            total_edge_observation <- ecount(g_observation)
            }
        for (i in seq(1, n_sample, 1)){
            print (i)
            #generate null distribution 
            #ER Graphs, conditioned on p
            if (null == "er_gnp"){
                g_null <- erdos.renyi.game(graph_size, p)
            }
            #ER Graphs, conditioned on graph_size
            if (null == "er_gnm"){
                g_null <- erdos.renyi.game(graph_size, total_edge_observation, type = "gnm")
            }
            #
            # Uniform Sampling of Graphs with same DS
            if (null =="ds"){
                ### call to Joes' Uniform Sampling Algorithm of Grpahs Given Degree Sequence
                g_null <-sample_graph_ds(graph_size, p, m_size_fraction, q, sample_method = 2)
        
            }
            scan_null[i] = max(local_scan(g_null))
            
        }
        #save scan_null distribution
        scan_null_filename <- paste("scan_", null, "_", as.character(graph_size), ".csv", sep="")
        write.csv(scan_null, scan_null_filename)
        #find the critical point of this graph size (alpha =0.05)
        scan_null_df[g, ] <- c(graph_size, quantile(scan_null, prob=0.95)[1]) #,NA
        scan_critical_filename <- paste("scan_critical_", null, ".csv", sep="")
        write.csv(scan_null_df, scan_critical_filename)
    }
    
}



power_test_graph_size_vary <- function(null, alternate, graph_sizes){
    scan_null_alternate_power <- data.frame(graph_size_df=numeric(0), scan_critical_df=numeric(0), power_df=numeric(0))
    scan_null_filename <- paste("scan_critical_", null, ".csv", sep="")
    scan_critical_null = read.csv(scan_null_filename)
    g <- 0
    for (graph_size in graph_sizes){
        g <- g+1
        scan_critical <- scan_critical_null$scan_critical_df[scan_critical_null$graph_size_df==graph_size]
        
        #read sbm scan values
        scan_alternate_filename <- paste("scan_", alternate, "_", as.character(graph_size), ".csv", sep="")
        scan_alternate <- read.csv(scan_alternate_filename)[[2]]
        
        power <- sum(scan_alternate[scan_alternate > scan_critical])/sum(scan_alternate)
        scan_null_alternate_power[g, ] <- c(graph_size, scan_critical, power)
        
    }
    
    
    null_alternate_power_file_name = paste("power_", null, '_vs_', alternate, "graphsize_vary.csv", sep="")
    write.csv( scan_null_alternate_power, null_alternate_power_file_name)
    
}



#########################
#SCAN statistic of SBM
generate_sbm <-function(n_sample, graph_sizes, p, m_size_fraction, q){
scan_sbm_df = data.frame(graph_size_df=numeric(0), scan_critical_df=numeric(0))
pm <- cbind( c(p, p), c(p, q) )
g=0
for (graph_size in graph_sizes){
    scan_sbm <- c(rep(0, n_sample))
    m_size <- floor(graph_size*m_size_fraction)
    n_size <- (graph_size - m_size)
    block_sizes = c(n_size,m_size)
    g=g+1
    print (graph_size)
    for (i in seq(1, n_sample, 1)){
       
      g_sbm <- sbm.game(sum(block_sizes), pref.matrix=pm, block.sizes=block_sizes, directed=FALSE,loops=FALSE)
        scan_sbm[i] = max(local_scan(g_sbm))
        
    }
    #save scan_sbm distribution
    scan_sbm_filename <- paste("scan_sbm", "_", as.character(graph_size), ".csv", sep="")
    write.csv(scan_sbm, scan_sbm_filename)
    
}
}

#####PLOT DISTRIBUTION NULL vs SBM 10 Graphs
##10 Graphs, change this 
null_alt_dist <- function(null, graph_sizes){
i=0
plots <- list() #new empty list
for (graph_size in graph_sizes){
    i=i+1
    null_sbm_dataframe <- data.frame(scan_df=numeric(0), type_df=numeric(0))
    null_filename = paste("scan_", null, "_", as.character(graph_size), ".csv", sep="")
    scan_null = read.csv(null_filename)[[2]]
    null_type = as.factor(rep(null, length(scan_null)))
    temp_df <- data.frame(scan_df=scan_null, type_df=null_type)
    null_sbm_dataframe<- rbind(null_sbm_dataframe, temp_df)
    sbm_filename = paste("scan_sbm_", as.character(graph_size), ".csv", sep="")
    scan_sbm = read.csv(sbm_filename)[[2]]
    alternate_type = as.factor(rep("sbm", length(scan_sbm)))
    temp_df <- data.frame(scan_df=scan_sbm, type_df=alternate_type)
    null_sbm_dataframe<- rbind(null_sbm_dataframe, temp_df)
    
    graph_title = paste("Graph_size: ", as.character(graph_size))
    
    plots[[i]] <- ggplot(null_sbm_dataframe, aes(scan_df, fill = type_df)) +
        geom_density(alpha = 0.2) + xlim(min(null_sbm_dataframe$scan_df), max(null_sbm_dataframe$scan_df)) + ggtitle(graph_title) 
    
}

multiplot(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], plots[[7]], plots[[8]], plots[[9]], plots[[10]], cols=2)
}