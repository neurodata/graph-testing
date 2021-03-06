# Graph Power of Test Statistic RMarkdown file.
Zeinab Mousavi  




## Data Preparation 

The data have been imported and set up in .... 



```r
setwd("/Users/zeinab/Desktop/R_Network/GKTB")

source("GKTB_BD_sampling.R")
require("igraph")
library("lattice")
library("ggplot2")

source("functions2.R")
```

# Normal distribution

Here is code to plot a normal distribution
Look  into this: what am I doing wrong here
[link](www.google.com)


```r
x <- rnorm(1e4)
hist(x)
```

![](TestPower_files/figure-html/cc2-1.png)<!-- -->

# Graph parameters 


```r
n_sample=1000  # number of samples for null distribution creation 
graph_sizes <- seq(100, 1000, 100)
p = 0.1
m_size_fraction = 1/20
q =0.5 
```

# Generate Alternate Distribution: SBM k(n, p, m, q)

The function saves the scan test paramater of the samples

```r
#generate_sbm(n_sample, graph_sizes, p, m_size_fraction, q)
```


# Generate Null Distribution 
## Null: ER conditioned on p


```r
##generate ER_gnp scan distributions and critical values
#null ="er_gnp"
#generate_null(n_sample, null, graph_sizes, p, m_size_fraction, q) 
##
##now calculate power ER_gnp vs SBM:
#null = "er_gnp"
#alternate ="sbm"
#power_test_graph_size_vary(null, alternate, graph_sizes)

er_gnp_vs_sbm <- read.csv("power_er_gnp_vs_sbmgraphsize_vary.csv")

par(mfrow=c(1,1))
plot(er_gnp_vs_sbm$graph_size_df, er_gnp_vs_sbm$power_df, xlab="graph size", ylab="test power", main="power test: ER_gnp vs SBM")
```

![](TestPower_files/figure-html/cc5-1.png)<!-- -->

## Null: ER conditioned on m (edge counts)

```r
##generate ER_gnm scan distributions and critical values
#null ="er_gnm"
#generate_null(n_sample, null, graph_sizes, p, m_size_fraction, q)
##
##now calculate power ER_gnm vs SBM:
#null = "er_gnm"
#alternate ="sbm"
#graph_sizes <- seq(100, 1000, 100)
#power_test_graph_size_vary(null, alternate, graph_sizes)

er_gnm_vs_sbm <- read.csv("power_er_gnm_vs_sbmgraphsize_vary.csv")

par(mfrow=c(1,1))
plot(er_gnm_vs_sbm$graph_size_df, er_gnm_vs_sbm$power_df, xlab="graph size", ylab="test power", main="power test: ER_gnm vs SBM")
```

![](TestPower_files/figure-html/cc6-1.png)<!-- -->


```r
##Null: Uniformly sampled graphs conditioned on the scan distribution of observed SBM
#null = "ds"
#graph_sizes <- seq(600, 1000, 100)
#n_sample=100
#generate_null(n_sample, null, graph_sizes, p, m_size_fraction, q)
##
#now calculate power DS_Uniform vs SBM:
#null = "ds"
#alternate ="sbm"
#graph_sizes <- c(100, 200, 400, 600)
#power_test_graph_size_vary(null, alternate, graph_sizes)

ds_vs_sbm <- read.csv("power_ds_vs_sbmgraphsize_vary.csv")

par(mfrow=c(1,1))
plot(ds_vs_sbm$graph_size_df, ds_vs_sbm$power_df, xlab="graph size", ylab="test power", main="power test: Uniform_DS vs SBM")
```

![](TestPower_files/figure-html/cc7-1.png)<!-- -->
