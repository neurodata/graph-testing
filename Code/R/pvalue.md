# p-value using Chernoff bound.
Zeinab Mousavi  





p-value for observed graph statistic: $P_u(X \geq X(G^{obs}))$ 

Chernoff Bound:
For $t>0$ and $a>0$ 
 $P_u(X \geq a) \leq \frac{E_u[e^{tX}]}{e^{ta}}$
So:
$P_u(X \geq X(G^{obs})) \leq \frac{E_u[e^{tX}]}{e^{tX(G^{obs})}}$

From Blitzstein & Diaconis, we know:

$\frac{\sum_{i=1}^{n}w_iX(G_{i})}{\sum_{i=1}^{n}w_i}) = E_{alg}[X]$

$E_{u}[X]=E_{alg}[X]$

So
$P_u(X \geq X(G^{obs})) \leq \frac{E_u[e^{tX}]}{e^{tX(G^{obs})}} = \frac{E_{alg}[e^{tX}]}{e^{tX(G^{obs})}}$ , where 
$E_{alg}[e^{tX}]=\frac{\sum_{i=1}^{n}w_ie^{tX(G_{i})}}{\sum_{i=1}^{n}w_i})$

### Data Preparation 

setwd("/Users/zeinab/Desktop/R_Network/GKTB")


```r
source("GKTB_BD_sampling.R")
require("igraph")
library("lattice")
library("ggplot2")

source("functions2.R")
```

### Find p-value.
Test Statistics: Scan and Rich Club Connectivity



```r
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
```


###sample a graph from a uniform distribution of graphs with same degree sequence
### Here, we generate a 100-vertex 2-regular graph


```r
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
```
Now compute p-values for test statistics of various graphs

```r
#generate graphs drawn from other distributions 
observed_graph = "simple generated 2-regular graph"
g_observed_deg <- rep(2, graph_size)
g_observed <- sample_degseq(g_observed_deg, method = "simple.no.multiple")
p_value_rich_obs <- p_value_rich(g_observed, t, exp_rich_graph_norm)
p_value_scan_obs <- p_value_scan(g_observed, t, exp_scan_graph_norm)
paste(observed_graph) 
```

```
## [1] "simple generated 2-regular graph"
```

```r
paste(" p_value (rich) =", as.character(p_value_rich_obs))
```

```
## [1] " p_value (rich) = 1.0160638070666"
```

```r
paste(" p_value (scan) =", as.character(p_value_scan_obs))
```

```
## [1] " p_value (scan) = 1.51057069711988"
```

```r
observed_graph = "vl(Viger&Latapy) generated 2-regular graph"
g_observed_deg <- rep(2, graph_size)
g_observed <- sample_degseq(g_observed_deg, method = "simple.no.multiple")
p_value_rich_obs <- p_value_rich(g_observed, t, exp_rich_graph_norm)
p_value_scan_obs <- p_value_scan(g_observed, t, exp_scan_graph_norm)
paste(observed_graph) 
```

```
## [1] "vl(Viger&Latapy) generated 2-regular graph"
```

```r
paste(" p_value (rich) =", as.character(p_value_rich_obs))
```

```
## [1] " p_value (rich) = 1.0160638070666"
```

```r
paste(" p_value (scan) =", as.character(p_value_scan_obs))
```

```
## [1] " p_value (scan) = 1.51057069711988"
```

```r
observed_graph = "3-regular graph" 
g_observed <- sample_k_regular(graph_size, 3) 
p_value_rich_obs <- p_value_rich(g_observed, t, exp_rich_graph_norm)
p_value_scan_obs <- p_value_scan(g_observed, t, exp_scan_graph_norm)
paste(observed_graph) 
```

```
## [1] "3-regular graph"
```

```r
paste(" p_value (rich) =", as.character(p_value_rich_obs))
```

```
## [1] " p_value (rich) = 1.0160638070666"
```

```r
paste(" p_value (scan) =", as.character(p_value_scan_obs))
```

```
## [1] " p_value (scan) = 0.555707903906418"
```

```r
observed_graph = "7-regular graph" 
g_observed <- sample_k_regular(graph_size, 7) 
p_value_rich_obs <- p_value_rich(g_observed, t, exp_rich_graph_norm)
p_value_scan_obs <- p_value_scan(g_observed, t, exp_scan_graph_norm)
paste(observed_graph) 
```

```
## [1] "7-regular graph"
```

```r
paste(" p_value (rich) =", as.character(p_value_rich_obs))
```

```
## [1] " p_value (rich) = 0.889231934389893"
```

```r
paste(" p_value (scan) =", as.character(p_value_scan_obs))
```

```
## [1] " p_value (scan) = 0.000186419233790643"
```

```r
observed_graph = "full connected graph" 
g_observed <- make_full_graph(graph_size)
p_value_rich_obs <- p_value_rich(g_observed, t, exp_rich_graph_norm)
p_value_scan_obs <- p_value_scan(g_observed, t, exp_scan_graph_norm)
paste(observed_graph) 
```

```
## [1] "full connected graph"
```

```r
paste(" p_value (rich) =", as.character(p_value_rich_obs))
```

```
## [1] " p_value (rich) = 0.37378898553819"
```

```r
paste(" p_value (scan) =", as.character(p_value_scan_obs))
```

```
## [1] " p_value (scan) = 0"
```

```r
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
```

```
## [1] "SBM graph"
```

```r
paste(" p_value (rich) =", as.character(p_value_rich_obs))
```

```
## [1] " p_value (rich) = 0.889231934389893"
```

```r
paste(" p_value (scan) =", as.character(p_value_scan_obs))
```

```
## [1] " p_value (scan) = 1.28897717005774e-16"
```
