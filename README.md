# graph-testing

given a graph, we sometimes notice a pattern of connectivity, a *signal* of something, and we desire to know whether "it is more extreme then we might expect by chance".  this is a 1-sample testing question, which means we need to define:

1. the null distribution
1. the test statistic

### null distribution

the simplest null is ER.  almost always in real applications ER is *too* simple, because nearly every test statistic from nearly every measured graph will substantially deviate from the expected test stat from any ER distribution.  rather than conditioning on the expected # of edges (which ER basically does), one could condition on the actual number of edges. this is basically equivalent and similarly too simple.

one of the next simplest nulls is to condition on the degree sequence.  for a directed graph, this could mean in-degree, out-degree, or both.  assuming it is either in-degree or out-degree, conducting a permutation test is reasonably straightforward.  for each row (or column) of the adjacency matrix, we simply uniformly permute the entries. this gives us a uniform sample from the null, which we can use to obtain an empirical estimate of the null distribution of any test statistic. 

however, if one desires that the null distribution is conditioned on both the in-degree and out-degree, or the graph is undirected, a problem arises.  in particular, that strategy will, in general, not yield a uniform sample from the null distribution of a given fixed degree sequence [proof or citation required here].  therefore, we have a few options:

1. sample from some distribution other than the uniform distribution with a given degree sequence.  this is essentially what most people do.  unfortunately, they typically do not know this.  so, doing so and knowing is better than the currect state of affiars, but still sub-optimal in many regards.
1. sample from some distribution for which we suspect is near uniform, but do not have any guarantees [citations].
1. sample from some distribution that matches certain properties of the observed sequence.  for example, we could sample from a distribution whose mean is the observed degree sequence [citations]. of course, if we don't know the variance, we won't have a good understanding of how far away we might be.  
1. sample from some distribution for which we know it has bounded error from the null distribution, with high probability, assuming at least S(n) samples, where n is the number of nodes.  there are several methods that purport to do this [citations]. they claim to have complexity ....? 
1. we can *exactly* sample from an arbitrary given fixed degree sequence, there are several methods that purport to do this [citations].  those that say they do this also say that S(n)=n^11, which makes them cost prohibitive for any reasonably sized n [citations].
1. sample from some distribution that is not uniform, but for which we know how to rescale the samples such that we can validly estimate the critical region.  several manuscripts provide a mechanism for doing something like this, in particular, they provide algorithms that they claim sample in such a way that they can estimate the mean [citations], perhaps one could extend them to estimate the 95% instead.

a related but different strategy is a parametric bootstrap.  in particular, we can estimate a degree corrected stochasitic block model to the data (somehow choosing d and k).  then, given that estimate, we can sample from that distribuition nmc times, compute the test statistic, and then, get the critical region for any significance level.  this has the conceptual advantage that we believe that the graphs we estimate tend to include noise, so we do not quite believe the degree sequence precisely.  also, if the graph is nearly a DC-SBM, then we expect this approach to have higher power.  however, if the graph does not look like a DC-SBM, then this approach will have worse power.

a numerical experiment of interest is to compute the power of the parametric bootstrap vs a version of the permutation test, for both data generated from the null and not, and see what happens as n increases.

### test statistic

there are, of course, a large number of possible test statistics. often, people are interested in determining whether the high-degree nodes are more connected than we would expect by chance.  a few natural test statistics for this:

1. scan statistic
2. q of the kidney egg model, or b11 of the SBM-2 model
3. rich club coefficient for a given k

all of these are fine.  if you know already the number of clusters of interest, and the data look like a DC-SBM, i suspect 2 will be more effective.  however, for other generative models, or if there is no obvious clustering of the data, the other test statistics might be more effective.

### diagnostics

for the permutation test, and the bootstrap, it is nice to know whether we have generated enough samples such that our estimators have converged.  thus, having some tool that enables us to assess that would be helpful. possibly we can use a simple generative model and test statistic for which we know what the null distribution is, an therefore, we can look at our estimates as a function of sample size.


### connectomics

in connectomics, people have argued for the existence of a "rich club", meaning a set of nodes that are more connected than one would expect "by chance".  they tend to mean "under a fixed degree sequence".  however, they have not yet quite tested that exactly.  thus, we can check the power of different null distributions using their test statistic, to see which one we recommend in practice.


### discussion

we now understand the various possible tests one could use for 1-sample graph testing.
we have compared different ways of generating the null distribution, and different test statistics.
we have determined that rich clubs do (not) exist with p-value X.XX in the brain networks that we have observed. 


to determine whether there is more clustering of the "high degree







# CovariateError
