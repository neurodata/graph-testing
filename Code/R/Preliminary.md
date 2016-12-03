# Preliminary Steps - Graph Testing.
Zeinab Mousavi  




#Generating Graphs conditioned on Degree Sequence

##References
[Genio] Efficient and Exact Sampling of Simple Graphs with Given Arbitrary Degree Sequence
[Blitzstein & Diaconis] A Sequential Importance Sampling Algorithm for Generating Random Graphs with Prescribed Degrees 
[Carey] Statistical Inference on Random Graphs: Comparative Power Analyses via Monte Carlo

##Runtime - Genio
We use Genio algorithm to generate the graphs. The run times are as:

https://github.com/neurodata/graph-testing/blob/master/Figures/runtime_200_4000_3.jpeg

https://github.com/neurodata/graph-testing/blob/master/Figures/runtime_200_3200_10.jpeg

##Runtime - B&D
https://github.com/neurodata/graph-testing/blob/master/Figures/runtime_Genio_vs_BD.jpeg

##Convergence of Estimators - Genio vs B&D
Generate 2-regular graphs via Genio and B&D algorithms. 
Estimate the proportion of n-cycles in the family 2-regular graphs on n-vertices
The probability that a labeled 2-regular n-vertex graph is connected goes to about 1.876/sqrt(n)

Convergence on 6-vertex 2-regular graphs
https://github.com/neurodata/graph-testing/blob/master/Figures/conv_6v_100_100000.jpeg

Convergence on 40-vertex 2-regular graphs
https://github.com/neurodata/graph-testing/blob/master/Figures/conv_40v_100_100000.jpeg

Convergence on 100-vertex 2-regular graphs
https://github.com/neurodata/graph-testing/blob/master/Figures/conv_100v_100_100000.jpeg

##Hypothesis Tesing : ER vs SBM
Following Carey's Paper, look at distribution of the Scan statistic of ER & SBM graphs and test:
Ho:scan(ER) = scan(SBM) 
Ha:scan(SBM) > scan(ER) 
where,
ErdosRenyi (n=1000, p=0.5) vs SBM (n=950, p=0.5, m=50, q=0.1)
ErdosRenyi (n=1000, m=total_edge_observation) vs SBM (n=950, p=0.5, m=50, q=0.1)

https://github.com/neurodata/graph-testing/blob/master/Code/R/TestPower.md
 



Here is code to plot a normal distribution 
[link](www.google.com)






