require(distrEx)
require(mixtools)
###HELLINGER DISTANCE####
Hellinger_Dist <- function(d1, d2){
  min_d1 = min(hist(d1, plot=FALSE)$breaks)
  max_d1 = max(hist(d1, plot=FALSE)$breaks)
  bin_width_d1 = mean(diff((hist(d1, plot=FALSE)$breaks)))
  min_d2 = min(hist(d2, plot=FALSE)$breaks)
  max_d2 = max(hist(d2, plot=FALSE)$breaks)
  bin_width_d2 = mean(diff((hist(d2, plot=FALSE)$breaks)))
  
  bin_width = min(bin_width_d1, bin_width_d2)
  min_break = min(min_d1, min_d2)
  max_break = max(max_d1, max_d2)
  h_breaks = seq(min_break, max_break, bin_width)
  h1 = hist(d1, breaks=h_breaks, plot=FALSE)
  h2 = hist(d2, breaks=h_breaks, plot=FALSE)
  p<- h1$density * diff(h1$breaks)
  q<- h2$density * diff(h2$breaks)
  Dist <- 1/sqrt(2)*sqrt(sum((sqrt(p)-sqrt(q))^2))
  return(Dist)
}


GMM_sd <- c(1, 1)
GMM0_lambda <- c(0.3, 0.7)
GMM0_mean <- c(1, 8)
GMM1_lambda <-  c(0.3, 0.7)
GMM1_mean <-  c(1.1, 8.1)

k<-0 
alpha = 0.05
n <- seq(1000, 7000, 1000)


power_boot <- c()

for (n_sample in n){
  ts_critical_null <- c()
  p_value <- c()
  #observed data
  set.seed(1)
  GMM_0 = rnormmix(n=n_sample, lambda=GMM0_lambda, mu=GMM0_mean, sigma=GMM_sd)
  set.seed(1)
  GMM_1 = rnormmix(n=n_sample, lambda=GMM1_lambda, mu=GMM1_mean, sigma=GMM_sd)
  ts_data <- Hellinger_Dist(GMM_0, GMM_1)
  
  #Estimate Parameters of Eachdataset 
  GMM_0_EM = normalmixEM(GMM_0, k=2, maxit=1000, sigma=c(1, 1)) 
  GMM_0_lambda_estimate <- GMM_0_EM$lambda
  GMM_0_mean_estimate <- GMM_0_EM$mu
  
  GMM_1_EM = normalmixEM(GMM_1, k=2, maxit=1000, sigma=c(1, 1)) 
  GMM_1_lambda_estimate <- GMM_1_EM$lambda
  GMM_1_mean_estimate <- GMM_1_EM$mu
  
  #Estimate Paramaters of Null Distribution 
  GMM_null <- c(GMM_0, GMM_1)
  GMM_null_EM = normalmixEM(GMM_null, k=2, maxit=1000, sigma=c(1, 1)) 
  GMM_null_lambda_estimate <- GMM_null_EM$lambda
  GMM_null_mean_estimate <- GMM_null_EM$mu
  
  #Now generate Bootstrap Samples

  ts_null <- c()
  ts_alt <- c()
  boot_strap <-1000
  for (b in seq(1, boot_strap, 1)){
    #generate from parameter estiamtes 
    set.seed(b)
    GMM_0_boot <- rnormmix(n=n_sample, lambda=GMM_null_lambda_estimate, mu=GMM_null_mean_estimate, sigma=GMM_sd)
    GMM_1_boot <- rnormmix(n=n_sample, lambda=GMM_null_lambda_estimate, mu=GMM_null_mean_estimate, sigma=GMM_sd)
    
    ts_null[b] <- Hellinger_Dist(GMM_1_boot, GMM_0_boot)
  
  }
  
  k <- k+1
  ts_critical_null <- quantile(ts_null , 0.95)[[1]]
  p_value[k] <- sum(ts_null>ts_data)/boot_strap
  print (p_value[k])

  
  #Alternate Distrbution Bootstrap
  
  
  ts_null <- c()
  ts_alt <- c()
  boot_strap <-1000
  for (b in seq(1, boot_strap, 1)){
    #generate from parameter estiamtes 
    set.seed(b)
    GMM_0_boot_alt <- rnormmix(n=n_sample, lambda=GMM_0_lambda_estimate, mu=GMM_0_mean_estimate, sigma=GMM_sd)
    GMM_1_boot_alt <- rnormmix(n=n_sample, lambda=GMM_1_lambda_estimate, mu=GMM_1_mean_estimate, sigma=GMM_sd)
    ts_alt[b] <- Hellinger_Dist(GMM_1_boot_alt, GMM_0_boot_alt)
  }

power_boot[k] <- sum(ts_alt>ts_critical_null)/boot_strap 
print (k)
print (power_boot[k])
print (power_boot)

}




plot(n, power_boot)
