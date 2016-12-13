rm(list=ls()); 
mean1 = 0
mean2 = 0.1
sigma = 1
alpha = 0.05

k<-0 
power_boot <- c()
n_sample <- seq(50, 3000, 100)
for (n in n_sample){
  
  D0 <- rnorm (n=n, mean=0, sd=sigma)
  D1 <- rnorm (n=n, mean=mean1, sd=sigma)
  D2 <- rnorm (n=n, mean=mean2, sd=sigma)
  
  
  #estimate parameters
  D0_mean_est <- mean(D0)
  D1_mean_est <- mean(D1)
  D2_mean_est <- mean(D2)
  
  D_null <- list()
  D_alt <- list()
  ts_null <- c()
  ts_alt <- c()
  boot_strap <-1000
  for (b in seq(1, boot_strap, 1)){
    #generate from parameter estiamtes 
    set.seed(b)
    D_null[[b]] <- rnorm(n=n, mean = D1_mean_est, sd=sigma) - rnorm(n=n, mean = D0_mean_est, sd=sigma)
    D_alt[[b]] <- rnorm(n=n, mean = D2_mean_est, sd=sigma) - rnorm(n=n, mean = D0_mean_est, sd=sigma)
    #D_null[[b]] <- rnorm(n=n, mean = D0_mean_est, sd=sigma) 
    #D_alt[[b]] <- rnorm(n=n, mean = D2_mean_est, sd=sigma) 
    
    ts_null[b] <- mean(D_null[[b]])
    ts_alt[b] <- mean(D_alt[[b]])
  }
  
  k <- k+1
  ts_critical_null <- quantile(ts_null , 0.95)[[1]]
  power_boot[k] <- sum(ts_alt>ts_critical_null)/boot_strap # =( 1- ecdf(ts_alt)(ts_critical_null))
}


plot(n_sample, power_boot, title="Power")
