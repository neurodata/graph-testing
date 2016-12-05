
rm(list=ls()); 

require(ggplot2)
source("multiplot.R")


###Functions####

#Generate bootstrap samples with the computed parameter estimates 
boot_Gaussian_mean <-function(sample_size, mean, sd, boot_samples){
#first, initialize a matrix that will receive the values of the
#estimate from each sample

boot.sampling.mean.Gauss<-matrix(1,boot_samples)

#Now create "boot_sample" bootstrap samples and compute the value of the stat(mean) for each of them

for (i in 1:boot_samples){
  boot.sampling.mean.Gauss[i]<-rnorm(sample_size, mean = mean, sd = sd )
}
return(boot.sampling.mean.Gauss)
}
###EOF####

sample_size = 1000
null_Gaussian_mean <- rep(0, )
#Generate Null Set
mean_null = 0
sd_null = 1
#for (i in 1:sample_size) {null_Gaussian_mean[i] <-mean(rnorm(n=sample_size, mean=mean_null, sd=sd_null))}

Null.Data.Gauss<-rnorm(n=sample_size, mean = mean_null, sd = sd_null)
#Estimate Parameters 
empirical.mean.Gauss.Null<-mean(Null.Data.Gauss)
empirical.sd.Gauss.Null <- sd_null #assume known



#Generate Observed Set
mean_observed = 1
sd_observed = 1
Observed.Data.Gauss<-rnorm(n=sample_size, mean = mean_observed, sd = sd_observed)
#Estimate Parameters 
empirical.mean.Gauss.Obs <-mean(Observed.Data.Gauss)
#empirical.sd.Gauss <- sqrt(var(Observed.Data.Gauss)) #don't use this. assume it is known.
empirical.sd.Gauss.Obs <- sd_observed


#Find critical value on null distribution, boot strap 
boot_samples <- 10
mean_critical_boot <- rep(0,boot_samples)
for (i in seq(1, boot_samples)){
Null.Gauss.Boot <-rnorm(n=sample_size, mean=empirical.mean.Gauss.Null, sd=empirical.sd.Gauss.Null)
print (i)
mean_critical_boot[i] <- quantile(Null.Gauss.Boot, prob=0.95)[[1]]
critical_boot <- mean(mean_critical_boot)
}

#Generate 1000 sample points from Gaussian & "boot_sample" bootstrap sample sets of size 100
boot_samples <- 10000
power<- rep(0,boot_samples)
for (i in seq(1, boot_samples)){
  Alt.Gauss.Boot <-rnorm(n=sample_size, mean=empirical.mean.Gauss.Obs, sd=empirical.sd.Gauss.Obs)
  power[i] <- sum(Alt.Gauss.Boot[Alt.Gauss.Boot>critical_boot])/sum(abs(Alt.Gauss.Boot))
  power <- mean(power)
}

#############################################
#############################################
#############################################
#############################################
#############################################

sample_size = 1000
null_Gaussian_mean <- rep(0, )
#Generate Null Set
mean_null = 0
sd_null = 1
for (i in 1:sample_size) {null_Gaussian_mean[i] <-mean(rnorm(n=sample_size, mean=mean_null, sd=sd_null))}

#Null.Data.Gauss<-rnorm(n=sample_size, mean = mean_null, sd = sd_null)
##Estimate Parameters 
#empirical.mean.Gauss.Null<-mean(Null.Data.Gauss)
#empirical.sd.Gauss.Null <- sd_null



#Generate Observed Set
mean_observed = 0.05
sd_observed = 1
Observed.Data.Gauss<-rnorm(n=sample_size, mean = mean_observed, sd = sd_observed)
#Estimate Parameters 
empirical.mean.Gauss.Obs <-mean(Observed.Data.Gauss)
#empirical.sd.Gauss <- sqrt(var(Observed.Data.Gauss)) #don't use this. assume it is known.
empirical.sd.Gauss.Obs <- sd_observed

#Generate 1000 sample points from Gaussian & "boot_sample" bootstrap sample sets of size 100
i=0
plots = list()
power <- c()
steps <- (seq(
#set.seed(1)
#steps <- c(10)
for (boot_samples in steps ) {
i=i+1
# This will be null distribution 
#null_Gaussian_mean <-boot_Gaussian_mean(sample_size, mean=empirical.mean.Gauss.Null, sd=empirical.sd.Gauss.Null, boot_samples)
null_Gaussian_mean <-boot_Gaussian_mean(sample_size, mean=mean_null, sd=sd_null, boot_samples)

# This will be alternate distribution 
#alt_Gaussian_mean <-boot_Gaussian_mean(sample_size, mean=empirical.mean.Gauss.Obs, sd=empirical.sd.Gauss.Obs, boot_samples)
alt_Gaussian_mean <-boot_Gaussian_mean(sample_size, mean=mean_observed, sd=sd_observed, boot_samples)

mean_critical <- quantile(null_Gaussian_mean, prob=0.95)[1]
power[i] <- sum(alt_Gaussian_mean[alt_Gaussian_mean>mean_critical])/sum(abs(alt_Gaussian_mean))
}
print (power)


hist(null_Gaussian_mean,main="Boot Strap Estimate of sampling distribution of mean of Gaussian",breaks='Scott', prob=T, col="red", xlim=c(min(null_Gaussian_mean, alt_Gaussian_mean), max(null_Gaussian_mean, alt_Gaussian_mean)))
hist(alt_Gaussian_mean,main="Boot Strap Estimate of sampling distribution of mean of Gaussian",breaks='Scott', prob=T, add=TRUE, col="purple")

#Hypothesis Test
#Ho: mu1=mu2
#HA: mu2>mu1
#Find 95-percentile 


graph_title = paste(as.character(boot_samples), "Bootstrap Samples   Power:", as.character(power[i]))

null_alt_dataframe <- data.frame(mean_df=numeric(0), type_df=numeric(0))
null <- "Gaussian_mean_0.2"
#null_statistics <- null_Gaussian_mean[,1]
null_statistics <- null_Gaussian_mean
null_type = as.factor(rep(null, length(null_Gaussian_mean)))
temp_df <- data.frame(mean_df=null_statistics, type_df=null_type)
null_alt_dataframe <- rbind(null_alt_dataframe, temp_df)
alt <- "Gaussian_mean_0.3"
alt_statistics <- alt_Gaussian_mean[,1]
alt_type = as.factor(rep(alt, length(alt_Gaussian_mean)))
temp_df <- data.frame(mean_df=alt_statistics, type_df=alt_type)
null_alt_dataframe <- rbind(null_alt_dataframe, temp_df)

#plots[[i]]<- ggplot(null_alt_dataframe, aes(mean_df, fill = type_df)) +  geom_density(alpha = 0.2) +
# xlim(min(null_alt_dataframe$mean_df), max(null_alt_dataframe$mean_df)) + ggtitle(graph_title) 







plot(steps, power)
#multiplot(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], plots[[7]], plots[[8]], plots[[9]], plots[[10]], cols=2)





