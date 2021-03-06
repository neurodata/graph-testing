
rm(list=ls()); 

require(ggplot2)
source("multiplot.R")
boot_Gaussian_mean <-function(sample_size, mu, sd, boot_samples){

#Generate Samples
my.Test.Data.Gauss<-rnorm(sample_size, mean = mu, sd = sd)
#Estimate Parameters 
empirical.mean.Gauss<-mean(my.Test.Data.Gauss)
#empirical.sd.Gauss <- sqrt(var(my.Test.Data.Gauss))
empirical.sd.Gauss <- sd

#Generate bootstrap samples with the computed parameter estimates 
#first, initialize a matrix that will receive the values of the
#estimate from each sample

boot.sampling.mean.Gauss<-matrix(1,boot_samples)

#Now create "boot_sample" bootstrap samples and compute the value of the stat(mean) for each of them

for (i in 1:boot_samples){
  boot.sampling.mean.Gauss[i]<-mean(rnorm(sample_size, mean = empirical.mean.Gauss, sd = empirical.sd.Gauss ))
}
return(boot.sampling.mean.Gauss)
}


#Generate 1000 sample points from Gaussian & "boot_sample" bootstrap sample sets of size 100
sample_size =1000 
i=0
plots = list()
power <- list()
for (boot_samples in seq(10, 200, 20) ) {
i=i+1
#N(mu=0, sd=1) This will be null distribution 
null_Gaussian_mean <-boot_Gaussian_mean(sample_size, mu=0.2, sd=1, boot_samples)

#N(mu=3, sd=1) This will be alternate distribution 
alt_Gaussian_mean <-boot_Gaussian_mean(sample_size, mu=0.3, sd=1, boot_samples)

#hist(null_Gaussian_mean,main="Boot Strap Estimate of sampling distribution of mean of Gaussian",breaks='Scott', prob=T, col="red", xlim=c(min(null_Gaussian_mu, alternate_Gaussian_mu), max(null_Gaussian_mu, alternate_Gaussian_mu)))
#hist(alternate_Gaussian_mean,main="Boot Strap Estimate of sampling distribution of mean of Gaussian",breaks='Scott', prob=T, add=TRUE, col="purple")

#Hypothesis Test
#Ho: mu1=mu2
#HA: mu2>mu1
#Find 95-percentile 
mean_critical <- quantile(null_Gaussian_mean, prob=0.95)[1]
power[i] <- sum(alt_Gaussian_mean[alt_Gaussian_mean>mean_critical])/sum(abs(alt_Gaussian_mean))
#print (power[1:i])

graph_title = paste(as.character(boot_samples), "Bootstrap Samples   Power:", as.character(power[i]))

null_alt_dataframe <- data.frame(mean_df=numeric(0), type_df=numeric(0))
null <- "Gaussian_mean_0.2"
null_statistics <- null_Gaussian_mean[,1]
null_type = as.factor(rep(null, length(null_Gaussian_mean)))
temp_df <- data.frame(mean_df=null_statistics, type_df=null_type)
null_alt_dataframe <- rbind(null_alt_dataframe, temp_df)
alt <- "Gaussian_mean_0.3"
alt_statistics <- alt_Gaussian_mean[,1]
alt_type = as.factor(rep(alt, length(alt_Gaussian_mean)))
temp_df <- data.frame(mean_df=alt_statistics, type_df=alt_type)
null_alt_dataframe <- rbind(null_alt_dataframe, temp_df)

plots[[i]]<- ggplot(null_alt_dataframe, aes(mean_df, fill = type_df)) +  geom_density(alpha = 0.2) +
 xlim(min(null_alt_dataframe$mean_df), max(null_alt_dataframe$mean_df)) + ggtitle(graph_title) 

#print(plots)

}



multiplot(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], plots[[7]], plots[[8]], plots[[9]], plots[[10]], cols=2)



