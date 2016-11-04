
######
######
######
#PLOT ER Scan Statistic


fs <- function (n, p, s){


a <- 1/2 * p^3 * n^2 + p^2 * sqrt(p * (1 - p) * n^3) *
    sqrt(2 * log(n)) * (1 - (log(log(n)) - log(4 * pi^2))/(4 * log(n)))

b <- (p^2 * sqrt(p * (1 - p)* n^3))/(sqrt(2 * log(n)))

    1/b * exp(((s-a)/b - exp(s - a)/b))
} 


p <- 0.1
n <- 1000
s <- seq(500,1000,0.1)
n2 <- 1500
s2 <- seq(1500,2000,0.1)
par(mfrow=c(2,1))
plot(s,fs(n, p, s), type='l', col='red')
plot(s,fs(n2, p, s2), type='l', col='blue')


#EMPIRICAL##
library(grid)
library(gridExtra)
source("multiplot.R")


graph_sizes = seq(100, 1000, 100)
null = "er_gnp"
null = "er_gnm"
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
    #print (z)
}

multiplot(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], plots[[7]], plots[[8]], plots[[9]], plots[[10]], cols=2)
#multiplot(for (i in (1:2)){plots[[i]]}, cols=2))


scan_er_gnp_200

scan_sbm_200 = read.csv("scan_sbm_200.csv")[[2]]



df <-data.frame(scan_null=scan_null_200, scan_sbm=scan_sbm_200)
scan = c(scan_null_200, scan_sbm_200)
type = c(rep(("o"), length(scan_null_200)), rep(("u"), length(scan_sbm_200)))
type = as.factor(type)
df <-data.frame(scan_df=scan, type_df=type)
z <- ggplot(df, aes(scan_df, fill = type_df)) +
    geom_density(alpha = 0.2) + xlim(60, 180) + ggtitle("Graph_size: 200") 




###OLD PLOTS###
scan_sbm_200 = read.csv("scan_sbm_200.csv")[[2]]
scan_sbm_400 = read.csv("scan_sbm_400.csv")[[2]]
scan_sbm_800 = read.csv("scan_sbm_800.csv")[[2]]
scan_sbm_1000 = read.csv("scan_sbm_1000.csv")[[2]]


scan_null_200 = read.csv("scan_er_gnp_200.csv")[[2]]
scan_null_400 = read.csv("scan_er_gnp_400.csv")[[2]]
scan_null_800 = read.csv("scan_er_gnp_800.csv")[[2]]
scan_null_1000 = read.csv("scan_er_gnp_1000.csv")[[2]]
er_vs_sbm <- read.csv("power_er_gnp_vs_sbmgraphsize_vary.csv")
er_critical <- er_vs_sbm$scan_critical_df 

#dev.off()
quartz()
par(mfrow=c(4,1))
hist (scan_null_200, prob=TRUE, col="lightblue", breaks='Scott', xlim =c(min(scan_null_200), max(scan_sbm_200)))
hist (scan_sbm_200, add=TRUE, prob=TRUE, col="purple", breaks='Scott')
par(xpd=FALSE)
abline(v = er_critical[2], b = 0, col = "red", lty=2)
legend('topright', c("SBM graph", "ER graph"), col=c("purple", "blue"), lty=1, cex=0.8)
#mtext(power_str)
hist (scan_null_400, prob=TRUE, col="lightblue", breaks='Scott', xlim =c(min(scan_null_400), max(scan_sbm_400)))
hist (scan_sbm_400, add=TRUE, prob=TRUE, col="purple", breaks='Scott')
abline(v = er_critical[4], b = 0, col = "red", lty=2)
legend('topright', c("SBM graph", "ER graph"), col=c("purple", "blue"), lty=1, cex=0.8)
hist (scan_null_800, prob=TRUE, col="lightblue", breaks='Scott', xlim =c(min(scan_null_800), max(scan_sbm_800)))
hist (scan_sbm_800, add=TRUE, prob=TRUE, col="purple", breaks='Scott')
abline(v = er_critical[8], b = 0, col = "red", lty=2)
legend('topright', c("SBM graph", "ER graph"), col=c("purple", "blue"), lty=1, cex=0.8)
hist (scan_null_1000, prob=TRUE, col="lightblue", breaks='Scott', xlim =c(min(scan_null_1000), max(scan_sbm_1000)))
hist (scan_sbm_1000, add=TRUE, prob=TRUE, col="purple", breaks='Scott')
abline(v = er_critical[10], b = 0, col = "red", lty=2)
legend('topright', c("SBM graph", "ER graph"), col=c("purple", "blue"), lty=1, cex=0.8)





