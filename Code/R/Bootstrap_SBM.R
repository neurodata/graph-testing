rm(list=ls())
#SBM 2 blocks vs 3 blocks 

require(igraph)
require(raster)
####################################
##FUNCTIONS#########################
####################################
sbm.sim <- function(A, k){
  
  A_kmeans <- kmeans(A[, 1:n], k, nstart = 20)
  
  log.b = c()
  B.fit = matrix(, nrow = k, ncol = k)
  for (i in seq(1, k, 1)){
    for (j in seq(1, k, 1)){
      if (i<=j){#SBM is symmetric, reduce computation
        B.fit[i, j] = mean(A[(A_kmeans$cluster==i), (A_kmeans$cluster==j)])
      }
      else {
        B.fit[i,j] = B.fit[j, i]
      }
    }
  }
  
  block.sizes.fit = A_kmeans$size
  list(block.sizes.est = block.sizes.fit, B.est =B.fit)
}


sbm.loglik = function(A, k){
  A_kmeans <- kmeans(A[, 1:n], k, nstart = 20)
  
  log.b = c()
  
  for (i in seq(1, k, 1)){
    for (j in seq(1, k, 1)){
      if (i<=j){#SBM is symmetric, reduce computation
        b_ij = mean(A[(A_kmeans$cluster==i), (A_kmeans$cluster==j)])
        log.b_ij = sum(A[(A_kmeans$cluster==i), (A_kmeans$cluster==j)])*log(b_ij) + (A_kmeans$size[i]*A_kmeans$size[j]-sum(A[(A_kmeans$cluster==i), (A_kmeans$cluster==j)]))*log(1-b_ij)
        if (i!=j){ #take advantage of SBM symmetry
          log.b_ij = 2*log.b_ij
        }
        log.b = sum(log.b, log.b_ij)
      }
    }
  }
  
  return(log.b)
}

####################################
####################################
####################################

k_null =2
k_alt = 3
graph_size = c(seq(100, 200, 50), seq(200, 300, 20))
#graph_size = c(280)


p_value <- c()
power = c()
ctr = 0 
for (n in graph_size){
m = ceiling(n/3)
###DATA
#2block 
#p<-0.35
#q<-0.4
#block_sizes = c((n-m), m)
#pm <- cbind( c(p, q), c(q, p) )

#3block 
p<-0.55
q<-0.35
r<-0.4
block_sizes = c((n-2*m), m, m)
pm <- cbind( c(p, q, r), c(q, p, q) , c(r, q, p))
#l= r
#pm <- cbind( c(p, r, r), c(r, q, l) , c(r, l, l))
#pm <- cbind( c(p, r, r), c(r, q, r) , c(r, r, r))

g.data <- sbm.game(sum(block_sizes), pref.matrix=pm, block.sizes=block_sizes, directed=FALSE,loops=FALSE)
A.data = as.matrix(get.adjacency(g.data))
plot(raster(A.data))
#####

A.param.null <- sbm.sim(A.data,k_null)
A.param.alt <- sbm.sim(A.data,k_alt)

data.loglik.null <- sbm.loglik(A.data, 2)
data.loglik.alt <- sbm.loglik(A.data, 3)
data.stat = - 2 * (data.loglik.null - data.loglik.alt)


boot_strap <- 100
test.stat.null <- c()
test.stat.alt <- c()


for (b in seq(1, boot_strap)){

A.boot.null.sim <- sbm.game(sum(A.param.null$block.sizes.est), pref.matrix=A.param.null$B.est, block.sizes=A.param.null$block.sizes.est, directed=FALSE,loops=FALSE)
A.boot.null <- as.matrix(get.adjacency(A.boot.null.sim))

loglik.null <- sbm.loglik(A.boot.null, k_null)
loglik.alt <- sbm.loglik(A.boot.null, k_alt)
test.stat.null[b] = - 2 * (loglik.null - loglik.alt)
}

ctr = ctr+1
p_value[ctr] = sum(data.stat<test.stat.null)/boot_strap #== 1-ecdf(test.stat.null)(data.stat)
print (p_value)
critical.stat = quantile(test.stat.null, 0.95)[1]

for (b in seq(1, boot_strap)){
  A.boot.alt.sim <- sbm.game(sum(A.param.alt$block.sizes.est), pref.matrix=A.param.alt$B.est, block.sizes=A.param.alt$block.sizes.est, directed=FALSE,loops=FALSE)
  A.boot.alt <- as.matrix(get.adjacency(A.boot.alt.sim))
  
  loglik.null <- sbm.loglik(A.boot.alt, k_null)
  loglik.alt <- sbm.loglik(A.boot.alt, k_alt)
  test.stat.alt[b] = - 2 * (loglik.null - loglik.alt)
}

power[ctr]= sum(critical.stat<test.stat.alt)/boot_strap 

print (power)
}

plot(graph_size, power)

g = graph_size
p = power
################################################
################################################
################################################
################################################
plot(raster(A_sbm))
plot(raster(A))

b11 = sum(A[(A_kmeans$cluster==1), (A_kmeans$cluster==1)])/A_kmeans$size[1]^2
b12 = sum(A[(A_kmeans$cluster==1), (A_kmeans$cluster==2)])/(A_kmeans$size[1]*A_kmeans$size[2])
b22 = sum(A[(A_kmeans$cluster==2), (A_kmeans$cluster==2)])/A_kmeans$size[2]^2


b11 = mean(A[(A_kmeans$cluster==1), (A_kmeans$cluster==1)])
b12 = mean(A[(A_kmeans$cluster==1), (A_kmeans$cluster==2)])
b22 = mean(A[(A_kmeans$cluster==2), (A_kmeans$cluster==2)])

l1 = sum(A[(A_kmeans$cluster==1), (A_kmeans$cluster==1)])*log(b11) + (A_kmeans$size[1]^2-sum(A[(A_kmeans$cluster==1), (A_kmeans$cluster==1)]))*log(1-b11)
l2 = sum(A[(A_kmeans$cluster==1), (A_kmeans$cluster==2)])*log(b12) + (A_kmeans$size[1]*A_kmeans$size[2]-sum(A[(A_kmeans$cluster==1), (A_kmeans$cluster==2)]))*log(1-b12)
l3 = sum(A[(A_kmeans$cluster==2), (A_kmeans$cluster==2)])*log(b22) + (A_kmeans$size[2]^2-sum(A[(A_kmeans$cluster==2), (A_kmeans$cluster==2)]))*log(1-b22)

l1+2*l2+l3

m=0
l = c()
for (i in seq(1, k, 1)){
  for (j in seq(1, k, 1)){
    m = m+1
    b_ij = mean(A[(A_kmeans$cluster==i), (A_kmeans$cluster==j)])
    l[m] = sum(A[(A_kmeans$cluster==i), (A_kmeans$cluster==j)])*log(b_ij) + (A_kmeans$size[i]*A_kmeans$size[j]-sum(A[(A_kmeans$cluster==i), (A_kmeans$cluster==j)]))*log(1-b_ij)
  }
}

log = sum(l)


for(b in seq(1, k, 1)){
  
  
} 


n_B1 <- sum(A_kmeans$cluster==1)
n_B2 <- sum(A_kmeans$cluster==2)
B11 <- A_sbm[1:n_B1, 1:n_B1]
B22 <- A_sbm[(n_B1+1):(n_B1+n_B2), (n_B1+1):(n_B1+n_B2)]
B12 <- A_sbm[1:n_B1, (n_B1+1):(n_B1+n_B2)]
B21 <- A_sbm[(n_B1+1):(n_B1+n_B2), 1:n_B1]
print (sum(B11)/(n_B1)^2)
print (sum(B22)/(n_B2)^2)
print (sum(B12)/(n_B1*n_B2))
print (sum(B21)/(n_B2*n_B1))

L2 = (sum(B11)/(n_B1)^2)*(sum(B22)/(n_B2)^2)*(sum(B12)/(n_B1*n_B2))*(sum(B21)/(n_B2*n_B1))
print (L2)

print ("---------------")
n_B1 <- sum(A_kmeans$cluster==1)
n_B2 <- sum(A_kmeans$cluster==2)
n_B3 <- sum(A_kmeans$cluster==3)
B11 <- A_sbm[1:n_B1, 1:n_B1]
B12 <- A_sbm[1:n_B1, (n_B1+1):(n_B1+n_B2)]
B13 <- A_sbm[1:n_B1, (n_B2+n_B1+2):n]
B21 <- A_sbm[(n_B1+1):(n_B1+n_B2), 1:n_B1]
B22 <- A_sbm[(n_B1+1):(n_B1+n_B2), (n_B1+1):(n_B1+n_B2)]
B23 <- A_sbm[(n_B1+1):(n_B1+n_B2), (n_B2+n_B1+2):n]  
B31 <- A_sbm[(n_B1+n_B2+2):n, 1:n_B1]
B32 <- A_sbm[(n_B1+n_B2+2):n, (n_B1+1):(n_B1+n_B2)]
B33 <- A_sbm[(n_B1+n_B2+2):n, (n_B2+n_B1+1):n]  
  

print (sum(B11)/(n_B1)^2)
print (sum(B22)/(n_B2)^2)
print (sum(B33)/(n_B3)^2)

print (sum(B21)/(n_B1*n_B2))
print (sum(B12)/(n_B1*n_B2))
print (sum(B23)/(n_B2*n_B3))
print (sum(B32)/(n_B2*n_B3))

print (sum(B13)/(n_B1*n_B3))
print (sum(B31)/(n_B1*n_B3))

L3 = (sum(B11)/(n_B1)^2) * (sum(B22)/(n_B2)^2) * (sum(B33)/(n_B3)^2) * (sum(B21)/(n_B1*n_B2)) *(sum(B12)/(n_B1*n_B2))* (sum(B23)/(n_B2*n_B3)) * (sum(B32)/(n_B2*n_B3)) * (sum(B13)/(n_B1*n_B3)) * (sum(B31)/(n_B1*n_B3)) 
  


print (L3)
  