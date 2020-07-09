# Load MDD function:
source("./Publication/MDD.R", local = T)

# Simulation 2: Sensitivity of the MDD and other statistical filters to real effect size

# Define number of experiments to be simulated:
size = 1000

# Define experimental/ real world parameters:
experiments = data.frame(
  effectsize = runif(n = size),
  theta = runif(size, max = 50,min = 1),
  sampleSize = sample(3:10, size = size, replace = T),
  abund = sample(10:100, size, replace=T)
)
experiments$variance = experiments$theta * experiments$abund

# Prepare empty output dataframe:
out <- data.frame(
  p = rep(NA,size),
  pMDD = rep(NA,size),
  pMDE = rep(NA,size),
  pCI = rep(NA,size)
)

# Simulate and analyze experiments:
for (i in 1:size)
{
  N <- experiments$sampleSize[i]
  theta <- experiments$theta[i]
  abund <- experiments$abund[i]
  effectsize <- experiments$effectsize[i]
  
  # Simulate experiments:
  control <- rnbinom(n = N, mu = abund, size = abund/(theta-1))
  tmean <- abund * (1 - effectsize)
  treatment <- rnbinom(n = N, mu = tmean, size = tmean/(theta-1))
  
  #log-transform data prior to analysis:
  transc <- log((2*control)+1)
  transt <- log((2*treatment)+1)
  
  # Perform tests and calculations on transformed data:
  test <- t.test(transc, transt, alternative = "greater",var.equal = T) 
  mdd.ln <- MDD(N1 = N, N2 = N, variance1 = (var(transc)+var(transt))/2,
                alpha=0.05, two.sided = F, var.equal = T)
  postpower <- power.t.test(n = N, delta = NULL, sd = sd(c(transc-mean(transc),transt-mean(transt))), 
                             sig.level = 0.05, alternative = "one.sided", type = "two.sample", power = 0.8)
  upperCI <- mean(transc)-mean(transt) + 
    qt(0.05/1, df = 2*N-2, lower.tail = FALSE) * sqrt(var(c(transc-mean(transc),transt-mean(transt)))) * sqrt(2/N)
  
  # Back-transformation:
  mdd.abu <- (exp(mean(transc))-exp(mean(transc) - mdd.ln$mdd))/2
  mde.abu <- (exp(mean(transc))-exp(mean(transc) - postpower$delta))/2
  upperCI.abu <- (exp(mean(transc))-exp(mean(transc) - upperCI))/2
  
  # Relation to control mean:
  pMDD <- 100 * mdd.abu / ((exp(mean(transc))-1)/2)
  pMDE <- 100 * mde.abu / ((exp(mean(transc))-1)/2)
  pCI <- 100 * upperCI.abu / ((exp(mean(transc))-1)/2)
  
  # Store results in output dataframe:
  out[i,1] <- test$p.value
  out[i,2] <- pMDD
  out[i,3] <- pMDE
  out[i,4] <- pCI
}

# Combine experimental and output dataframe:
all <- cbind(experiments, out)

# Subset dataframe: include only non-significant results at alpha = 0.05
nonsig <- all[all$p >= 0.05,]


