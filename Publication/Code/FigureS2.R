# Load MDD function:
source("./Publication/MDD.R", local = T)

## Simulate experiments with effect size = 0.2 vs. zero-effect:-----

# Set number of experiments to be simulated:
size = 2000

# Create dataframe and specify real world and experimental conditions:
experiments = data.frame(
  effect = c(rep("no",size/2),rep("yes",size/2)),
  effectsize = c(rep(0,size/2),rep(0.2,size/2)), 
  theta = runif(size,max = 50,min=1),
  sampleSize = sample(3:10,size, replace = T),
  abund = sample(10:100, size, replace=T)
)
experiments$variance = experiments$theta * experiments$abund

# Create empty output dataframe:
out <- data.frame(
  p = rep(NA,size),
  pMDD = rep(NA,size),
  pMDE = rep(NA,size),
  pCI = rep(NA,size)
)

# Simulate experiments, analyze experimental data and store results into output dataframe:
for (i in 1:size)
{
  N <- experiments$sampleSize[i]
  theta <- experiments$theta[i]
  abund <- experiments$abund[i]
  effectsize <- experiments$effectsize[i]
  #simulate experiments:
  control <- rnbinom(n = N, mu = abund, size = abund/(theta-1))
  tmean <- abund * (1 - effectsize)
  treatment <- rnbinom(n = N, mu = tmean, size = tmean/(theta-1))
  #log-transform data prior to analysis:
  transc <- log((2*control)+1)
  transt <- log((2*treatment)+1)
  #tests and calculations on transformed data:
  test <- t.test(transc, transt, alternative = "greater", var.equal = T)
  mdd.ln <- MDD(N1 = N, N2 = N, variance1 = var(c(transc-mean(transc),transt-mean(transt))),
                alpha=0.05, two.sided = F, var.equal = T)
  postpower <- power.t.test(n = N, delta = NULL, sd = sd(c(transc-mean(transc),transt-mean(transt))), 
                            sig.level = 0.05, alternative="one.sided", type = "two.sample", 
                            power = 0.8)
  upperCI <- mean(transc)-mean(transt) + 
    qt(0.05/1, df = 2*N-2, lower.tail = FALSE) * sqrt(var(c(transc-mean(transc),
                                                            transt-mean(transt)))) * sqrt(2/N)
  # Backtransformation:
  mdd.abu <- (exp(mean(transc))-exp(mean(transc) - mdd.ln$mdd))/2
  mde.abu <- (exp(mean(transc))-exp(mean(transc) - postpower$delta))/2
  upperCI.abu <- (exp(mean(transc))-exp(mean(transc) - upperCI))/2
  # Relation to control mean:
  pMDD <- 100 * mdd.abu / ((exp(mean(transc))-1)/2)
  pMDE <- 100 * mde.abu / ((exp(mean(transc))-1)/2)
  pCI <- 100 * upperCI.abu / ((exp(mean(transc))-1)/2)
  # Store in output data frame:
  out[i,1] <- test$p.value
  out[i,2] <- pMDD
  out[i,3] <- pMDE
  out[i,4] <- pCI
}
# Combine the dataframe with experimental and real world parameters 
# and the output dataframe:
all <- cbind(experiments, out)

# Filter experiments based on p-values from the t test:
nonsig <- all[all$p >= 0.05,]

# Define thresholds to be applied to secondary filters:
thresholds = seq(from = 30, to = 100, by = 10)

# Create empty dataframe to be filled with false trust and false mistrust rates:
ErrorRates <- data.frame(
  threshold = thresholds,
  mdd.ftrust = rep(NA,length(thresholds)),
  mdd.fmist = rep(NA,length(thresholds)),
  post.ftrust = rep(NA,length(thresholds)),
  post.fmist = rep(NA,length(thresholds)),
  CI.ftrust = rep(NA,length(thresholds)),
  CI.fmist = rep(NA,length(thresholds))
)

# Calculate false trust and false mistrust rates for 
# pMDD, pMDE and pCI and write results into the ErrorRates dataframe:
for (i in 1:length(thresholds))
{
  mdd.ftrust <- length(which(nonsig$pMDD <= ErrorRates$threshold[i] & 
                               nonsig$effect == "yes"))/length(which(nonsig$effect == "yes"))
  mdd.fmist <- length(which(nonsig$pMDD > ErrorRates$threshold[i] & 
                              nonsig$effect == "no"))/length(which(nonsig$effect == "no"))
  post.ftrust <- length(which(nonsig$pMDE <= ErrorRates$threshold[i] & 
                                nonsig$effect == "yes"))/length(which(nonsig$effect == "yes"))
  post.fmist <- length(which(nonsig$pMDE > ErrorRates$threshold[i] & 
                               nonsig$effect == "no"))/length(which(nonsig$effect == "no")) 
  CI.ftrust <- length(which(nonsig$pCI <= ErrorRates$threshold[i] & 
                              nonsig$effect == "yes"))/length(which(nonsig$effect == "yes"))
  CI.fmist <- length(which(nonsig$pCI > ErrorRates$threshold[i] & 
                             nonsig$effect == "no"))/length(which(nonsig$effect == "no")) 
  ErrorRates[i,2] <- mdd.ftrust
  ErrorRates[i,3] <- mdd.fmist
  ErrorRates[i,4] <- post.ftrust
  ErrorRates[i,5] <- post.fmist
  ErrorRates[i,6] <- CI.ftrust
  ErrorRates[i,7] <- CI.fmist
}

op <- par(mar = c(5,6,1,1), mfrow=c(1,1))

# Plot pMDD error rates:
plot(ErrorRates$mdd.ftrust, ErrorRates$mdd.fmist, xlim = c(-0.1, 1), ylim = c(-0.05,1), 
     type = "l", xlab = "False trust rate", ylab = "", bty = "l", las = 1, 
     cex.lab = 1.5, cex.axis = 1.5, col = "darkred", lwd = 2)
mtext("False mistrust rate", side = 2, line = 3.5, cex = 1.5)
text(ErrorRates$mdd.ftrust, ErrorRates$mdd.fmist, labels = ErrorRates$threshold, 
     cex= 1.5, col = "darkred", font = 4, pos = c(4, 4, 4, rep(3, 5)))
points(ErrorRates$mdd.ftrust, ErrorRates$mdd.fmist, pch = 20, col = "darkred")
text(-0.05, 1, "A", cex = 1.5)

# Add pMDE error rates:
lines(ErrorRates$post.ftrust, ErrorRates$post.fmist, col = "darkgreen", lty = 3, lwd = 2)
text(ErrorRates$post.ftrust, ErrorRates$post.fmist, labels=ErrorRates$threshold, 
     cex= 1.5, col = "darkgreen", pos = c(4, 2, 4, 4, 3, 3, 3, 3))
points(ErrorRates$post.ftrust, ErrorRates$post.fmist, pch = 20, col = "darkgreen")

# Add pCI error rates
lines(ErrorRates$CI.ftrust, ErrorRates$CI.fmist, col = "darkblue", lty = 2, lwd = 2)
text(ErrorRates$CI.ftrust, ErrorRates$CI.fmist, labels=ErrorRates$threshold, 
     cex= 1.5, col = "darkblue", pos = c(rep(2, 4), rep(1, 4)))
points(ErrorRates$CI.ftrust, ErrorRates$CI.fmist, pch = 20, col = "darkblue")
par(op)


## Simulate experiments with effect size = 0.8 vs. zero-effect:-----------

# Set number of experiments to be simulated:
size = 2000

# Create dataframe and specify real world and experimental conditions:
experiments = data.frame(
  effect = c(rep("no",size/2),rep("yes",size/2)),
  effectsize = c(rep(0,size/2),rep(0.8,size/2)), 
  theta = runif(size,max = 50,min=1),
  sampleSize = sample(3:10,size, replace = T),
  abund = sample(10:100, size, replace=T)
)
experiments$variance = experiments$theta * experiments$abund

# Create empty output dataframe:
out <- data.frame(
  p = rep(NA,size),
  pMDD = rep(NA,size),
  pMDE = rep(NA,size),
  pCI = rep(NA,size)
)

# Simulate experiments, analyze experimental data and store results into output dataframe:
for (i in 1:size)
{
  N <- experiments$sampleSize[i]
  theta <- experiments$theta[i]
  abund <- experiments$abund[i]
  effectsize <- experiments$effectsize[i]
  #simulate experiments:
  control <- rnbinom(n = N, mu = abund, size = abund/(theta-1))
  tmean <- abund * (1 - effectsize)
  treatment <- rnbinom(n = N, mu = tmean, size = tmean/(theta-1))
  #log-transform data prior to analysis:
  transc <- log((2*control)+1)
  transt <- log((2*treatment)+1)
  #tests and calculations on transformed data:
  test <- t.test(transc, transt, alternative = "greater", var.equal = T)
  mdd.ln <- MDD(N1 = N, N2 = N, variance1 = var(c(transc-mean(transc),transt-mean(transt))),
                alpha=0.05, two.sided = F, var.equal = T)
  postpower <- power.t.test(n = N, delta = NULL, sd = sd(c(transc-mean(transc),transt-mean(transt))), 
                            sig.level = 0.05, alternative="one.sided", type = "two.sample", 
                            power = 0.8)
  upperCI <- mean(transc)-mean(transt) + 
    qt(0.05/1, df = 2*N-2, lower.tail = FALSE) * sqrt(var(c(transc-mean(transc),
                                                            transt-mean(transt)))) * sqrt(2/N)
  # Backtransformation:
  mdd.abu <- (exp(mean(transc))-exp(mean(transc) - mdd.ln$mdd))/2
  mde.abu <- (exp(mean(transc))-exp(mean(transc) - postpower$delta))/2
  upperCI.abu <- (exp(mean(transc))-exp(mean(transc) - upperCI))/2
  # Relation to control mean:
  pMDD <- 100 * mdd.abu / ((exp(mean(transc))-1)/2)
  pMDE <- 100 * mde.abu / ((exp(mean(transc))-1)/2)
  pCI <- 100 * upperCI.abu / ((exp(mean(transc))-1)/2)
  # Store in output data frame:
  out[i,1] <- test$p.value
  out[i,2] <- pMDD
  out[i,3] <- pMDE
  out[i,4] <- pCI
}
# Combine the dataframe with experimental and real world parameters 
# and the output dataframe:
all <- cbind(experiments, out)

# Filter experiments based on p-values from the t test:
nonsig <- all[all$p >= 0.05,]

# Define thresholds to be applied to secondary filters:
thresholds = seq(from = 30, to = 100, by = 10)

# Create empty dataframe to be filled with false trust and false mistrust rates:
ErrorRates <- data.frame(
  threshold = thresholds,
  mdd.ftrust = rep(NA,length(thresholds)),
  mdd.fmist = rep(NA,length(thresholds)),
  post.ftrust = rep(NA,length(thresholds)),
  post.fmist = rep(NA,length(thresholds)),
  CI.ftrust = rep(NA,length(thresholds)),
  CI.fmist = rep(NA,length(thresholds))
)

# Calculate false trust and false mistrust rates for 
# pMDD, pMDE and pCI and write results into the ErrorRates dataframe:
for (i in 1:length(thresholds))
{
  mdd.ftrust <- length(which(nonsig$pMDD <= ErrorRates$threshold[i] & 
                               nonsig$effect == "yes"))/length(which(nonsig$effect == "yes"))
  mdd.fmist <- length(which(nonsig$pMDD > ErrorRates$threshold[i] & 
                              nonsig$effect == "no"))/length(which(nonsig$effect == "no"))
  post.ftrust <- length(which(nonsig$pMDE <= ErrorRates$threshold[i] & 
                                nonsig$effect == "yes"))/length(which(nonsig$effect == "yes"))
  post.fmist <- length(which(nonsig$pMDE > ErrorRates$threshold[i] & 
                               nonsig$effect == "no"))/length(which(nonsig$effect == "no")) 
  CI.ftrust <- length(which(nonsig$pCI <= ErrorRates$threshold[i] & 
                              nonsig$effect == "yes"))/length(which(nonsig$effect == "yes"))
  CI.fmist <- length(which(nonsig$pCI > ErrorRates$threshold[i] & 
                             nonsig$effect == "no"))/length(which(nonsig$effect == "no")) 
  ErrorRates[i,2] <- mdd.ftrust
  ErrorRates[i,3] <- mdd.fmist
  ErrorRates[i,4] <- post.ftrust
  ErrorRates[i,5] <- post.fmist
  ErrorRates[i,6] <- CI.ftrust
  ErrorRates[i,7] <- CI.fmist
}

op <- par(mar = c(5,6,1,1), mfrow=c(1,1))

# Plot pMDD error rates:
plot(ErrorRates$mdd.ftrust, ErrorRates$mdd.fmist, xlim = c(-0.1, 1), ylim = c(-0.05,1), 
     type = "l", xlab = "False trust rate", ylab = "", bty = "l", las = 1, 
     cex.lab = 1.5, cex.axis = 1.5, col = "darkred", lwd = 2)
mtext("False mistrust rate", side = 2, line = 3.5, cex = 1.5)
text(ErrorRates$mdd.ftrust, ErrorRates$mdd.fmist, labels = ErrorRates$threshold, 
     cex= 1.5, col = "darkred", font = 4, pos = c(4, 4, 4, rep(3, 5)))
points(ErrorRates$mdd.ftrust, ErrorRates$mdd.fmist, pch = 20, col = "darkred")
text(-0.05, 1, "B", cex = 1.5)

# Add pMDE error rates:
lines(ErrorRates$post.ftrust, ErrorRates$post.fmist, col = "darkgreen", lty = 3, lwd = 2)
text(ErrorRates$post.ftrust, ErrorRates$post.fmist, labels=ErrorRates$threshold, 
     cex= 1.5, col = "darkgreen", pos = c(4, 2, 4, 4, 3, 3, 3, 3))
points(ErrorRates$post.ftrust, ErrorRates$post.fmist, pch = 20, col = "darkgreen")

# Add pCI error rates
lines(ErrorRates$CI.ftrust, ErrorRates$CI.fmist, col = "darkblue", lty = 2, lwd = 2)
text(ErrorRates$CI.ftrust, ErrorRates$CI.fmist, labels=ErrorRates$threshold, 
     cex= 1.5, col = "darkblue", pos = c(rep(2, 4), rep(1, 4)))
points(ErrorRates$CI.ftrust, ErrorRates$CI.fmist, pch = 20, col = "darkblue")
par(op)