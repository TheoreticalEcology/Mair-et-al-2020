# Define variances for group 1 and 2
variances <- data.frame(
  variance1 = seq(from = 0, to = 250, by = 1),
  variance2 = seq(from = 500, to = 250, by = -1)
)
# Define sample sizes ranging from 2 to 10
sampleSize = c(2, 3, 4, 5, 6, 8, 10)

# Create empty output dataframe
out2 = data.frame(
  N = NA,
  mdd.power2 = NA,
  vardiff = NA
)

# Calculate MDD power for each sample size and different variance combinations, 
# and store results in output data frame
for (i in 1:length(sampleSize)) 
{
  N = sampleSize[i]
  for (j in 1:length(variances$variance1))
  {
    variance1 = variances$variance1[j]
    variance2 = variances$variance2[j]
    # Calculate degrees of freedom using the Welch-Satterthwaite approximation:
    df2 = (variance1/N + variance2/N)^2 / ((variance1/N)^2/(N-1) + (variance2/N)^2/(N-1))
    # Calculate MDD power:
    mdd.power2 <- pt(qt(p = 0.05/1, df = df2, lower.tail=F),
                     df = df2, ncp = qt(0.05/1, df = df2, lower.tail=F),
                     lower.tail=F)
    temp <- data.frame(
      N = N,
      mdd.power2 = mdd.power2,
      vardiff = variance2 - variance1
    )
    out2 <- rbind(out2, temp)
  }
}
out2 <- na.omit(out2)

# Plot results:
par(mar = c(4, 4, 1, 3))
plot(mdd.power2 ~ vardiff, data = out2, type="n", bty = "l",
     ylab = "MDD power", xlab = "Difference between groups' variances")
for (k in sampleSize)
{
  sub <- subset(out2, out2$N == k)
  lines(smooth.spline(sub$vardiff, sub$mdd.power2))
  mtext(paste("N =",as.character(k)), side = 4, line = 0, las =1, 
        at = max(sub$mdd.power2))
}

