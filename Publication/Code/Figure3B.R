# Define sample sizes ranging from 2 to 10
sampleSize = c(seq(from = 2, to = 10, by = 1))

# Create empty data frame
out1 = data.frame(
  N = sampleSize,
  mddpower1 = rep(NA, length(sampleSize))
)

# Calculate MDD power for each sample size and store results in data frame 'out'
for (i in 1:length(sampleSize)) 
{
  N = sampleSize[i]
  df1 = N + N - 2
  # Calculate MDD power:
  mdd.power1 <- pt(qt(p = 0.05/1, df = df1, lower.tail = F),
                   df = df1, ncp = qt(0.05/1, df = df1, lower.tail = F),
                   lower.tail = F)
  out1$mddpower1[i] <- mdd.power1
}

# Plot 
op = par(mfrow = c(1, 1), mar = c(5, 5, 1, 1), las = 1, cex = 1.5, yaxt = "s", xaxt = "s")
plot(mddpower1 ~ N, data = out1, ylim = c(0.5, 0.6), pch = 19, bty = "l",
     ylab = "MDD power", xlab = "Sample size", cex.lab = 1.5, yaxt = "n", xaxt = "n")
axis(2, at = c(0.5, 0.55, 0.6), cex.lab = 1, labels =T)
axis(1, at = c(seq(2, 10, 1)), labels = T, cex.lab = 1)
text(10, 0.59, "alpha = 0.05", cex= 1, pos = 2, col ="grey50")
par(op)

