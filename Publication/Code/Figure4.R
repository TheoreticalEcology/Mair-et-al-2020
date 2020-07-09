### Note: Run code in 'Simulation1.R' first to obtain dataframe 'nonsig'.

## Calculation of error rates ----------------

# Define threshold levels:
thresholds = seq(from = 30, to = 100, by = 10)

# Prepare output dataframe:
ErrorRates <- data.frame(
  threshold = thresholds,
  mdd.ftrust = rep(NA,length(thresholds)),
  mdd.fmist = rep(NA,length(thresholds)),
  post.ftrust = rep(NA,length(thresholds)),
  post.fmist = rep(NA,length(thresholds)),
  CI.ftrust = rep(NA,length(thresholds)),
  CI.fmist = rep(NA,length(thresholds))
)

# Calculate false trust and false mistrust rates:
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


## Plot false trust/ false mistrust rates (pareto-plot):------------
#
op <- par(mar = c(5,6,1,1), mfrow=c(1,1))

# pMDD:
plot(ErrorRates$mdd.ftrust, ErrorRates$mdd.fmist, xlim = c(-0.1, 1), ylim = c(-0.05,1), 
     type = "l", xlab = "False trust rate", ylab = "", bty = "l", las = 1, 
     cex.lab = 1.5, cex.axis = 1.5, col = "darkred", lwd = 2)
mtext("False mistrust rate", side = 2, line = 3.5, cex = 1.5)
text(ErrorRates$mdd.ftrust, ErrorRates$mdd.fmist, labels = ErrorRates$threshold, 
     cex= 1.5, col = "darkred", font = 4, pos = c(4, 4, 4, rep(3, 5)))
points(ErrorRates$mdd.ftrust, ErrorRates$mdd.fmist, pch = 20, col = "darkred")

# pMDE:
lines(ErrorRates$post.ftrust, ErrorRates$post.fmist, col = "darkgreen", lty = 3, lwd = 2)
text(ErrorRates$post.ftrust, ErrorRates$post.fmist, labels=ErrorRates$threshold, 
     cex= 1.5, col = "darkgreen", pos = c(4, 2, 4, 4, 3, 3, 3, 3))
points(ErrorRates$post.ftrust, ErrorRates$post.fmist, pch = 20, col = "darkgreen")

# pCI:
lines(ErrorRates$CI.ftrust, ErrorRates$CI.fmist, col = "darkblue", lty = 2, lwd = 2)
text(ErrorRates$CI.ftrust, ErrorRates$CI.fmist, labels=ErrorRates$threshold, 
     cex= 1.5, col = "darkblue", pos = c(rep(2, 4), rep(1, 4)))
points(ErrorRates$CI.ftrust, ErrorRates$CI.fmist, pch = 20, col = "darkblue")

# Add legend:
arrows(0.4 , 0.8, 0.5, 0.8, angle = 0, col = "darkblue", lty = 2, lwd = 2, length = 0)
arrows(0.4 , 0.7, 0.5, 0.7, angle = 0, col = "darkgreen", lty = 3, lwd = 2, length = 0)
arrows(0.4 , 0.6, 0.5, 0.6, angle = 0, col = "darkred", lty = 1, lwd = 2, length = 0)
text(0.52 , 0.8, "pCI", col = "darkblue", pos=4, offset= 0, cex = 1.5)
text(0.52 , 0.7, "pMDE", col = "darkgreen", pos=4, offset= 0, cex = 1.5)
text(0.52 , 0.6, "pMDD", col = "darkred", pos=4, offset= 0, cex = 1.5)

par(op)










