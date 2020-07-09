### Note: Run code in 'Simulation2.R' first to obtain dataframes 'all' and 'nonsig'.


## Plot pCI, pMDE and pMDD vs. real effectsize: ----------

# Define graphical parameters:
palette(c("grey60", "black", "red"))
ylim = max(max(all$pCI), max(all$pMDD), max(all$pMDE)) + 5
op <- par(mfrow=c(2, 3), oma = c(1, 1, 3, 1), mar = c(4, 5, 1, 1), pch = 1, bty = "l", 
          cex.lab = 1.5, las = 1)

# Figure 5 A-C: all experiments:
dotcol <- as.numeric(all$effectsize * 100 <= all$pCI)
plot(all$pCI ~ all$effectsize, ylim = c(0, ylim),  
     xlab = "", col = dotcol + 1, ylab = "pCI", pch = dotcol, 
     xaxt = "n", yaxt = "n", cex = 0.8)
axis(2, at = c(0, 50, 100, 150, 200))
axis(1, at = c(0, 0.5, 1))
abline(0, 100, col = "grey40", lwd = 2, lty = 5)
fit1 <- lm(all$pCI ~ all$effectsize)
abline(fit1, col = "darkred", lwd = 2, lty = "solid")
text(0.1, ylim, "A", cex = 1.5)

dotcol <- as.numeric(all$effectsize * 100 <= all$pMDE)
plot(all$pMDE ~ all$effectsize, ylim = c(0, ylim),  
     xlab = "", col = dotcol + 1, ylab = "pMDE", pch = dotcol, 
     xaxt = "n", yaxt = "n", cex = 0.8)
axis(2, at = c(0, 50, 100, 150, 200))
axis(1, at = c(0, 0.5, 1))
abline(0, 100, col = "grey40", lwd = 2, lty = 5)
fit2 <- lm(all$pMDE ~ all$effectsize)
abline(fit2, col = "darkred", lwd = 2, lty = "solid")
text(0.1, ylim, "B", cex = 1.5)

dotcol <- as.numeric(all$effectsize * 100 <= all$pMDD)
plot(all$pMDD ~ all$effectsize, ylim = c(0, ylim),  
     xlab = "", col = dotcol + 1, ylab = "pMDD", pch = dotcol, 
     xaxt = "n", yaxt = "n", cex = 0.8)
axis(2, at = c(0, 50, 100, 150, 200))
axis(1, at = c(0, 0.5, 1))
abline(0, 100, col = "grey40", lwd = 2, lty = 5)
fit3 <- lm(all$pMDD ~ all$effectsize)
abline(fit3, col = "darkred", lwd = 2, lty = "solid")
text(0.1, ylim, "C", cex = 1.5)


# Figure 5 D-F: only non-significant experiments:
dotcol <- as.numeric(nonsig$effectsize * 100 <= nonsig$pCI)
plot(nonsig$pCI ~ nonsig$effectsize, ylim = c(0, ylim), xlim = c(0, 1),
     xlab = "", col = dotcol + 1, ylab = "pCI", pch = dotcol, 
     xaxt = "n", yaxt = "n", cex = 0.8)
axis(2, at = c(0, 50, 100, 150, 200))
axis(1, at = c(0, 0.5, 1))
abline(0, 100, col = "grey40", lwd = 2, lty = 2)
fit1 <- lm(nonsig$pCI ~ nonsig$effectsize)
abline(fit1, col = "darkred", lwd = 2, lty = "solid")
text(0.1, ylim, "D", cex = 1.5)

dotcol <- as.numeric(nonsig$effectsize * 100 <= nonsig$pMDE)
plot(nonsig$pMDE ~ nonsig$effectsize, ylim = c(0, ylim),  xlim = c(0, 1),
     xlab = "Real effect size", col = dotcol + 1, ylab = "pMDE", pch = dotcol, 
     xaxt = "n", yaxt = "n", cex = 0.8)
axis(2, at = c(0, 50, 100, 150, 200))
axis(1, at = c(0, 0.5, 1))
abline(0, 100, col = "grey40", lwd = 2, lty = 2)
fit2 <- lm(nonsig$pMDE ~ nonsig$effectsize)
abline(fit2, col = "darkred", lwd = 2, lty = "solid")
text(0.1, ylim, "E", cex = 1.5)

dotcol <- as.numeric(nonsig$effectsize * 100 <= nonsig$pMDD)
plot(nonsig$pMDD ~ nonsig$effectsize, ylim = c(0, ylim),  xlim = c(0, 1),
     xlab = "", col = dotcol + 1, ylab = "pMDD", pch = dotcol, 
     xaxt = "n", yaxt = "n", cex = 0.8)
axis(2, at = c(0, 50, 100, 150, 200))
axis(1, at = c(0, 0.5, 1))
abline(0, 100, col = "grey40", lwd = 2, lty = 2)
fit3 <- lm(nonsig$pMDD ~ nonsig$effectsize)
abline(fit3, col = "darkred", lwd = 2, lty = "solid")
text(0.1, ylim, "F", cex = 1.5)

par(op)