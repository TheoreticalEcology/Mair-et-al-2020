### Note: Run code in 'Simulation2.R' first to obtain dataframes 'all' and 'nonsig'.

# Load packages:
library(dplyr)

## Apply secondary filter with different threshold levels to non-significant experiments:

# Prepare empty output dataframe:
outeffect <- data.frame(
  indicator = NA,
  threshold = NA,
  effectsize = NA
)

# Filter non-signiificant experiments based on pCI, pMDE and pMDD and different threshold levels:-----
ind <- list("pCI", "pMDE", "pMDD")

for (i in ind){
  for (j in 30:100){
    sub <- nonsig[nonsig[, i] <= j, ]
    temp <- data.frame(
      indicator = rep(i, nrow(sub)),
      threshold = rep(j, nrow(sub)),
      effectsize = sub$effectsize
    )
    outeffect <- rbind(outeffect, temp)
  }
}
outeffect <- na.omit(outeffect)

# Subset output by indicator:
pCIpass <- outeffect[outeffect$indicator == "pCI", ] 
pMDEpass <- outeffect[outeffect$indicator == "pMDE", ] 
pMDDpass <- outeffect[outeffect$indicator == "pMDD", ] 

# Calculate summary statistics per threshold level for each indicator:
summ.pCI <- pCIpass %>%
  group_by(threshold) %>%
  summarise(min = min(effectsize), q25 = quantile(effectsize, 0.25), median = median(effectsize), 
            q75 = quantile(effectsize, 0.75), max = max(effectsize), N = n(), 
            mean = mean(effectsize), sd = sd(effectsize))
summ.pMDE <- pMDEpass %>%
  group_by(threshold) %>%
  summarise(min = min(effectsize), q25 = quantile(effectsize, 0.25), median = median(effectsize), 
            q75 = quantile(effectsize, 0.75), max = max(effectsize), N = n(),
            mean = mean(effectsize), sd = sd(effectsize))
summ.pMDD <- pMDDpass %>%
  group_by(threshold) %>%
  summarise(min = min(effectsize), q25 = quantile(effectsize, 0.25), median = median(effectsize), 
            q75 = quantile(effectsize, 0.75), max = max(effectsize), N = n(),
            mean = mean(effectsize), sd = sd(effectsize))


## Plot summary statistics for effect sizes passing the different thresholds applied to the three indicators:-----

## Define figure layout:
layout(matrix(c(1,2,3,4), 1, 4, byrow = TRUE), width = c(0.5,1,1,1))

# plot1: effects sizes passing p-value filter:
par(mar = c(3,0,1,0), oma = c(0.5,8,0,0), cex = 0.8, bty = "n")

boxplot(nonsig$effectsize, bty = "n", col = "grey90", yaxt = "n", ylim = c(0,1.03), pch = 20)
mtext(c("p-value filter","max", "75%", "median", "25%", "min"), 
      side = 2, line = -1, las = 2, cex = 0.8, 
      at = c(1.03, max(nonsig$effectsize), quantile(nonsig$effectsize, 0.75), median(nonsig$effectsize),
             quantile(nonsig$effectsize, 0.25), min(nonsig$effectsize)))
axis(side = 2, las = 2, labels = T, at = seq(0, 1, by =0.1), line = 4, outer = T)
mtext("Real effect size", side = 2, line = 7, outer = T)

## plot2: pCI 
par(mar = c(3,0,1,1), cex = 0.8, bty = "n")

plot(summ.pCI$max ~ summ.pCI$threshold, type ="n", ylim = c(0,1.03), xlim = rev(c(30,100)),
     las = 2, xlab = "", ylab = "", las = 1, bty ="l", yaxt = "n", xaxt = "n")
axis(side = 1)
xcor = c(summ.pCI$threshold, rev(summ.pCI$threshold))
ycor = c(smooth.spline(summ.pCI$max ~ summ.pCI$threshold)$y, rev(smooth.spline(summ.pCI$min ~ summ.pCI$threshold)$y))
polygon(xcor, ycor, border = NA, col = adjustcolor("slategray1", alpha.f = 0.2))
ycor = c(smooth.spline(summ.pCI$q75 ~ summ.pCI$threshold)$y, rev(smooth.spline(summ.pCI$q25 ~ summ.pCI$threshold)$y))
polygon(xcor, ycor, border =NA, col = "slategray1")
lines(smooth.spline(summ.pCI$max ~ summ.pCI$threshold), lty = 3, col = "darkblue")
lines(smooth.spline(summ.pCI$q75 ~ summ.pCI$threshold), lty = 2, col = "darkblue")
lines(smooth.spline(summ.pCI$median ~ summ.pCI$threshold), lty = 1, col = "darkblue")
lines(smooth.spline(summ.pCI$q25 ~ summ.pCI$threshold), lty = 2, col = "darkblue")
lines(smooth.spline(summ.pCI$min ~ summ.pCI$threshold), lty = 3, col = "darkblue")
text(100, 1.03, "pCI filter", col = "darkblue", pos = 4)

# add quantile lines of effect sizes passing p-value filter for comparison:
abline(h = max(nonsig$effectsize), lty = 3, col = "grey60")
abline(h = min(nonsig$effectsize), lty = 3, col = "grey60")
abline(h = median(nonsig$effectsize), lty = 1, col = "grey60")
abline(h = quantile(nonsig$effectsize, 0.25), lty = 2, col = "grey60")
abline(h = quantile(nonsig$effectsize, 0.75), lty = 2, col = "grey60")


## plot3: pMDE 
plot(summ.pMDE$max ~ summ.pMDE$threshold, type ="n", ylim = c(0,1.03), xlim = rev(c(30,100)),
     las = 2, xlab = "", ylab = "", las = 1, bty ="l",
     yaxt = "n", xaxt = "n")
axis(side = 1)
mtext("Threshold level", side = 1, line = 2.5)
ycor = c(smooth.spline(summ.pMDE$max ~ summ.pMDE$threshold)$y, rev(smooth.spline(summ.pMDE$min ~ summ.pMDE$threshold)$y))
polygon(xcor, ycor, border =NA, col = adjustcolor("darkseagreen2", alpha.f = 0.2))
ycor = c(smooth.spline(summ.pMDE$q75 ~ summ.pMDE$threshold)$y, rev(smooth.spline(summ.pMDE$q25 ~ summ.pMDE$threshold)$y))
polygon(xcor, ycor, border =NA, col = "darkseagreen2")
lines(smooth.spline(summ.pMDE$max ~ summ.pMDE$threshold), lty = 3, col = "darkgreen")
lines(smooth.spline(summ.pMDE$q75 ~ summ.pMDE$threshold), lty = 2, col = "darkgreen")
lines(smooth.spline(summ.pMDE$median ~ summ.pMDE$threshold), lty = 1, col = "darkgreen")
lines(smooth.spline(summ.pMDE$q25 ~ summ.pMDE$threshold), lty = 2, col = "darkgreen")
lines(smooth.spline(summ.pMDE$min ~ summ.pMDE$threshold), lty = 3, col = "darkgreen")
text(100, 1.03, "pMDE filter", col = "darkgreen", pos = 4)

# add quantile lines of effect sizes passing p-value filter for comparison:
abline(h = max(nonsig$effectsize), lty = 3, col = "grey60")
abline(h = min(nonsig$effectsize), lty = 3, col = "grey60")
abline(h = median(nonsig$effectsize), lty = 1, col = "grey60")
abline(h = quantile(nonsig$effectsize, 0.25), lty = 2, col = "grey60")
abline(h = quantile(nonsig$effectsize, 0.75), lty = 2, col = "grey60")

# plot4: pMDD 
plot(summ.pMDD$max ~ summ.pMDD$threshold, type ="n", ylim = c(0,1.03), xlim = rev(c(30,100)),
     las=2, xlab = "", 
     ylab = "", las = 1, bty ="l", yaxt = "n", xaxt = "n")
axis(side = 1)
ycor = c(smooth.spline(summ.pMDD$max ~ summ.pMDD$threshold)$y, rev(smooth.spline(summ.pMDD$min ~ summ.pMDD$threshold)$y))
polygon(xcor, ycor, border =NA, col = adjustcolor( "mistyrose2", alpha.f = 0.2))
ycor = c(smooth.spline(summ.pMDD$q75 ~ summ.pMDD$threshold)$y, rev(smooth.spline(summ.pMDD$q25 ~ summ.pMDD$threshold)$y))
polygon(xcor, ycor, border =NA, col = "mistyrose2")
lines(smooth.spline(summ.pMDD$max ~ summ.pMDD$threshold), lty = 3, col = "darkred")
lines(smooth.spline(summ.pMDD$q75 ~ summ.pMDD$threshold), lty = 2, col = "darkred")
lines(smooth.spline(summ.pMDD$median ~ summ.pMDD$threshold), lty = 1, col = "darkred")
lines(smooth.spline(summ.pMDD$q25 ~ summ.pMDD$threshold), lty = 2, col = "darkred")
lines(smooth.spline(summ.pMDD$min ~ summ.pMDD$threshold), lty = 3, col = "darkred")
text(100, 1.03, "pMDD filter", col = "darkred", pos = 4)

# add quantile lines of effect sizes passing p-value filter for comparison:
abline(h = max(nonsig$effectsize), lty = 3, col = "grey60")
abline(h = min(nonsig$effectsize), lty = 3, col = "grey60")
abline(h = median(nonsig$effectsize), lty = 1, col = "grey60")
abline(h = quantile(nonsig$effectsize, 0.25), lty = 2, col = "grey60")
abline(h = quantile(nonsig$effectsize, 0.75), lty = 2, col = "grey60")