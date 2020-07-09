library(shiny)
library(shinyWidgets) #for setBackgroundColor

# UI: ------------------------
ui <- fluidPage(
  tags$style(HTML("
        .tabbable > .nav > li[class=active] > a {background-color:    #3498db;  color:black}")),
  setBackgroundColor(color = "#eaf2f8"),
   # Application title
   titlePanel(""),
   # Tab1 - Reference===================
  tabsetPanel(
      tabPanel("The MDD - a critical review", fluid = TRUE,
        mainPanel(
          HTML(paste(tags$h6("This Shiny application accompanies the article: "), "",
                     paste("Mair, MM, Kattwinkel, M, Jakoby, O and Hartig, F (2020) The MDD concept for establishing trust in non-significant results - a critical review.", tags$i("Environ. Toxicol. Chem."),"X:xx-xx", "DOI: XX"),
                     "","", tags$i("Abstract"),
                     tags$h6("Current regulatory guidelines for pesticide risk assessment recommend that non-significant results should be complemented by the minimum detectable difference (MDD), a statistical indicator that is used to decide if the experiment could have detected biologically relevant effects. Here, we review the statistical theory of the MDD and perform simulations to understand its properties and error rates. Most importantly, we compare the skill of the MDD in distinguishing between true and false negatives (i.e., type II errors) with two alternatives: the minimum detectable effect (MDE), an indicator based on a post-hoc power analysis common in medical studies; and confidence intervals (CIs). Our results demonstrate that MDD and MDE only differ in that the power of the MDD depends on the sample size. Moreover, while both MDD and MDE have some skill in distinguishing between false negatives and true absence of an effect, they do not perform as well as using CI upper bounds to establish trust in a non-significant result. The reason is that, unlike the CI, neither MDD nor MDE consider the estimated effect size in their calculation. We also show that MDD and MDE are no better than CIs in identifying larger effects among the false negatives. We conclude that, while MDDs are useful, CIs are preferable for deciding whether to treat a non-significant test result as a true negative, or for determining an upper bound for an unknown true effect."),
                     sep = "<br/>"))
        )
      ),
   # Tab2 - pareto================
      tabPanel("I. False trust/ False mistrust rates", fluid = TRUE,
        sidebarLayout(
          sidebarPanel(
            sliderInput("samples2", "Sample size", min = 2, max = 20, value = c(3, 10)),
            sliderInput("effectsize2", "Real effect size", min = 0, max = 1, value = 0.5),
            sliderInput("theta2", "Theta", min = 1, max = 50, value = c(1, 50)),
            sliderInput("abund2", "Abundance", min = 10, max = 200, value =c(10, 100)),
            sliderInput("alpha2", "Alpha-level (type I error rate)", min = 0.01, max = 0.2, value = 0.05),
            actionButton(inputId = "generate2", label = "Simulate Experiments"),
            checkboxInput("AddCI75", "Display results for 75% pCI", FALSE)
          ),
          
          # Main panel output 
          mainPanel(
            textOutput("N"),
            dataTableOutput("ns"),
            dataTableOutput("trust"),
            plotOutput("paretoPlot"),
            "Figure 5. The translation of the continuous indicators pCI, pMDE and pMDD into a dichotomous decision of trust/ mistrust requires choosing a threshold level for the indicator. We can then examine the error rates of this decision (false trust/ false mistrust rates) for the three methods and different thresholds. Rates at which pCI (blue, dashed), pMDE (green, dotted) and pMDD (red, solid; bold italics) erroneously suggest to trust (false trust rate) or mistrust (false mistrust rate) a non-significant result for varying threshold levels (numbers along lines). Rates are calculated from 1000 simulated experiments without effect (zero difference between means) and 1000 experiments with moderate effect (50% loss in mean abundance). Sample sizes (3 to 10), control means (10 to 100) and variance (i.e., theta values: 1 to 50) vary randomly among experiments. Values nearer to the bottom left are pareto-superior (i.e., more optimal) to values nearer to the top right. Numbers along lines indicate applied threshold levels at these points. Tick box  to display results for pCIs with 75% confidence level (then shown in light blue, dashdotted)."
          )
        )
      ),

   # Tab3 - effect size filter====================
        tabPanel("II. Sensitivity to real effect size", fluid = TRUE,
                 # Sidebar with range slider for sample size, theta, effect size and abundance 
                 # + simulate experiments button
           sidebarLayout(
             sidebarPanel(
               sliderInput("samples", "Sample size", min = 2, max = 20, value = c(3, 10)),
               sliderInput("effectsize", "Real effect size", min = 0, max = 1, value = c(0, 1)),
               sliderInput("theta", "Theta", min = 1, max = 50, value = c(1, 50)),
               sliderInput("abund", "Abundance", min = 10, max = 200, value =c(10, 100)),
               sliderInput("alpha", "Alpha-level (type I error rate)", min = 0.01, max = 0.2, value = 0.05),
               actionButton(inputId = "generate1", label = "Simulate Experiments")
             ),
             
             # Main panel output 
             mainPanel(
               plotOutput("corrPlot"),
               "Figure 6 D-F. Correlation between pCI (D), pMDE (E) and pMDD (F) and real effect size for simulated experiments with known treatment effects ranging between 0 and 1 (0 to 100% reduction in species abundance). We show the values only for experiments that yielded non-significant results (p > 0.05). Each dot represents one simulated experiment (i.e., one test). Solid line (red): regression line; dashed line (grey): pCI/ pMDE/ pMDD equals real effect, grey squares: experiments in which the indicator was lower than the real effect. pCI: upper bound of the 95% confidence interval related to control mean, pMDE: minimum detectable effect with 80% power related to control mean, pMDD: minimum detectable difference related to control mean.",
               plotOutput("filterPlot"),
               "Figure 7. Effect sizes in experiments passing both the significance filter p > 0.05 (boxplot and grey lines in all three plots) and the secondary pCI, pMDE or pMDD filter for different thresholds (horizontal axis, ranging from 100 to 30) and different real effect sizes (vertical axis). Lines represent median (solid line), 25% and 75% quartiles (dashed lines) and minimum and maximum real effect size (dotted lines). pCI: proportional upper bound of the 95% confidence interval, pMDE: proportional minimum detectable effect with 80% power, pMDD: proportional minimum detectable difference."
             )
           )
        )
      )
  )



#server-----
server <- function(input, output) {


  

# Set defaults - Create dataframes with default values and analyse using defaults ------------------------------------
source("./MDDfunction.R", local = T)


# Tab2 - Defaults - pareto ================
size = 2000
reffectsize2 = c(rep(0, size/2), rep(0.5, size/2))
rtheta2 = runif(size, max = 50, min = 1)
rsamples2 = sample(3:10, size, replace = T)
rabund2 = sample(10:100, size, replace=T)
alpha2 = 0.05

experiments2 = data.frame(
  effect2 = c(rep("no",size/2),rep("yes",size/2)),
  effectsize2 = reffectsize2,
  theta2 = rtheta2,
  sampleSize2 = rsamples2,
  abund2 = rabund2
)
experiments2$variance2 = experiments2$theta2 * experiments2$abund2

out2 <- data.frame(
  p = rep(NA,size),
  pMDD = rep(NA,size),
  pMDE = rep(NA,size),
  pCI = rep(NA,size),
  pCI75 = rep(NA, size)
)

for (i in 1:size)
{
  try({
    N <- experiments2$sampleSize2[i]
    theta <- experiments2$theta2[i]
    abund <- experiments2$abund2[i]
    effectsize <- experiments2$effectsize2[i]
    #simulate experiments:
    control <- rnbinom(n = N, mu = abund, size = abund/(theta-1))
    tmean <- abund * (1 - effectsize)
    treatment <- rnbinom(n = N, mu = tmean, size = tmean/(theta-1))
    #log-transform data prior to analysis:
    transc <- log((2*control)+1)
    transt <- log((2*treatment)+1)
    #tests and calculations on transformed data:
    test <- t.test(transc, transt, alternative = "greater", var.equal = T, conf.level = 1 - alpha2)#for p-value
    upperCI <- mean(transc)-mean(transt) + 
      qt(0.05/1, df = 2*N-2, lower.tail = FALSE) * sqrt(var(c(transc-mean(transc),transt-mean(transt)))) * sqrt(2/N)
    upperCI75 <- mean(transc)-mean(transt) + 
      qt(0.25/1, df = 2*N-2, lower.tail = FALSE) * sqrt(var(c(transc-mean(transc), transt-mean(transt)))) * sqrt(2/N)
    mdd.ln <- MDD(N1 = N, N2 = N, variance1 = var(c(transc-mean(transc),transt-mean(transt))),
                  alpha = alpha2, two.sided = F, var.equal = T)
    postpower <- power.t.test(n = N, delta = NULL, sd = sd(c(transc-mean(transc),transt-mean(transt))), 
                              sig.level = alpha2, alternative="one.sided", type = "two.sample", 
                              power = 0.8)
    #backtransformation:
    mdd.abu <- (exp(mean(transc))-exp(mean(transc) - mdd.ln$mdd))/2
    mde.abu <- (exp(mean(transc))-exp(mean(transc) - postpower$delta))/2
    upperCI.abu <- (exp(mean(transc))-exp(mean(transc) - upperCI))/2
    upperCI75.abu <- (exp(mean(transc))-exp(mean(transc) - upperCI75))/2
    
    #relation to control mean:
    pMDD <- 100 * mdd.abu / ((exp(mean(transc))-1)/2)
    pMDE <- 100 * mde.abu / ((exp(mean(transc))-1)/2)
    pCI <- 100 * upperCI.abu / ((exp(mean(transc))-1)/2)
    pCI75 <- 100 * upperCI75.abu / ((exp(mean(transc))-1)/2)
    
    
    out2[i,1] <- test$p.value
    out2[i,2] <- pMDD
    out2[i,3] <- pMDE
    out2[i,4] <- pCI
    out2[i,5] <- pCI75
  }, silent = TRUE)
}

all2 <- cbind(experiments2,out2)[complete.cases(out2),]
nonsig2 <- all2[all2$p >= alpha2,]


# Tab3 - Defaults - effect size filter =================
size = 1000
rsamples = sample(3:10, size = size, replace = T)
rtheta = runif(size, max = 50,min = 1)
rabund = sample(10:100, size, replace = T)
reffectsize = runif(size)
alpha = 0.05

 
experiments = data.frame(
  effectsize = reffectsize,
  theta = rtheta,
  sampleSize = rsamples,
  abund = rabund
)
experiments$variance = experiments$theta * experiments$abund

out = data.frame(
  p = rep(NA,size),
  pMDD = rep(NA,size),
  pMDE = rep(NA,size),
  pCI = rep(NA,size)
)

for (i in 1:size)
{ 
  try({
    N <- experiments$sampleSize[i]
    theta <- experiments$theta[i]
    abund <- experiments$abund[i]
    effectsize <- experiments$effectsize[i]
    control <- rnbinom(n = N, mu = abund, size = abund/(theta-1))
    tmean <- abund * (1 - effectsize)
    treatment <- rnbinom(n = N, mu = tmean, size = tmean/(theta-1))
    transc <- log((2*control)+1)
    transt <- log((2*treatment)+1)
    #tests and calculations on transformed data:
    test <- t.test(transc, transt, alternative = "greater", var.equal = T, conf.level = 1 - alpha) #for p-values
    mdd.ln <- MDD(N1 = N, N2 = N, variance1 = (var(transc)+var(transt))/2,
                  alpha = alpha, two.sided = F, var.equal = T)
    postpower <- power.t.test(n = N, delta = NULL, sd = sd(c(transc-mean(transc), transt-mean(transt))), 
                              sig.level = alpha, alternative = "one.sided", 
                              type = "two.sample", power = 0.8)
    upperCI <- mean(transc)-mean(transt) + 
      qt(0.05/1, df = 2*N-2, lower.tail = FALSE) * sqrt(var(c(transc-mean(transc), transt-mean(transt)))) * sqrt(2/N)
    #backtransformation
    mdd.abu <- (exp(mean(transc))-exp(mean(transc) - mdd.ln$mdd))/2
    mde.abu <- (exp(mean(transc))-exp(mean(transc) - postpower$delta))/2
    upperCI.abu <- (exp(mean(transc))-exp(mean(transc) - upperCI))/2
    #relation to control mean
    pMDD <- 100 * mdd.abu / ((exp(mean(transc))-1)/2)
    pMDE <- 100 * mde.abu / ((exp(mean(transc))-1)/2)
    pCI <- 100 * upperCI.abu / ((exp(mean(transc))-1)/2)
    out[i,1] <- test$p.value
    out[i,2] <- pMDD
    out[i,3] <- pMDE
    out[i,4] <- pCI
  }, silent = TRUE)
}

all <- cbind(experiments,out)[complete.cases(out),]
all <- all[!is.infinite(rowSums(all)) & !is.na(rowSums(all)),]
nonsig <- all[all$p >= alpha,]


# Define reactives (for both tabs)----------------------------------------------
# define objects as reactive and calculations to be done on simulate experiments:

rv <- reactiveValues(experiments = experiments, out = out, all = all, 
                     nonsig = nonsig, reffectsize = reffectsize, rtheta = rtheta,
                     rsamples = rsamples, rabund = rabund, alpha = alpha,
                     experiments2 = experiments2, out2 = out2, all2 = all2,
                     nonsig2 = nonsig2, reffectsize2 = reffectsize2, rtheta2 = rtheta2,
                     rsamples2 = rsamples2, rabund2 = rabund2, alpha2 = alpha2)




# Tab2: observeEvent: generate2 - pareto --------------
observeEvent(input$generate2, {
  size = 2000
  rv$alpha2 = input$alpha2
  rv$reffectsize2 = c(rep(0, size/2), rep(input$effectsize2, size/2))
  ifelse(input$theta2[1] == input$theta2[2],
         rv$rtheta2 <- rep(input$theta2[1], size),
         rv$rtheta2 <- runif(size, max = input$theta2[2], min = input$theta2[1]))
  ifelse(input$samples2[1] == input$samples2[2],
         rv$rsamples2 <- rep(input$samples2[1], size),
         rv$rsamples2 <- sample(input$samples2[1]:input$samples2[2], size = size, replace = T)) 
  ifelse(input$abund2[1] == input$abund2[2],
         rv$rabund2 <- rep(input$abund2[1], size),
         rv$rabund2 <- sample(input$abund2[1]:input$abund2[2], size = size, replace = T))
  rv$experiments2 = data.frame(
    effect2 = c(rep("no", size/2), rep("yes", size/2)),
    effectsize2 = rv$reffectsize2,
    theta2 = rv$rtheta2,
    sampleSize2 = rv$rsamples2,
    abund2 = rv$rabund2
  )
  rv$experiments2$variance2 = rv$experiments2$theta2 * rv$experiments2$abund2
  
  rv$out2 <- data.frame(
    p = rep(NA,size),
    pMDD = rep(NA,size),
    pMDE = rep(NA,size),
    pCI = rep(NA,size),
    pCI75 = rep(NA,size)
  )
  
  for (i in 1:size)
  {
    try({
      N <- rv$experiments2$sampleSize2[i]
      theta <- rv$experiments2$theta2[i]
      abund <- rv$experiments2$abund2[i]
      effectsize <- rv$experiments2$effectsize2[i]
      #simulate experiments:
      control <- rnbinom(n = N, mu = abund, size = abund/(theta-1))
      tmean <- abund * (1 - effectsize)
      treatment <- rnbinom(n = N, mu = tmean, size = tmean/(theta-1))
      #log-transform data prior to analysis:
      transc <- log((2*control)+1)
      transt <- log((2*treatment)+1)
      #tests and calculations on transformed data:
      test <- t.test(transc, transt, alternative = "greater", var.equal = T, conf.level = 1 - rv$alpha2)#for p-value
      upperCI <- mean(transc)-mean(transt) + 
        qt(0.05/1, df = 2*N-2, lower.tail = FALSE) * sqrt(var(c(transc-mean(transc),transt-mean(transt)))) * sqrt(2/N)
      upperCI75 <- mean(transc)-mean(transt) + 
        qt(0.25/1, df = 2*N-2, lower.tail = FALSE) * sqrt(var(c(transc-mean(transc),transt-mean(transt)))) * sqrt(2/N)
      mdd.ln <- MDD(N1 = N, N2 = N, variance1 = var(c(transc-mean(transc),transt-mean(transt))),
                    alpha = rv$alpha2, two.sided = F, var.equal = T)
      postpower <- power.t.test(n = N, delta = NULL, sd = sd(c(transc-mean(transc),transt-mean(transt))), 
                                sig.level = rv$alpha2, alternative="one.sided", type = "two.sample", 
                                power = 0.8)
      #backtransformation:
      mdd.abu <- (exp(mean(transc))-exp(mean(transc) - mdd.ln$mdd))/2
      mde.abu <- (exp(mean(transc))-exp(mean(transc) - postpower$delta))/2
      upperCI.abu <- (exp(mean(transc))-exp(mean(transc) - upperCI))/2
      upperCI75.abu <- (exp(mean(transc))-exp(mean(transc) - upperCI75))/2
      
      #relation to control mean:
      pMDD <- 100 * mdd.abu / ((exp(mean(transc))-1)/2)
      pMDE <- 100 * mde.abu / ((exp(mean(transc))-1)/2)
      pCI <- 100 * upperCI.abu / ((exp(mean(transc))-1)/2)
      pCI75 <- 100 * upperCI75.abu / ((exp(mean(transc))-1)/2)
      
      rv$out2[i,1] <- test$p.value
      rv$out2[i,2] <- pMDD
      rv$out2[i,3] <- pMDE
      rv$out2[i,4] <- pCI
      rv$out2[i,5] <- pCI75
    }, silent = TRUE)
  }
  
  rv$all2 <- cbind(rv$experiments2,rv$out2)[complete.cases(rv$out2),]
  rv$nonsig2 <- rv$all2[rv$all2$p >= rv$alpha2,]
})
# Tab3: observeEvent: generate1 - effect size filter --------------------
observeEvent(input$generate1, {
  rv$alpha = input$alpha
  ifelse(input$effectsize[1] == input$effectsize[2],
         rv$reffectsize <- rep(input$effectsize[1], size),
         rv$reffectsize <- runif(size, min = input$effectsize[1], max = input$effectsize[2]))
  ifelse(input$theta[1] == input$theta[2],
         rv$rtheta <- rep(input$theta[1], size),
         rv$rtheta <- runif(size, max = input$theta[2], min = input$theta[1]))
  ifelse(input$samples[1] == input$samples[2],
         rv$rsamples <- rep(input$samples[1], size),
         rv$rsamples <- sample(input$samples[1]:input$samples[2], size = size, replace = T)) 
  ifelse(input$abund[1] == input$abund[2],
         rv$rabund <- rep(input$abund[1], size),
         rv$rabund <- sample(input$abund[1]:input$abund[2], size = size, replace = T))
  rv$experiments <- data.frame(
    effectsize = rv$reffectsize,
    theta = rv$rtheta,
    sampleSize = rv$rsamples, 
    abund = rv$rabund
  )
  rv$experiments$variance = rv$experiments$theta * rv$experiments$abund
  
  rv$out = data.frame(
    p = rep(NA,size),
    pMDD = rep(NA,size),
    pMDE = rep(NA,size),
    pCI = rep(NA,size) 
  )
  
  for (i in 1:size)
  { 
    try({
      N <- rv$experiments$sampleSize[i]
      theta <- rv$experiments$theta[i]
      abund <- rv$experiments$abund[i]
      effectsize <- rv$experiments$effectsize[i]
      control <- rnbinom(n = N, mu = abund, size = abund/(theta-1))
      tmean <- abund * (1 - effectsize)
      treatment <- rnbinom(n = N, mu = tmean, size = tmean/(theta-1))
      transc <- log((2*control)+1)
      transt <- log((2*treatment)+1)
      test <- t.test(transc, transt, alternative = "greater", var.equal = T, 
                     conf.level = 1 - rv$alpha)
      upperCI <- mean(transc)-mean(transt) + 
        qt(0.05/1, df = 2*N-2, lower.tail = FALSE) * sqrt(var(c(transc-mean(transc),transt-mean(transt)))) * sqrt(2/N)
      mdd.ln <- MDD(N1 = N, N2 = N, variance1 = (var(transc)+var(transt))/2,
                    alpha = rv$alpha, two.sided = F, var.equal = T)
      posteffect <- power.t.test(n = N, delta = NULL, sd = sd(c(transc-mean(transc),transt-mean(transt))), 
                                 sig.level = rv$alpha, alternative = "one.sided", type = "two.sample", 
                                 power = 0.8)
      #backtransformation
      MDD.abu <- (exp(mean(transc))-exp(mean(transc) - mdd.ln$mdd))/2
      MDE.abu <- (exp(mean(transc))-exp(mean(transc) - posteffect$delta))/2
      CI.abu <- (exp(mean(transc))-exp(mean(transc) - upperCI))/2
      #relation to control mean
      pMDD <- 100 * MDD.abu / ((exp(mean(transc))-1)/2)
      pMDE <- 100 * MDE.abu / ((exp(mean(transc))-1)/2)
      pCI <- 100 * CI.abu / ((exp(mean(transc))-1)/2)
      rv$out[i,1] <- test$p.value
      rv$out[i,2] <- pMDD
      rv$out[i,3] <- pMDE
      rv$out[i,4] <- pCI
    }, silent = TRUE)
  }
  
  rv$all <- cbind(rv$experiments, rv$out)[complete.cases(rv$out),]
  rv$all <- rv$all[!is.infinite(rowSums(rv$all)) & !is.na(rowSums(rv$all)),]
  rv$nonsig <- rv$all[rv$all$p >= rv$alpha,]
})




# Define output: PLOTS ---------------------------------------------------
## Tab 2 - plot1: pareto-plot----------

output$paretoPlot <- renderPlot({
  thresholds = seq(from = 30, to = 100, by = 10)
  
  ErrorRates <- data.frame(
    threshold = thresholds,
    mdd.ftrust = rep(NA,length(thresholds)),
    mdd.fmist = rep(NA,length(thresholds)),
    MDE.ftrust = rep(NA,length(thresholds)),
    MDE.fmist = rep(NA,length(thresholds)),
    CI.ftrust = rep(NA,length(thresholds)),
    CI.fmist = rep(NA,length(thresholds)),
    CI75.ftrust = rep(NA,length(thresholds)),
    CI75.fmist = rep(NA,length(thresholds))
  )
  
  for (i in 1:length(thresholds))
  {
    try({
      mdd.ftrust <- length(which(rv$nonsig2$pMDD <= ErrorRates$threshold[i] & 
                                   rv$nonsig2$effect2 == "yes"))/length(which(rv$nonsig2$effect2 == "yes"))
      mdd.fmist <- length(which(rv$nonsig2$pMDD > ErrorRates$threshold[i] & 
                                  rv$nonsig2$effect2 == "no"))/length(which(rv$nonsig2$effect2 == "no"))
      MDE.ftrust <- length(which(rv$nonsig2$pMDE <= ErrorRates$threshold[i] & 
                                    rv$nonsig2$effect2 == "yes"))/length(which(rv$nonsig2$effect2 == "yes"))
      MDE.fmist <- length(which(rv$nonsig2$pMDE > ErrorRates$threshold[i] & 
                                   rv$nonsig2$effect2 == "no"))/length(which(rv$nonsig2$effect2 == "no")) 
      CI.ftrust <- length(which(rv$nonsig2$pCI <= ErrorRates$threshold[i] & 
                                  rv$nonsig2$effect2 == "yes"))/length(which(rv$nonsig2$effect2 == "yes"))
      CI.fmist <- length(which(rv$nonsig2$pCI > ErrorRates$threshold[i] & 
                                 rv$nonsig2$effect2 == "no"))/length(which(rv$nonsig2$effect2 == "no")) 
      CI75.ftrust <- length(which(rv$nonsig2$pCI75 <= ErrorRates$threshold[i] & 
                                  rv$nonsig2$effect2 == "yes"))/length(which(rv$nonsig2$effect2 == "yes"))
      CI75.fmist <- length(which(rv$nonsig2$pCI75 > ErrorRates$threshold[i] & 
                                 rv$nonsig2$effect2 == "no"))/length(which(rv$nonsig2$effect2 == "no")) 
      ErrorRates[i,2] <- mdd.ftrust
      ErrorRates[i,3] <- mdd.fmist
      ErrorRates[i,4] <- MDE.ftrust
      ErrorRates[i,5] <- MDE.fmist
      ErrorRates[i,6] <- CI.ftrust
      ErrorRates[i,7] <- CI.fmist
      ErrorRates[i,8] <- CI75.ftrust
      ErrorRates[i,9] <- CI75.fmist
    }, silent = T)
  }
  
  op <- par(mar = c(5,6,1,1), mfrow=c(1,1))
  #MDD
  plot(ErrorRates$mdd.ftrust, ErrorRates$mdd.fmist, xlim = c(-0.1, 1), ylim = c(-0.05,1), 
       type = "l", xlab = "False trust rate", ylab = "", bty = "l", las = 1, 
       cex.lab = 1.5, cex.axis = 1.5, col = "darkred", lwd = 2)
  mtext("False mistrust rate", side = 2, line = 3.5, cex = 1.5)
  text(ErrorRates$mdd.ftrust, ErrorRates$mdd.fmist, labels = ErrorRates$threshold, 
       cex= 1.5, col = "darkred", font = 4, pos = c(4, 4, 4, rep(3, 5)))
  points(ErrorRates$mdd.ftrust, ErrorRates$mdd.fmist, pch = 20, col = "darkred")
  #MDE
  lines(ErrorRates$MDE.ftrust, ErrorRates$MDE.fmist, col = "darkgreen", lty = 3, lwd = 2)
  text(ErrorRates$MDE.ftrust, ErrorRates$MDE.fmist, labels=ErrorRates$threshold, 
       cex= 1.5, col = "darkgreen", pos = c(4, 2, 4, 4, 3, 3, 3, 3))
  points(ErrorRates$MDE.ftrust, ErrorRates$MDE.fmist, pch = 20, col = "darkgreen")
  #CI
  lines(ErrorRates$CI.ftrust, ErrorRates$CI.fmist, col = "darkblue", lty = 2, lwd = 2)
  text(ErrorRates$CI.ftrust, ErrorRates$CI.fmist, labels=ErrorRates$threshold, 
       cex= 1.5, col = "darkblue", pos = c(rep(2, 4), rep(1, 4)))
  points(ErrorRates$CI.ftrust, ErrorRates$CI.fmist, pch = 20, col = "darkblue")
  
 

  # add legend
  arrows(0.4 , 0.8, 0.5, 0.8, angle = 0, col = "darkblue", lty = 2, lwd = 2, length = 0)
  arrows(0.4 , 0.7, 0.5, 0.7, angle = 0, col = "darkgreen", lty = 3, lwd = 2, length = 0)
  arrows(0.4 , 0.6, 0.5, 0.6, angle = 0, col = "darkred", lty = 1, lwd = 2, length = 0)
  text(0.52 , 0.8, "pCI (95% confidence)", col = "darkblue", pos=4, offset= 0, cex = 1.5)
  text(0.52 , 0.7, "80% pMDE", col = "darkgreen", pos=4, offset= 0, cex = 1.5)
  text(0.52 , 0.6, "pMDD", col = "darkred", pos=4, offset= 0, cex = 1.5)
  
  if(input$AddCI75) {
    #CI75
    lines(ErrorRates$CI75.ftrust, ErrorRates$CI75.fmist, col = "steelblue", lty = 4, lwd = 2)
    text(ErrorRates$CI75.ftrust, ErrorRates$CI75.fmist, labels=ErrorRates$threshold, 
         cex= 1.5, col = "steelblue", pos = c(rep(2, 4), rep(1, 4)))
    points(ErrorRates$CI75.ftrust, ErrorRates$CI75.fmist, pch = 20, col = "steelblue")
    text(0.52 , 0.9, "pCI (75% confidence)", col = "steelblue", pos=4, offset= 0, cex = 1.5)
    arrows(0.4 , 0.9, 0.5, 0.9, angle = 0, col = "steelblue", lty = 4, lwd = 2, length = 0)
  }
  
  mtext(paste0("n = ", nrow(rv$nonsig2), " experiments with n.s. outcome"), 
        line = 3, side = 1, at = 0)
  
  if(nrow(rv$nonsig2 [rv$nonsig2$effect2 == "yes",]) == 0) 
    text(0.5, 1, 
         "Plotting not possible - all simulated experiments with real effects had p < alpha.")
  par(op)
  
})




## Tab 3 - plot1: correlation with real effect (nonsignificant outcomes only) ----------------
output$corrPlot <- renderPlot({
    palette(c("grey60", "black", "red"))
    op <- par(cex.lab = 1.5, mfrow = c(1,3), pch = 1, bty = "l", oma = c(1,1,3,1), mar = c(4,5,1,1))
    ylim = max(max(rv$nonsig$pCI), max(rv$nonsig$pMDD), max(rv$nonsig$pMDE)) + 5
    # pCI:
    dotcol <- as.numeric(rv$nonsig$effectsize * 100 <= rv$nonsig$pCI)
    plot(rv$nonsig$pCI ~ rv$nonsig$effectsize, ylim = c(0, ylim), 
         xlim = c(min(rv$experiments$effectsize) - 0.05, max(rv$experiments$effectsize) + 0.05),
         xlab = "", col = dotcol + 1, ylab = "pCI", pch = dotcol, 
         xaxt = "n", yaxt = "n")
    axis(2, at = seq(0, 300, by = 50))
    axis(1, at = seq(0, 1, by = 0.1))
    abline(0, 100, col = "grey40", lwd = 2, lty = 5)
    try({
      fit1 <- lm(rv$nonsig$pCI ~ rv$nonsig$effectsize)
      abline(fit1, col = "darkred", lwd = 2, lty = "solid")
    }, silent = T)
    text(0.1, ylim, "D", cex = 1.5)
    # pMDE:
    dotcol <- as.numeric(rv$nonsig$effectsize * 100 <= rv$nonsig$pMDE)
    plot(rv$nonsig$pMDE ~ rv$nonsig$effectsize, ylim = c(0, ylim),  
         xlim = c(min(rv$experiments$effectsize) - 0.05, max(rv$experiments$effectsize) + 0.05),
         xlab = "Real effect size", col = dotcol + 1, ylab = "pMDE", pch = dotcol, 
         xaxt = "n", yaxt = "n")
    axis(2, at = seq(0, 300, by = 50))
    axis(1, at = seq(0, 1, by = 0.1))
    abline(0, 100, col = "grey40", lwd = 2, lty = 5)
    try({
      fit2 <- lm(rv$nonsig$pMDE ~ rv$nonsig$effectsize)
      abline(fit2, col = "darkred", lwd = 2, lty = "solid")
    }, silent = T)
    text(0.1, ylim, "E", cex = 1.5)
    # pMDD:
    dotcol <- as.numeric(rv$nonsig$effectsize * 100 <= rv$nonsig$pMDD)
    plot(rv$nonsig$pMDD ~ rv$nonsig$effectsize, ylim = c(0, ylim),  
         xlim = c(min(rv$experiments$effectsize) - 0.05, max(rv$experiments$effectsize) + 0.05),
         xlab = "", col = dotcol + 1, ylab = "pMDD", pch = dotcol, 
         xaxt = "n", yaxt = "n")
    axis(2, at = seq(0, 300, by = 50))
    axis(1, at = seq(0, 1, by = 0.1))
    abline(0, 100, col = "grey40", lwd = 2, lty = 5)
    try({
      fit3 <- lm(rv$nonsig$pMDD ~ rv$nonsig$effectsize)
      abline(fit3, col = "darkred", lwd = 2, lty = "solid")
    }, silent = T)
    text(0.1, ylim, "F", cex = 1.5)
    par(op)
})


## Tab 3 - plot2: secondary effect size filters----------------

output$filterPlot <- renderPlot({
  outeffect <- data.frame(
    indicator = NA,
    threshold = NA,
    effectsize = NA
  )
  ind <- list("pCI", "pMDE", "pMDD")
  
  for (i in ind){
    for (j in 30:100){
      sub <- rv$nonsig[rv$nonsig[, i] <= j, ]
      temp <- data.frame(
        indicator = rep(i, nrow(sub)),
        threshold = rep(j, nrow(sub)),
        effectsize = sub$effectsize
      )
      outeffect <- rbind(outeffect, temp)
    }
  }
  outeffect <- na.omit(outeffect)
  
  pCIpass <- outeffect[outeffect$indicator == "pCI", ] 
  pMDEpass <- outeffect[outeffect$indicator == "pMDE", ] 
  pMDDpass <- outeffect[outeffect$indicator == "pMDD", ] 
  
  library(dplyr)
  summ.pCI <- pCIpass %>%
    group_by(threshold) %>%
    summarise(min = min(effectsize), q25 = quantile(effectsize, 0.25), median = median(effectsize), 
              q75 = quantile(effectsize, 0.75), max = max(effectsize), N = n(), 
              mean = mean(effectsize), sd = sd(effectsize))
  summ.pCI <- summ.pCI[complete.cases(summ.pCI),]
  summ.pMDE <- pMDEpass %>%
    group_by(threshold) %>%
    summarise(min = min(effectsize), q25 = quantile(effectsize, 0.25), median = median(effectsize), 
              q75 = quantile(effectsize, 0.75), max = max(effectsize), N = n(),
              mean = mean(effectsize), sd = sd(effectsize))
  summ.pMDE <- summ.pMDE[complete.cases(summ.pMDE),]
  summ.pMDD <- pMDDpass %>%
    group_by(threshold) %>%
    summarise(min = min(effectsize), q25 = quantile(effectsize, 0.25), median = median(effectsize), 
              q75 = quantile(effectsize, 0.75), max = max(effectsize), N = n(),
              mean = mean(effectsize), sd = sd(effectsize)) 
  summ.pMDD <- summ.pMDD[complete.cases(summ.pMDD),]
  #
  ## plot max, quartiles, min
  layout(matrix(c(1,2,3), 1, 3, byrow = TRUE), width = c(1,1,1))
  ## plot1: pCI (9)
  op <- par(mar = c(3,0,1,1), oma = c(1,8,1,1), cex = 0.8)
  plot(summ.pCI$max ~ summ.pCI$threshold, type = "n", 
       ylim = c(min(rv$experiments$effectsize) - 0.05, max(rv$experiments$effectsize) + 0.05), 
       xlim = rev(c(30,100)),
       las = 2, xlab = "", ylab = "", las = 1, bty ="l", yaxt = "n", xaxt = "n")
  axis(side = 1)
  axis(side = 2, las = 2, labels = T, at = seq(0, 1, by =0.1), line = 4, outer = T)
  mtext("Real effect size", side = 2, line = 7, outer = T)
  xcor = c(summ.pCI$threshold, rev(summ.pCI$threshold))
  try({
    ycor = c(smooth.spline(summ.pCI$max ~ summ.pCI$threshold)$y, 
             rev(smooth.spline(summ.pCI$min ~ summ.pCI$threshold)$y))
    polygon(xcor, ycor, border = NA, col = adjustcolor("slategray1", alpha.f = 0.2))
    ycor = c(smooth.spline(summ.pCI$q75 ~ summ.pCI$threshold)$y, 
             rev(smooth.spline(summ.pCI$q25 ~ summ.pCI$threshold)$y))
    polygon(xcor, ycor, border = NA, col = "slategray1")
    lines(smooth.spline(summ.pCI$max ~ summ.pCI$threshold), lty = 3, col = "darkblue")
    lines(smooth.spline(summ.pCI$q75 ~ summ.pCI$threshold), lty = 2, col = "darkblue")
    lines(smooth.spline(summ.pCI$median ~ summ.pCI$threshold), lty = 1, lwd =2, col = "darkblue")
    lines(smooth.spline(summ.pCI$q25 ~ summ.pCI$threshold), lty = 2, col = "darkblue")
    lines(smooth.spline(summ.pCI$min ~ summ.pCI$threshold), lty = 3, col = "darkblue")
  }, silent = T)
  text(100, 1.03, "pCI filter", col = "darkblue", pos = 4)
  # add effectsizes passing p-value (> alpha): min, 1st quartile, median, 3rd quartile and max
  abline(h = max(rv$nonsig$effectsize), lty = 3, col = "grey60")
  abline(h = min(rv$nonsig$effectsize), lty = 3, col = "grey60")
  abline(h = median(rv$nonsig$effectsize), lty = 1, col = "grey60")
  abline(h = quantile(rv$nonsig$effectsize, 0.25), lty = 2, col = "grey60")
  abline(h = quantile(rv$nonsig$effectsize, 0.75), lty = 2, col = "grey60")
  mtext(c("p-value filter","max", "75%", "median", "25%", "min"), 
        side = 2, line = 0.5, las = 2, cex = 0.8, adj = 1,
        at = c(1.03,max(rv$nonsig$effectsize) - 0.01, quantile(rv$nonsig$effectsize, 0.75), 
               median(rv$nonsig$effectsize),
               quantile(rv$nonsig$effectsize, 0.25), min(rv$nonsig$effectsize)))
  ## plot2: pMDE (8)
  op <- par(mar = c(3,0,1,1))
  plot(summ.pMDE$max ~ summ.pMDE$threshold, type ="n", 
       ylim = c(min(rv$experiments$effectsize) - 0.05, max(rv$experiments$effectsize) + 0.05), 
       xlim = rev(c(30,100)),
       las = 2, xlab = "", ylab = "", las = 1, bty ="l",
       yaxt = "n", xaxt = "n")
  axis(side = 1)
  mtext("Threshold", side = 1, line = 2.5)
  xcor = c(summ.pMDE$threshold, rev(summ.pMDE$threshold))
  try({
    ycor = c(smooth.spline(summ.pMDE$max ~ summ.pMDE$threshold)$y, 
             rev(smooth.spline(summ.pMDE$min ~ summ.pMDE$threshold)$y))
    polygon(xcor, ycor, border = NA, col = adjustcolor("darkseagreen2", alpha.f = 0.2))
    ycor = c(smooth.spline(summ.pMDE$q75 ~ summ.pMDE$threshold)$y, 
             rev(smooth.spline(summ.pMDE$q25 ~ summ.pMDE$threshold)$y))
    polygon(xcor, ycor, border = NA, col = "darkseagreen2")
    lines(smooth.spline(summ.pMDE$max ~ summ.pMDE$threshold), lty = 3, col = "darkgreen")
    lines(smooth.spline(summ.pMDE$q75 ~ summ.pMDE$threshold), lty = 2, col = "darkgreen")
    lines(smooth.spline(summ.pMDE$median ~ summ.pMDE$threshold), lty = 1, lwd = 2, col = "darkgreen")
    lines(smooth.spline(summ.pMDE$q25 ~ summ.pMDE$threshold), lty = 2, col = "darkgreen")
    lines(smooth.spline(summ.pMDE$min ~ summ.pMDE$threshold), lty = 3, col = "darkgreen")
  }, silent = T)
  text(100, 1.03, "pMDE filter", col = "darkgreen", pos = 4)
  # add effectsizes passing p-value (> alpha): min, 1st quartile, median, 3rd quartile and max
  abline(h = max(rv$nonsig$effectsize), lty = 3, col = "grey60")
  abline(h = min(rv$nonsig$effectsize), lty = 3, col = "grey60")
  abline(h = median(rv$nonsig$effectsize), lty = 1, col = "grey60")
  abline(h = quantile(rv$nonsig$effectsize, 0.25), lty = 2, col = "grey60")
  abline(h = quantile(rv$nonsig$effectsize, 0.75), lty = 2, col = "grey60")
  ## plot3: pMDD (7)
  plot(summ.pMDD$max ~ summ.pMDD$threshold, type ="n", 
       ylim = c(min(rv$experiments$effectsize) - 0.05, max(rv$experiments$effectsize) + 0.05), 
       xlim = rev(c(30,100)),
       las=2, xlab = "", 
       ylab = "", las = 1, bty ="l", yaxt = "n", xaxt = "n")
  axis(side = 1)
  xcor = c(summ.pMDD$threshold, rev(summ.pMDD$threshold))
  try({
    ycor = c(smooth.spline(summ.pMDD$max ~ summ.pMDD$threshold)$y, 
             rev(smooth.spline(summ.pMDD$min ~ summ.pMDD$threshold)$y))
    polygon(xcor, ycor, border =NA, col = adjustcolor( "mistyrose2", alpha.f = 0.2))
    ycor = c(smooth.spline(summ.pMDD$q75 ~ summ.pMDD$threshold)$y, 
             rev(smooth.spline(summ.pMDD$q25 ~ summ.pMDD$threshold)$y))
    polygon(xcor, ycor, border =NA, col = "mistyrose2")
    lines(smooth.spline(summ.pMDD$max ~ summ.pMDD$threshold), lty = 3, col = "darkred")
    lines(smooth.spline(summ.pMDD$q75 ~ summ.pMDD$threshold), lty = 2, col = "darkred")
    lines(smooth.spline(summ.pMDD$median ~ summ.pMDD$threshold), lty = 1, lwd =2, col = "darkred")
    lines(smooth.spline(summ.pMDD$q25 ~ summ.pMDD$threshold), lty = 2, col = "darkred")
    lines(smooth.spline(summ.pMDD$min ~ summ.pMDD$threshold), lty = 3, col = "darkred")
  }, silent = T)
  text(100, 1.03, "pMDD filter", col = "darkred", pos = 4)
  # add effectsizes passing p-value (> alpha): min, 1st quartile, median, 3rd quartile and max
  abline(h = max(rv$nonsig$effectsize), lty = 3, col = "grey60")
  abline(h = min(rv$nonsig$effectsize), lty = 3, col = "grey60")
  abline(h = median(rv$nonsig$effectsize), lty = 1, col = "grey60")
  abline(h = quantile(rv$nonsig$effectsize, 0.25), lty = 2, col = "grey60")
  abline(h = quantile(rv$nonsig$effectsize, 0.75), lty = 2, col = "grey60")
  par(op)
})


} # end of server
     
# Run ------------------
shinyApp(ui = ui, server = server)

