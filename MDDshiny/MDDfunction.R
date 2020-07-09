MDD=function (N1, N2, variance1, variance2 = NULL, alpha = 0.05, two.sided = TRUE, var.equal = TRUE)
{
  m <- NULL
  m$n1 <- N1
  m$n2 <- N2
  m$variance1 <- variance1
  m$variance2 <- variance2
  m$alpha <- alpha
  m$two.sided <- two.sided
  if (var.equal) {
    dfreedom <- N1 + N2 - 2
    nu <- sqrt(variance1) * sqrt(1/N1 + 1/N2)
    m$nu <- nu
  }
  if (!var.equal) {
    dfreedom <- (variance1/N1 + variance2/N2)^2 / ((variance1/N1)^2/(N1-1) + (variance2/N2)^2/(N2-1))
    nu <- sqrt(variance1/N1 + variance2/N2)
    m$nu <- nu
  }
  m$df <- dfreedom
  if (two.sided) {
    m$mdd <- qt(alpha/2, df = dfreedom, lower.tail=F) * nu
  }
  if (!two.sided) {
    m$mdd <- qt(alpha, df = dfreedom, lower.tail=F) * nu
  }
  class(m) <- "MDD"
  return(m)
}