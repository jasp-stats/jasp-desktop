quick.rstudent <- function(x, ...) { # the metafor::rstudent is weirdly slow...
  # formula's used from Steven's (1995), which disappeared in the newest edition of the book Stevens (2015)
  h = hatvalues(x)
  n = length(h)
  p = sum(h)
  df.resid = n - p - 1
  e = resid(x)
  sigma = sqrt(sum(e^2) / df.resid)
  rstud = e / sqrt(1-h) / sigma
  tval = rstud * sqrt((n-p-1) / (n-p-rstud^2))
  structure(rstud, t.value = tval, p.value = 2*pt(-abs(tval), df.resid), df=df.resid)
}

hat.significance <- function(x, h = if (inherits(x, "numeric")) x else hatvalues(x)) {
  nms = if (!is.null(names(h))) names(h) else seq_along(h)
  n = length(h)
  p = sum(h)
  df = n - p
  statistic = (n-p) * (h - 1/n) / ((1-h) * (p-1)) # F-values
  structure(h, statistic = statistic, df=c(df1 = p-1, df2 = n-p), p.value = pf(statistic, p-1, n-p, lower.tail = FALSE))
}

mahalanobis.significance <- function(x) {
  require(MASS)
  .X = model.matrix(x)
  df = qr(.X)$rank
  mahdist = mahalanobis(.X,colMeans(.X), MASS::ginv(cov(.X)), inverted = T)
  p.mahdist = pchisq(mahdist, df, lower.tail = FALSE)
  structure(mahdist, statistic = mahdist, df=df, p.value = p.mahdist)
}

quick.influence <- function(x, ...) {
  rstud = quick.rstudent(x)
  p.rstud = attr(rstud, 'p.value')
  hat = hat.significance(x)
  p.hat = attr(hat, 'p.value')
  mahdist = mahalanobis.significance(x)
  p.mahdist = attr(mahdist, 'p.value')
  structure(
    data.frame(#` ` = "|", 
      studentized = rstud, stud.pval = p.rstud, #` ` = marks[findInterval(p.rstud, cutof)],
      hatvalues = hat, hat.pval=p.hat, #` `= marks[findInterval(p.hat, cutof)],
      mahalanobis = mahdist, manh.pval = p.mahdist, #` ` = marks[findInterval(p.mahdist, cutof)],
      check.names=FALSE),
    class = c("quick.influence", "data.frame"))
}

print.quick.influence <- function(x, ...) {
  marks = c('***  \u23B9 ','**  \u23B9 ','*  \u23B9 ','.  \u23B9 ','  \u23B9 ')
  cutof = c(0,.001,.01,.05,.1,1)
  M = with(x, data.frame(` ` = "\u23B8", 
            studentized = studentized, `p-value` = stud.pval, ` ` = marks[findInterval(stud.pval, cutof)],
            hatvalues = hatvalues, `p-value` = hat.pval, ` ` = marks[findInterval(hat.pval, cutof)],
            mahalanobis = mahalanobis, `p-value` = manh.pval, ` ` = marks[findInterval(manh.pval, cutof)],
            check.names = FALSE)
           )
  rownames(M) = rownames(x)
  print(M, ...)
}
