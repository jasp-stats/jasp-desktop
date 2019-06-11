emmeans.rma <- function(object, formula, data, conf.level = .95, contrasts = NULL, ...) {
  if (is.null(object$call) || is.null(object$call$yi) || !inherits(object$call$yi, "formula")) 
    stop("Currently this only works with rma and rma.mv objects that have specified 'yi' as a formula.")
  
  mdlformula = object$call$yi
  mdlformula[[2]] = NULL # remove dependent
  
  # build a new data matrix
  used_terms = all.vars(mdlformula)
  if(!(all.vars(formula) %in% used_terms)) stop("Formula is refering to factors that are not in the model.")
  factor_levels = lapply(data[, used_terms, drop=FALSE],  function(x) if (!is.numeric(x)) levels(factor(x)) ) # extract factor levels
  .grid = expand.grid(Filter(Negate(is.null), factor_levels))
  if(nrow(.grid) == 0) stop("There are no predictors in the model that correspond to factors in '", deparse(substitute(data)), "'. Factor levels: ", paste(capture.output(str(factor_levels)), collapse = ", "), ". Data names: ", paste(names(data), collapse = ", "), "; Used Terms: ",paste(used_terms, collapse=", "), "; Data set head: ", paste(capture.output(head(data)), collapse="\n"))    
  covariate_means = colMeans(data[, names(Filter(is.null, factor_levels)), drop = FALSE], na.rm = TRUE)
  newdata = do.call(transform, c(list(.grid), as.list(covariate_means)))
  
  b = coef(object)
  Vb = vcov(object)
  
  # compute transformation matrix: 
  # - M: design matrix conforming the model for obtain cell means for all factors crossed (fixing covariates to their sample averages; should this be within cell averages?)
  # - K: matrix with dummies for picking out the means to combine
  # - K.M = (K'K)^-1 K'M is the transformation matrix for the model's regression coefficients to yield the marginal means
  M = do.call(model.matrix, list(mdlformula, newdata))
  match.colnames =  max.col(-adist(names(b), colnames(M)))
  M = M[, match.colnames]
  factors = all.vars(formula)
  Kmf = model.frame(paste("~ 0 + ", factors, collapse = ":"), unique(.grid[, factors, drop = FALSE]))
  K = model.matrix(Kmf, newdata, constrasts = sapply(factors, function(...) 'contr.Treatment',simplify = F))
  K.M = MASS::ginv(K) %*% M
  
  # if contrast matrix is specified, a hypothesis test on these contrasts are given
  if(!missing(contrasts) && is.matrix(contrasts)) {
    if (is.null(rownames(contrasts))) rownames(contrasts) = paste(deparse(substitute(contrasts)), "[", 1:nrow(contrasts), ", ]", sep="")
    K.M = contrasts %*% K.M
  }
  
  # compute marginal means requested, plus their standard errors
  emmeans = K.M %*% b
  se = diag(K.M %*% Vb %*% t(K.M))^0.5
  
  # build return structure
  if (!missing(contrasts)) {
    ret = data.frame(Contrast=rownames(K.M), Estimate = emmeans, SE = se, df = df.residual(object))
    t = emmeans / se
    pval = pt(-abs(t), ret$df, lower.tail = TRUE)
    comparisons = if (is.null(rownames(contrasts))) ret$Contrast else rownames(contrasts)
    ret = transform(ret, Contrast = comparisons, `t value` = t, `P(>|t|)` = pval, check.names=FALSE)
  } else {
    ret = transform(Kmf, emmean = emmeans, SE = se, df = df.residual(object))
    p = (1-conf.level) / 2
    ret = transform(ret, lower.CL = emmean + qt(p, df) * SE, upper.CL = emmean - qt(p, df) * SE)
  }
  rownames(ret) = NULL
  structure(ret, collapsed = setdiff(colnames(.grid), all.vars(formula)), conf.level = conf.level,
            covariate_means = covariate_means, class = c("emmeans.rma","data.frame"))
}

print.emmeans.rma <- function(x, ...) {
  print(structure(x, class = "data.frame"), ...)
  if (length(attr(x, "collapsed")) > 0 ) {
    cat("\n\nMeans are averaged over the levels of: ")
    cat(attr(x, "collapsed"), sep = ", ")
  }  
  if (length(attr(x,"covariate_means")) > 0) {
    cat("\nThe following means for the covariates were used:\n")
    print(attr(x,"covariate_means"), ...)
  }
  cat("\nConfidence level used:", attr(x,"conf.level"), "\n")
  invisible(x)
}


emmeans_contrasts.rma <- function(x, ..., contrasts) {
  C = contrasts
  if (!is.matrix(C)) stop("Argument 'contrasts' should be a matrix.")
  if (!all(abs(rowSums(C)) < .Machine$double.eps)^.5) stop("Invalid contrasts: Rows in", deparse(substitute(contrasts)), "do not add to 0.")
  emmeans.rma(x, ..., contrasts = C)
}

contr.all_deviations <- function(n) {
  # n is either a factor, or a number (of levels)
  # Deviation contrasts for all levels are computed
  fact <- NA
  if (is.factor(n)) {
    fact <- n
    n <- nlevels(n)
  }
  C <- diag(n) - 1/n
  rownames(C) = colnames(C) = if (is.factor(fact)) levels(fact) else 1:n
  C
}

contr.all_pairs <- function(n) {
  # n is either a factor, or a number of levels
  # Contrasts for all possible pairwise comparisons are computed
  fact = NA
  if (is.factor(n)) {
    fact = n
    n = nlevels(n)
  }
  if (length(n) > 1) {n = n[1]; warning("Only using first element of", deparse(substitute(n)),".")}
  if (n < 2) stop(deparse(substitute(n)), "is small than 2.")
  N = n-1
  C = sapply(1:n, function(i) cbind(-1, diag(N))[,(0:N+i) %% n + 1, drop=FALSE])
  C = C[!duplicated(abs(C)),, drop = FALSE]
  colnames(C) = if (is.factor(fact)) levels(fact) else 1:n
  rownames(C) = apply(C, 1, function(x) {i <- which(x!=0); nms <- colnames(C)[i[order(x[i], decreasing=TRUE)]]; paste(nms, collapse = " - ")})
  C
}

addSignifStars <- function(x, ..., column = "P(>|t|)", stars = c(' ***'=.001, ' **'=.01, ' *'=.05, ' .'=.1) ) {
  X = transform(x, ` ` = c(names(stars),'')[findInterval(get(column), c(0, stars, 1))], check.names = FALSE, stringsAsFactors=FALSE)
  attributes(X) <- attributes(x)
  names(X) = ifelse(is.na(names(X)), ' ', names(X))
  X
}

(function(conf.level = .95){
  formula = ~  colour
  data = carData::Arrests
  fit = lm(checks ~ colour*sex+employed, data = data)
  mf = if(is.data.frame(model.frame(fit))) model.frame(fit) else data[, all.vars(mdlformula)]
  
  mdlformula = fit$call$formula
  mdlformula[[2]] = NULL # remove response variable
  
  b = coef(fit)
  Vb = vcov(fit)
  
  tmp = expand.grid(Filter(Negate(is.null), lapply(mf, levels)))
  #browser()
  M = do.call(model.matrix, list(mdlformula, tmp)) #, contrasts = list(wool='contr.Treatment', tension='contr.Treatment')))
  Kmf = model.frame(paste("~ 0 + ",all.vars(formula), collapse = ":"), unique(tmp[, all.vars(formula), drop = FALSE]))
  K = model.matrix(Kmf, tmp, contrasts = as.list(setNames(rep("contr.Treatment", length(all.vars(formula))), all.vars(formula))))
  K.M = ginv(K) %*% M
  em = K.M %*% b
  se = diag(K.M %*% Vb %*% t(K.M))^.5
  ret = transform(Kmf, emmean = em, SE = se, df = df.residual(fit))
  structure(transform(ret, lower.CL = emmean - qt(.025, df)*se, upper.CL = emmean - qt(0.5-conf.level/2, df)*se), 
            collapsed = setdiff(all.vars(mdlformula), all.vars(formula)), conf.level = conf.level)
})#() # () # for testing the correctness


###
### NIET AAN BEGINNEN!!!! (gebruik gewoon package emmeans!)
###
#
# emmeans <- function(object, ...) UseMethod("emmeans")
# 
# emmeans.default <- function(object, conditioning, contrasts = NULL) {
#   require(tidyverse)
#   conditioning = if (is_formula(conditioning)) all.vars(conditioning[-2]) else as.character(conditioning)
#   
#   method = match.arg(method)
# 
#   mf = model.frame(object) 
#   used_terms = names(mf)
#   
#   factor_levels = Filter(Negate(is.null), lapply(mf, levels)) # extract factor levels
#   covariate_names = names(Filter(is.null, lapply(mf, levels)))
#   .grid = expand.grid(factor_levels)
#   covariate_means = colMeans(mf[covariate_names], na.rm = TRUE)
#   newdata = do.call(transform, c(list(.grid), as.list(covariate_means)))
#   
#   piv = object$qr$pivot[1:object$rank]
#   b = coef(object)[piv]
#   Vb = vcov(object)[piv,piv]
#   
#   # compute transformation matrix: 
#   # - M: design matrix conforming the model for obtain cell means for all factors crossed (fixing covariates to their sample averages; should this be within cell averages?)
#   # - K: matrix with dummies for picking out the means to combine
#   # - K.M = (K'K)^-1 K'M is the transformation matrix for the model's regression coefficients to yield the marginal means
# #  M = do.call(model.matrix, list(mdlformula, newdata))
#   M = model.matrix(terms(object), newdata)[, object$qr$pivot[1:object$rank], drop = FALSE] 
#   match.colnames =  max.col(-adist(names(b), colnames(M)))
#   M = M[, match.colnames]
#   factors = all.vars(formula)
#   Kmf = model.frame(paste("~ 0 + ", factors, collapse = ":"), unique(.grid[, factors, drop = FALSE]))
#   K = model.matrix(Kmf, newdata, constrasts = sapply(factors, function(...) 'contr.Treatment',simplify = F))
#   K.M = MASS::ginv(K) %*% M
#   
#   # if contrast matrix is specified, a hypothesis test on these contrasts are given
#   if(!missing(contrasts) && is.matrix(contrasts)) {
#     K.M = contrasts %*% K.M
#   }
#   
#   # compute marginal means requested, plus their standard errors
#   emmeans = K.M %*% b
#   se = diag(K.M %*% Vb %*% t(K.M))^0.5
#   
#   # build return structure
#   if (!missing(contrasts)) {
#     ret = data.frame(Contrast=rownames(K.M), Estimate = emmeans, SE = se, df = df.residual(object))
#     t = emmeans / se
#     pval = pt(-abs(t), ret$df, lower.tail = TRUE)
#     comparisons = if (is.null(rownames(contrasts))) ret$Contrast else rownames(contrasts)
#     ret = transform(ret, Contrast = comparisons, `t value` = t, `P(>|t|)` = pval, check.names=FALSE)
#   } else {
#     ret = transform(Kmf, emmean = emmeans, SE = se, df = df.residual(object))
#     p = (1-conf.level) / 2
#     ret = transform(ret, lower.CL = emmean + qt(p, df) * SE, upper.CL = emmean - qt(p, df) * SE)
#   }
#   rownames(ret) = NULL
#   structure(ret, collapsed = setdiff(colnames(.grid), all.vars(formula)), conf.level = conf.level,
#             covariate_means = covariate_means, class = c("emmeans.rma","data.frame"))
# }
# 
# 
# 
