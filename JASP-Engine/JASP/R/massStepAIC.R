# MASS replacements for JASP

getDatasetFromNearestAncestor <- function(name, i=1) {
  var <- try(get(name, envir=parent.frame(i)), silent = TRUE)
  if (inherits(var, "try-error") || class(var)!="data.frame") {
    var <- getDatasetFromNearestAncestor(name, i+1)
  }
  var
}


JASPaddterm <-
  function(object, scope, scale = 0, test = c("none", "Chisq", "F"),
           k = 2, sorted = FALSE, trace = FALSE, ...)
{
    Fstat <- function(table, rdf) {
      dev <- table$Deviance
      df <- table$Df
      diff <- pmax(0, (dev[1L] - dev)/df)
      Fs <- diff/(dev/(rdf-df))
      Fs[df < .Machine$double.eps] <- NA
      P <- Fs
      nnas <- !is.na(Fs)
      P[nnas] <- safe_pf(Fs[nnas], df[nnas], rdf - df[nnas], lower.tail=FALSE)
      list(Fs=Fs, P=P)
    }
    if(missing(scope) || is.null(scope)) stop("no terms in scope")
    if(!is.character(scope))
        scope <- add.scope(object, update.formula(object, scope))
    if(!length(scope))
        stop("no terms in scope for adding to object")
    oTerms <- attr(terms(object), "term.labels")
    int <- attr(object$terms, "intercept")
    ns <- length(scope)
    dfs <- dev <- numeric(ns+1)
    names(dfs) <- names(dev) <- c("<none>", scope)
    add.rhs <- paste(scope, collapse = "+")
    add.rhs <- eval(parse(text = paste("~ . +", add.rhs)))
    new.form <- update.formula(object, add.rhs)
    oc <- object$call
    Terms <- terms(new.form)
    oc$formula <- Terms
    ## model.frame.glm looks at the terms part for the environment
    fob <- list(call = oc, terms=Terms)
    class(fob) <- class(object)

    ###########################################################################
    ######### HERE IS THE EDIT, USE DATA FROM 2nd ORDER PARENT FRAME ##########
    ###########################################################################

    dname <- as.character(as.list(fob$call)$data)
    dset <- parent.frame(2)[[dname]]

    x <- model.matrix(Terms, model.frame(fob, xlev = object$xlevels, data=dset),
                      contrasts = object$contrasts)

    ###########################################################################

    n <- nrow(x)
    oldn <- length(object$residuals)
    y <- object$y
    newn <- length(y)
    if(newn < oldn)
        warning(sprintf(ngettext(newn,
                                 "using the %d/%d row from a combined fit",
                                 "using the %d/%d rows from a combined fit"),
                        newn, oldn), domain = NA)
    wt <- object$prior.weights
    if(is.null(wt)) wt <- rep(1, n)
    Terms <- attr(Terms, "term.labels")
    asgn <- attr(x, "assign")
    ousex <- match(asgn, match(oTerms, Terms), 0L) > 0L
    if(int) ousex[1L] <- TRUE
    X <- x[, ousex, drop = FALSE]
    z <-  glm.fit(X, y, wt, offset=object$offset,
                  family=object$family, control=object$control)
    dfs[1L] <- z$rank
    dev[1L] <- z$deviance
    ## workaround for PR#7842. terms.formula may have flipped interactions
    sTerms <- sapply(strsplit(Terms, ":", fixed=TRUE),
                     function(x) paste(sort(x), collapse=":"))
    for(tt in scope) {
        if(trace) {
            message(gettextf("trying + %s", tt), domain = NA)
	    utils::flush.console()
	}
        stt <- paste(sort(strsplit(tt, ":")[[1L]]), collapse=":")
	usex <- match(asgn, match(stt, sTerms), 0L) > 0L
        X <- x[, usex|ousex, drop = FALSE]
        z <-  glm.fit(X, y, wt, offset=object$offset,
                      family=object$family, control=object$control)
        dfs[tt] <- z$rank
        dev[tt] <- z$deviance
    }
    if (is.null(scale) || scale == 0)
        dispersion <- summary(object, dispersion = NULL)$dispersion
    else dispersion <- scale
    fam <- object$family$family
    if(fam == "gaussian") {
	if(scale > 0) loglik <- dev/scale - n
	else loglik <- n * log(dev/n)
    } else loglik <- dev/dispersion
    aic <- loglik + k * dfs
    aic <- aic + (extractAIC(object, k = k)[2L] - aic[1L]) # same baseline for AIC
    dfs <- dfs - dfs[1L]
    dfs[1L] <- NA
    aod <- data.frame(Df = dfs, Deviance = dev, AIC = aic,
                      row.names = names(dfs), check.names = FALSE)
    o <- if(sorted) order(aod$AIC) else seq_along(aod$AIC)
    if(all(is.na(aic))) aod <- aod[, -3]
    test <- match.arg(test)
    if(test == "Chisq") {
        dev <- pmax(0, loglik[1L] - loglik)
        dev[1L] <- NA
        LRT <- if(dispersion == 1) "LRT" else "scaled dev."
        aod[, LRT] <- dev
        nas <- !is.na(dev)
        dev[nas] <- safe_pchisq(dev[nas], aod$Df[nas], lower.tail=FALSE)
        aod[, "Pr(Chi)"] <- dev
    } else if(test == "F") {
        if(fam == "binomial" || fam == "poisson")
            warning(gettextf("F test assumes 'quasi%s' family", fam),
                    domain = NA)
	rdf <- object$df.residual
	aod[, c("F value", "Pr(F)")] <- Fstat(aod, rdf)
    }
    aod <- aod[o, ]
    head <- c("Single term additions", "\nModel:", deparse(formula(object)))
    if(scale > 0)
        head <- c(head, paste("\nscale: ", format(scale), "\n"))
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
}

# file MASS/R/stepAIC.R
# copyright (C) 1994-2007 W. N. Venables and B. D. Ripley
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
JASPstepAIC <-
  function(object, scope, scale = 0,
           direction = c("both", "backward", "forward"),
           trace = 1, keep = NULL, steps = 1000, use.start = FALSE, k = 2, ...)
{
    mydeviance <- function(x, ...)
    {
        dev <- deviance(x)
        if(!is.null(dev)) dev else extractAIC(x, k=0)[2L]
    }

    cut.string <- function(string)
    {
        if(length(string) > 1L)
            string[-1L] <- paste("\n", string[-1L], sep = "")
        string
    }

    re.arrange <- function(keep)
    {
        namr <- names(k1 <- keep[[1L]])
        namc <- names(keep)
        nc <- length(keep)
        nr <- length(k1)
        array(unlist(keep, recursive = FALSE), c(nr, nc), list(namr, namc))
    }

    step.results <- function(models, fit, object, usingCp=FALSE)
    {
        change <- sapply(models, "[[", "change")
        rd <- sapply(models, "[[", "deviance")
        dd <- c(NA, abs(diff(rd)))
        rdf <- sapply(models, "[[", "df.resid")
        ddf <- c(NA, abs(diff(rdf)))
        AIC <- sapply(models, "[[", "AIC")
        heading <- c("Stepwise Model Path \nAnalysis of Deviance Table",
                     "\nInitial Model:", deparse(formula(object)),
                     "\nFinal Model:", deparse(formula(fit)),
                     "\n")
        aod <-
            if(usingCp)
                data.frame(Step = change, Df = ddf, Deviance = dd,
                           "Resid. Df" = rdf, "Resid. Dev" = rd,
                           Cp = AIC, check.names = FALSE)
            else data.frame(Step = change, Df = ddf, Deviance = dd,
                            "Resid. Df" = rdf, "Resid. Dev" = rd,
                            AIC = AIC, check.names = FALSE)
        attr(aod, "heading") <- heading
        class(aod) <- c("Anova", "data.frame")
        fit$anova <- aod
        fit
    }

    Terms <- terms(object)
    object$formula <- Terms
    if(inherits(object, "lme")) object$call$fixed <- Terms
    else if(inherits(object, "gls")) object$call$model <- Terms
    else object$call$formula <- Terms
    if(use.start) warning("'use.start' cannot be used with R's version of 'glm'")
    md <- missing(direction)
    direction <- match.arg(direction)
    backward <- direction == "both" | direction == "backward"
    forward <- direction == "both" | direction == "forward"
    if(missing(scope)) {
	fdrop <- numeric()
        fadd <- attr(Terms, "factors")
        if(md) forward <- FALSE
    } else {
        if(is.list(scope)) {
            fdrop <- if(!is.null(fdrop <- scope$lower))
                attr(terms(update.formula(object, fdrop)), "factors")
            else numeric()
            fadd <- if(!is.null(fadd <- scope$upper))
                attr(terms(update.formula(object, fadd)), "factors")
        } else {
            fadd <- if(!is.null(fadd <- scope))
                attr(terms(update.formula(object, scope)), "factors")
            fdrop <- numeric()
        }
    }
    models <- vector("list", steps)
    if(!is.null(keep)) keep.list <- vector("list", steps)
    n <- nobs(object, use.fallback = TRUE)  # might be NA
    fit <- object
    bAIC <- extractAIC(fit, scale, k = k, ...)
    edf <- bAIC[1L]
    bAIC <- bAIC[2L]
    if(is.na(bAIC))
        stop("AIC is not defined for this model, so 'stepAIC' cannot proceed")
    if(bAIC == -Inf)
        stop("AIC is -infinity for this model, so 'stepAIC' cannot proceed")
   nm <- 1
    Terms <- terms(fit)
    if(trace) {
        cat("Start:  AIC=", format(round(bAIC, 2)), "\n",
            cut.string(deparse(formula(fit))), "\n\n", sep='')
	utils::flush.console()
    }
    models[[nm]] <- list(deviance = mydeviance(fit), df.resid = n - edf,
                         change = "", AIC = bAIC)
    if(!is.null(keep)) keep.list[[nm]] <- keep(fit, bAIC)
    usingCp <- FALSE
    while(steps > 0) {
        steps <- steps - 1
        AIC <- bAIC
        ffac <- attr(Terms, "factors")
        ## don't drop strata terms
        if(!is.null(sp <- attr(Terms, "specials")) &&
           !is.null(st <- sp$strata)) ffac <- ffac[-st,]
        scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
        aod <- NULL
        change <- NULL
        if(backward && length(scope$drop)) {
            aod <- MASS::dropterm(fit, scope$drop, scale = scale,
                            trace = max(0, trace - 1), k = k, ...)
            rn <- row.names(aod)
            row.names(aod) <- c(rn[1L], paste("-", rn[-1L], sep=" "))
            ## drop all zero df terms first.
            if(any(aod$Df == 0, na.rm=TRUE)) {
                zdf <- aod$Df == 0 & !is.na(aod$Df)
                nc <- match(c("Cp", "AIC"), names(aod))
                nc <- nc[!is.na(nc)][1L]
                ch <- abs(aod[zdf, nc] - aod[1, nc]) > 0.01
                if(any(is.finite(ch) & ch)) {
                    warning("0 df terms are changing AIC")
                    zdf <- zdf[!ch]
                }
                ## drop zero df terms first: one at time since they
                ## may mask each other
                if(length(zdf) > 0L)
                    change <- rev(rownames(aod)[zdf])[1L]
            }
        }
        if(is.null(change)) {
            if(forward && length(scope$add)) {
              ## EDIT: USE JASPaddterm
                aodf <- JASPaddterm(fit, scope$add, scale = scale,
                                    trace = max(0, trace - 1), k = k, ...)
                rn <- row.names(aodf)
                row.names(aodf) <- c(rn[1L], paste("+", rn[-1L], sep=" "))
                aod <-
                    if(is.null(aod)) aodf
                    else rbind(aod, aodf[-1, , drop=FALSE])
            }
            attr(aod, "heading") <- NULL
            if(is.null(aod) || ncol(aod) == 0) break
            ## need to remove any terms with zero df from consideration
            nzdf <- if(!is.null(aod$Df)) aod$Df != 0 | is.na(aod$Df)
            aod <- aod[nzdf, ]
            if(is.null(aod) || ncol(aod) == 0) break
            nc <- match(c("Cp", "AIC"), names(aod))
            nc <- nc[!is.na(nc)][1L]
            o <- order(aod[, nc])
            if(trace) {
		print(aod[o,  ])
		utils::flush.console()
	    }
            if(o[1L] == 1) break
            change <- rownames(aod)[o[1L]]
        }
        usingCp <- match("Cp", names(aod), 0) > 0
        ## may need to look for a 'data' argument in parent
	fit <- update(fit, paste("~ .", change), evaluate = FALSE)
        fit <- eval.parent(fit)
        nnew <- nobs(fit, use.fallback = TRUE)
        if(all(is.finite(c(n, nnew))) && nnew != n)
            stop("number of rows in use has changed: remove missing values?")
        Terms <- terms(fit)
        bAIC <- extractAIC(fit, scale, k = k, ...)
        edf <- bAIC[1L]
        bAIC <- bAIC[2L]
        if(trace) {
            cat("\nStep:  AIC=", format(round(bAIC, 2)), "\n",
                cut.string(deparse(formula(fit))), "\n\n", sep='')
	    utils::flush.console()
	}
        ## add a tolerance as dropping 0-df terms might increase AIC slightly
        if(bAIC >= AIC + 1e-7) break
        nm <- nm + 1
        models[[nm]] <-
            list(deviance = mydeviance(fit), df.resid = n - edf,
                 change = change, AIC = bAIC)
        if(!is.null(keep)) keep.list[[nm]] <- keep(fit, bAIC)
    }
    if(!is.null(keep)) fit$keep <- re.arrange(keep.list[seq(nm)])
    step.results(models = models[seq(nm)], fit, object, usingCp)
}

extractAIC.loglm <- function(fit, scale, k = 2, ...)
{
    edf <- fit$n - fit$df
    c(edf,  fit$deviance + k * edf)
}

extractAIC.lme <- function(fit, scale, k = 2, ...)
{
    if(fit$method != "ML") stop("AIC undefined for REML fit")
    res <- logLik(fit)
    edf <- attr(res, "df")
    c(edf,  -2*res + k * edf)
}

extractAIC.gls <- function(fit, scale, k = 2, ...)
{
    if(fit$method != "ML") stop("AIC undefined for REML fit")
    res <- logLik(fit)
    edf <- attr(res, "df")
    c(edf,  -2*res + k * edf)
}

terms.gls <- terms.lme <- function(x, ...) terms(formula(x), ...)
