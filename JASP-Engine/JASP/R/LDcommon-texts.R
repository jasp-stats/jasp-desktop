#
# Copyright (C) 2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

.ldParsSupportMoments <- function(pars, support, moments){
  formulas <- createJaspHtml(title = gettext("Parameters, Support, and Moments"))
  formulas$dependOn(c("parsSupportMoments", "parametrization"))
  formulas$position <- 2
  
  if(!is.na(moments)){
    text <- gettextf(
      "<b>Parameters</b>
      %s
      
      <b>Support</b>
      %s
      
      <b>Moments</b>
      E(X) = %s
      Var(X) = %s", paste(pars, collapse = " \n "), support, moments[[1]], moments[[2]])
  } else{
    text <- gettextf(
      "<b>Parameters</b>
      %s
      
      <b>Support</b>
      %s
      
      <b>Moments</b>
      not available", paste(pars, collapse = " \n "), support)
  }
  formulas$text <- text
  
  return(formulas)
}

.ldIntroText <- function(jaspResults, options, introText = NULL){
  if(!options$explanatoryText) return()
  if(!is.null(jaspResults[['introText']])) return()
  
  intro <- createJaspHtml()
  intro$dependOn(c("explanatoryText"))
  intro$position <- 1
  
  if(is.function(introText)){
    intro[['text']] <- gettext(introText())
  } else if(is.character(introText)){
    intro[['text']] <- gettextf(.ldAllTextsList()$explanations$intro,
                                gettext(introText), gettext(introText), gettext(introText), gettext(introText), gettext(introText))
  }
  
  jaspResults[['introText']] <- intro
  
  return()  
}

.ldAllTextsList <- function(){
  list(
    explanations = list(
      pdf = gettext("The probability density function (PDF), usually denoted as f(x), is a function of a random variable X.
    The value of f(x) provides the relative likelihood that a realization of the random variable X yields a value equal to x. 
    More formally, the PDF is defined as a derivative of a cumulative distribution function (CDF).
    
    The density plot displays the probability density function of a random variable.
    The y-axis displays the value of the density function for a particular value of the random variable (displayed on the x-axis)."),
      
      pmf = gettext("The probability mass function (PMF), usually denoted as f(x), is a function of a random variable X.
    The value of f(x) provides the probability that a realization of the random variable X yields a value equal to x.
    
    The probability mass plot displays the probability mass function of a random variable.
    The y-axis displays the value of the probability mass function for a particular value of the random variable (displayed on the x-axis)."),
      
      cdf = gettext("The cumulative distribution function (CDF), usually denoted as F(x), is a function of a random variable X.
    The value of F(x) provides the probability that a realization of the random variable X yields a value that is equal to or smaller than x.
    
    The cumulative probability plot displays the cumulative distribution of a random variable.
    The y-axis displays the value of the cumulative distribution function for a particular value of the random variable (displayed on the x-axis)."),
      
      qf  = gettext("The quantile function, usually denoted as Q(p), is the inverse of the cumulative distribution function.
    The function gives the quantile such that the probability of the random variable being less than or equal to that value equals the given probability p.   
    
    The quantile plot displays the quantile function.
    The y-axis displays the quantile of which the probability that the random variable is less or equal to that value is equal to p (displayed on the x-axis)."),
      
      intro = "<h3> Demonstration of the %s </h3>
    This demonstration is divided into four parts. 
    The first part displays the %s, its probability density function, cumulative distribution function, and quantile function. 
    The second part allows to generate data from the %s, compute descriptive statistics and display descriptive plots.
    In the third part, the parameters of the %s can be estimated.
    The fourth part allows to check the fit of the %s to the data.
    
    <b>References</b>
    
    Blitzstein, J. K., & Hwang, J. (2014). <i>Introduction to probability.</i> Chapman and Hall/CRC.
    
    Leemis, L. M., & Pasupathy, R. (2019). The ties that bind. <i>Significance</i>.
    
    For relationships with other distributions, visit www.math.wm.edu/~leemis/chart/UDR/UDR.html.
    
    https://en.wikipedia.org/wiki/List_of_probability_distributions"
    ),
    references   = list(
      jasp = "JASP Team (2020). JASP (Version 0.11.1) [Computer software].",
      goftest = "Julian Faraway, George Marsaglia, John Marsaglia and Adrian Baddeley (2017). goftest: Classical Goodness-of-Fit Tests for Univariate Distributions. R package version 1.1-1. https://CRAN.R-project.org/package=goftest",
      fitdistrplus = "Marie Laure Delignette-Muller, Christophe Dutang (2015). fitdistrplus: An R Package for Fitting Distributions. Journal of Statistical Software, 64(4), 1-34. URL: http://www.jstatsoft.org/v64/i04/.",
      car = "John Fox and Sanford Weisberg (2011). An R Companion to Applied Regression, Second Edition. Thousand Oaks CA: Sage. URL: http://socserv.socsci.mcmaster.ca/jfox/Books/Companion."
    ),
    feedback = list(
      fitdistrError = gettext("Estimation failed: try adjusting parameter values, check outliers, or feasibility of the distribution fitting the data.")
    )
  )
}
