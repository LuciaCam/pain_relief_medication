#######################################################################
#
# Fonction R permettant de calculer les facteurs d'inflation de la
# variance (VIF - "variance inflation factors" - VIF)
#
#######################################################################
# Last modified: 23.07.2012.
#######################################################################
# Warranty: none.
#######################################################################

VIF = function(matx, y){
  #
  fit.dat = data.frame(y=y, matx)
  fit.lm = lm(y ~ . , data=fit.dat)
  #
  vifs = vif.lm.car(fit.lm)
  #
  cat("\n")
  print(signif(vifs))
  cat("\n  Mean:", format(signif(mean(vifs))), "\n\n")
}

#######################################################################

vif.lm.car = function(mod, ...)
{
  # Calculates variance-inflation and generalized variance-inflation factors for linear and generalized linear models.
  #
  # "vif" function from package "car" (Version 2.0-12)
  # Source: http://cran.r-project.org/web/packages/car/
  # R documentation: ?car::vif
  if (any(is.na(coef(mod))))
    stop("there are aliased coefficients in the model")
  v <- vcov(mod)
  assign <- attributes(model.matrix(mod))$assign
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  }
  else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2)
    stop("model contains fewer than 2 terms")
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs]))/detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1))
    result <- result[, 1]
  else result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  result
}

#######################################################################

## VIF = function(matx, y){
##  if(is.na(match("package:car", search()))) require("car")
##  fit.dat = data.frame(y=y, matx)
##  fit.lm = lm(y ~ . , data=fit.dat)
##  vif(fit.lm)
## }

#######################################################################
