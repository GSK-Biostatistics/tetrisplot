step_univariate <- function(data, model, family, level){
  
  model <- as.formula(model)
  
  fit <- stats::glm(formula = model, 
                    family = family, 
                    data = data)
  
  test <- aod::wald.test(Sigma = vcov(fit), 
                         b = coef(fit), 
                         Terms = 2)
  
  rslt <- test$result$chi2[["P"]] < level
  
  if(rslt) {
    attr(fit$terms, "term.labels")
  } else {
    NA_character_
  }
}

step_backward <- function(data, model, family, k){
  
  model <- as.formula(model)
  
  fit <- stats::glm(formula = model, 
                    family = family, 
                    data = data)
  
  y <- formula.tools::lhs.vars(model)
  lwr <- as.formula(paste0(y, "~ 1"))
  scope_list <- list(lower = lwr, 
                     upper = model)
  fit_step <- stats::step(fit, 
                          scope = scope_list, 
                          direction = "backward", 
                          trace = 0,
                          scale = 0,
                          k = k)
  rslt <- attr(fit_step$terms, "term.labels")
  return(rslt)
}



