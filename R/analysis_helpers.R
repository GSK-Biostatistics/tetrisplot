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

step_grlasso <- function(data, model, family, grpreg_list = list(NULL)){
  
  if(!family %in% c("gaussian", "binomial", "poisson"))
    stop("grpreg::grpreg handles 'gaussian', 'binomial' and 'poisson' only")
  
  model <- as.formula(model)
  y_nam <- formula.tools::lhs.vars(model)
  x_nam <- formula.tools::rhs.vars(model)
  mod_mat <- stats::model.matrix(object = model, data = data)
  
  y <- data[[y_nam]]
  X <- mod_mat[,-1]
  grp_num <- attr(mod_mat, "assign")[-1]
  grp <- as.character(factor(grp_num, labels = x_nam))

  if(family == "binomial" & length(unique(y)) == 2){
    y <- ifelse(as.numeric(as.factor(y))-1 == 1, 1, 0)
  }
  
  single_cv_lambda <- grpreg::cv.grpreg(y = y, 
                                        X = X, 
                                        group = grp)
  fit <- purrr::exec(grpreg::grpreg,
                     X = X, 
                     y = y,
                     group = grp,
                     lambda = single_cv_lambda$lambda.min,
                     !!!grpreg_list)
  
  rslt <- unique(grp[which(as.numeric(fit$beta)[-1] != 0)])
  return(rslt)
}
