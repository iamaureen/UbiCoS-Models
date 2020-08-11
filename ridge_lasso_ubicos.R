# Load libraries, get data & set seed for reproducibility ---------------------
set.seed(123)    # seef for reproducibility
library(glmnet)  # for ridge regression
library(dplyr)   # for data cleaning


ubicos <- read.csv("~/Documents/PythonCode/Data/UbiCoS/ubicos_model_data_v2.csv", stringsAsFactors=FALSE)

#transform data into matrices as required by glmnet
#X <- ubicos[,c(4:10)] %>% as.matrix #only the 7 factors (personal + platform property)
X <- with(ubicos, as.matrix(cbind("hh*ss"=ubicos[,4]*ubicos[,8],"hh"=ubicos[,4], "ss"=ubicos[,8],
                                  "ff*pp"=ubicos[,5]*ubicos[,9],"ff"=ubicos[,5], "pp"=ubicos[,9],
                                  "mm*platform"=ubicos[,6]*ubicos[,10],"mm"=ubicos[,6], "platform"=ubicos[,10],
                                  "cc"=ubicos[,7]))) 
y <- ubicos[,17] %>% as.matrix

# ####### Ridge Regression ####### 
# Perform 10-fold cross-validation to select lambda ---------------------------
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
# Setting alpha = 0 implements ridge regression
ridge_cv <- cv.glmnet(X, y, alpha = 0, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)
# Plot cross-validation results
plot(ridge_cv)

# Best cross-validated lambda
lambda_cv <- ridge_cv$lambda.min
# Fit final model, get its sum of squared residuals and multiple R-squared
model_cv <- glmnet(X, y, alpha = 0, lambda = lambda_cv, standardize = TRUE)
y_hat_cv <- predict(model_cv, X)
ssr_cv <- t(y - y_hat_cv) %*% (y - y_hat_cv)
rsq_ridge_cv <- cor(y, y_hat_cv)^2

coef(model_cv)

# ####### Lasso Regression ####### 
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
# Setting alpha = 1 implements lasso regression
lasso_cv <- cv.glmnet(X, y, alpha = 1, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)
# Plot cross-validation results
plot(lasso_cv)

# Best cross-validated lambda
lambda_cv <- lasso_cv$lambda.min
# Fit final model, get its sum of squared residuals and multiple R-squared
model_cv <- glmnet(X, y, alpha = 1, lambda = lambda_cv, standardize = TRUE)
y_hat_cv <- predict(model_cv, X)
ssr_cv <- t(y - y_hat_cv) %*% (y - y_hat_cv)
rsq_lasso_cv <- cor(y, y_hat_cv)^2

coef(model_cv)

rsq <- cbind("R-squared" = c(rsq_ridge_cv,  rsq_lasso_cv))
rownames(rsq) <- c("ridge cross-validated",  "lasso cross_validated")
print(rsq)