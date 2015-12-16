sim_regression_data <- function(N){
  srd <- data.frame(
    x1 = runif(N, min=0, max=100),
    x2 = runif(N, min=0, max=100), 
    x3 = runif(N, min=0, max=100)
  )
  transform(srd, y = 100 + 1.1*x1 + 2.2*x2 + 3.3*x3 + rnorm(N, sd=5))
}

sim_classification_data <- function(N){
  x <- runif(N, min=-5, max=5)
  y <- runif(N, min=-5, max=5)
  grp <- ifelse( x^2 + y^2 + rnorm(N) < 3, "red", "blue")
  data.frame(x=x, y=y, grp=grp)
}
