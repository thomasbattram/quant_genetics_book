
library(MASS)

# ------------------------------------------
# covariance
# ------------------------------------------
set.seed(2)
x <- rnorm(100)
y <- rnorm(100)

my_cov_func <- function(x, y) {
	n <- length(x)
	xy_bar <- sum(x * y) / n
	x_bar <- mean(x)
	y_bar <- mean(y)
	out <- (n * (xy_bar - x_bar * y_bar)) / (n - 1)
	return(out)
}

my_cov_func(x, y) # -0.063
cov(x, y) # -0.063
# huzza! 

qsub -I -l nodes=1:ppn=2,walltime=8:00:00

# ------------------------------------------
# least-squares
# ------------------------------------------

least_squares_func <- function(x, y) {
	# model: y ~ a + bx
	b <- cov(x, y) / var(x)
	a <- mean(y) - b * mean(x)
	return(list(estimate = b, intercept = a))
}

least_squares_func(x, y) # estimate = -0.04710, intercept = 0.02778
lm(y ~ x) # estimate = -0.04710, intercept = 0.02777
# huzza again! 

# ------------------------------------------
# correlation
# ------------------------------------------

cor_func <- function(x, y) {
	r <- cov(x, y) / sqrt(var(x) * var(y))
	return(r)
}

cor_func(x, y) # -0.05546129
cor(x, y) # -0.05546129
# huzza again! 

# ------------------------------------------
# differential selection 
# ------------------------------------------

generate_phens <- function(dad_phen, mum_phen, par_off_correlation) {
	midparent <- (dad_phen + mum_phen) / 2
	n <- length(midparent)
	theta <- acos(par_off_correlation)
	child <- rnorm(n)
	X     <- cbind(midparent, child)         # matrix
	Xctr  <- scale(X, center = TRUE, scale = FALSE)   # centered columns (mean 0)

	Id   <- diag(n)                               # identity matrix
	Q    <- qr.Q(qr(Xctr[ , 1, drop = FALSE]))      # QR-decomposition, just matrix Q
	P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
	x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
	Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
	Y    <- Xc2 %*% diag(1 / sqrt(colSums(Xc2^2)))  # scale columns to length 1

	x <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
	print(cor(x, midparent))
	
	out <- list(midp = midparent, child = x)
	return(out)
}

calculate_s <- function(dad_phen, mum_phen, par_off_correlation) {
	phens <- generate_phens(dad_phen, mum_phen, par_off_correlation)
	delt_mu <-  mean(phens$child) - mean(phens$midp)
	regression <- least_squares_func(phens$child, phens$midp)
	h2 <- regression$estimate
	S <- delt_mu / h2
	out <- list(S = S, h2 = h2, delt_mu = delt_mu)
	return(out)
}

params <- expand.grid(
	corr = c(0.001, 0.01, 0.1, 0.25, 0.5, 0.99),
	S = NA, 
	h2 = NA, 
	delt_mu = NA
	)

set.seed(2)
dp <- rnorm(1000)
mp <- rnorm(1000)
res <- map_dfr(split(params, 1:nrow(params)), function(x) {
	out <- calculate_s(dp, mp, x$corr)
	x$S = out$S
	x$h2 = out$h2
	x$delt_mu = out$delt_mu
	return(x)
})

res


