


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


