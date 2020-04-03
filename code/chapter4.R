# Chapter 4

pkgs <- c("tidyverse", "HardyWeinberg")
lapply(pkgs, require, character.only = TRUE)

# ---------------------------------------- 
# chi-squared test -- tidy code!!!
# ----------------------------------------

b1b1 <- 300
b1b2 <- 500
b2b2 <- 200

b1 <- 300 + 0.5 * 500
b2 <- 200 + 0.5 * 500

exp_b1b1 <- ((b1)^2 / 1000)
exp_b1b2 <- (2 * b1 * b2) / 1000
exp_b2b2 <- (b2^2) / 1000

chi2_part1 <- ((b1b1 - exp_b1b1)^2) / exp_b1b1
chi2_part2 <- ((b1b2 - exp_b1b2)^2) / exp_b1b2
chi2_part3 <- ((b2b2 - exp_b2b2)^2) / exp_b2b2

chi2 <- sum(c(chi2_part1, chi2_part2, chi2_part3))

# from package
x <- c(b1b1, b1b2, b2b2)
HW.test <- HWChisq(x,verbose=TRUE, cc=0)

# ---------------------------------------- 
# lrt -- tidy code!!
# ----------------------------------------

lrt_hwe_test <- function(hom1_n, het_n, hom2_n) {
	
	b1 <- hom1_n + 0.5 * het_n
	b2 <- hom2_n + 0.5 * het_n
	exp_hom1_n <- ((b1)^2 / 1000)
	exp_het_n <- (2 * b1 * b2) / 1000
	exp_hom2_n <- (b2^2) / 1000

	G <- -2 * ((hom1_n * log(exp_hom1_n / hom1_n)) +
			het_n * log(exp_het_n / het_n) + 
			hom2_n * log(exp_hom2_n / hom2_n))
	p <- pchisq(G, df=1, lower.tail=FALSE)
	out <- list(G = G, p_val = p)
	return(out)
}

lrt_hwe_test(b1b1, b1b2, b2b2)
HW.test2 <- HWExact(x,verbose=TRUE)
# similar but not the same!




