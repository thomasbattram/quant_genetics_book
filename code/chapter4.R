# Chapter 4

pkgs <- c("tidyverse", "HardyWeinberg")
lapply(pkgs, require, character.only = TRUE)

# ----------------------------------------
# testing for HWE
# ----------------------------------------

# set genotype and allele frequencies
b1b1 <- 300
b1b2 <- 500
b2b2 <- 200

# generate allele frequencies
gen_allele_freqs <- function(b1b1, b1b2, b2b2) {
	b1 <- b1b1 + 0.5 * b1b2
	b2 <- b2b2 + 0.5 * b1b2
	out <- list(b1 = b1, b2 = b2)
	return(out)
}

# generate expected values
gen_expected_freqs <- function(b1, b2) {
	exp_b1b1 <- (b1^2) / 1000
	exp_b1b2 <- (2 * b1 * b2) / 1000
	exp_b2b2 <- (b2^2) / 1000	
	out <- list(b1b1 = exp_b1b1, b1b2 = exp_b1b2, b2b2 = exp_b2b2)
	return(out)
}


# chi-squared test
chi_sq_test <- function(b1b1, b1b2, b2b2) {
	allele_freqs <- gen_allele_freqs(b1b1, b1b2, b2b2)
	expected <- gen_expected_freqs(allele_freqs$b1, allele_freqs$b2)

	observed <- c(b1b1, b1b2, b2b2)
	names(observed) <- c("b1b1", "b1b2", "b2b2")
	chi2_vals <- map_dbl(seq_along(observed), function(x) {
		obs <- observed[x]
		exp <- expected[[names(obs)]] 
		chi2_val <- ((obs - exp)^2) / exp
	})
	chi2 <- sum(chi2_vals)
	p <- pchisq(chi2, df=1, lower.tail=FALSE)
	out <- list(chi_sq = chi2, pval = p)
	return(out)
}

x <- chi_sq_test(b1b1, b1b2, b2b2)
package_hwe <- HWChisq(c(b1b1, b1b2, b2b2),verbose=TRUE, cc=0)

x
package_hwe
# same output!

# lrt! 
lrt_hwe_test <- function(b1b1, b1b2, b2b2) {	
	allele_freqs <- gen_allele_freqs(b1b1, b1b2, b2b2)
	expected <- gen_expected_freqs(allele_freqs$b1, allele_freqs$b2)

	G <- -2 * ((b1b1 * log(expected$b1b1 / b1b1)) +
			b1b2 * log(expected$b1b2 / b1b2) + 
			b2b2 * log(expected$b2b2 / b2b2))
	p <- pchisq(G, df=1, lower.tail=FALSE)
	out <- list(G = G, p_val = p)
	return(out)
}

lrt_hwe_test(b1b1, b1b2, b2b2)
HW.test2 <- HWLratio(c(b1b1, b1b2, b2b2),verbose=TRUE)
# same output!

# apparently what is done in plink is the exact test! (Haldane's???)
HW.test3 <- HWExact(c(b1b1, b1b2, b2b2),verbose=TRUE)
# similar p value, but slightly different! 



