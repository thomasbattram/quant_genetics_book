#
#
#


pkgs <- c("tidyverse")
lapply(pkgs, require, character.only = TRUE)

allele_freq <- 0.5 # for all 4 alleles

gen_geno_freq <- function(a1_freq, a2_freq) {
	a1a1 <- a1_freq * a1_freq
	a1a2 <- 2 * a1_freq * a2_freq
	a2a2 <- a2_freq * a2_freq
	out <- c(a1a1, a1a2, a2a2)
	return(out)
}

gen_freq <- gen_geno_freq(allele_freq, allele_freq)
all_gen_freq <- gen_freq^3 
matrix(rep(all_gen_freq, 3), nrow = 3, ncol = 3)


40.225 - (-9.7) - (-9.7) - 56.9

mu_g <- 56.8875

G_Bm <- 47.2
G_Bt <- 66.7
G_Um <- 49.3
G_Ut <- 64.4

a_Bm <- -9.7375
a_Bt <- 9.7375
a_Um <- -7.55
a_Ut <- 7.55

d_Bmm <- 2.8125
d_Bmt <- -2.8125
d_Btt <- 2.8125

d_Umm <- 1.9625
d_Umt <- -1.9625
d_Utt <- 1.9625

G_BmUm <- (0.5 * 0.5 * 18) + (0.5 * 0.5 * 54.6) + (0.25 * 40.9 ) + (0.25 * 47.6)
G_BmUt <- 54.025
G_BtUm <- 58.400
G_BtUt <- 74.850

G_BmmUm <- (0.5 * 18) + (0.5 * 40.9)
G_BmUmm <- (0.5 * 18) + (0.5 * 54.6)

aa_BmUm <- G_BmUm - mu_g - a_Bm - a_Um

ad_BmmUm <- G_BmmUm - mu_g - a_Bm - a_Bm - a_Um - d_Bmm - aa_BmUm - aa_BmUm
ad_BmUmm <- G_BmUmm - mu_g - a_Bm -  a_Um - a_Um - d_Umm - aa_BmUm - aa_BmUm

G_BmmUmm <- 1 * 18
dd_BmmUmm <- G_BmmUmm - 
             mu_g - 
			 a_Bm - a_Bm - a_Um - a_Um -
			 d_Bmm - d_Umm - 
			 aa_BmUm - aa_BmUm - aa_BmUm - aa_BmUm -
			 ad_BmmUm - ad_BmmUm - ad_BmUmm - ad_BmUmm


# variance components
# ASK GIB! --> DON'T UNDERSTAND VARIANCE OF DOMINANCE COMPONENTs

# 1 			 

p1 <- 0.5
p2 <- 0.5
a1 <- 27
k1 <- 0.33

sigd1 <- (2*p1*p2*a1*k1)^2

a2 <- 6
k2 <- -2.17

sigd2 <- (2*p1*p2*a2*k2)^2

a3 <- 21.6
k3 <- 0.06 

sigd3 <- (2*p1*p2*a3*k3)^2


a_avg <- mean(c(a1, a2, a3))
k_avg <- mean(c(k1, k2, k3))
sig_avg <- (2*p1*p2*a_avg*k_avg)^2

