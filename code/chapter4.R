# Chapter 4

b1b1 <- 300
b1b2 <- 500
b2b2 <- 200

b1 <- 300 + 0.5 * 500
b2 <- 200 + 0.5 * 500

exp_b1b1 <- ((b1)^2 / 1000)
exp_b1b2 <- (2 * b1 * b2) / 1000
exp_b2b2 <- (b2^2) / 1000

tab <- as.table(rbind(c(exp_b1b1, exp_b1b2, exp_b2b2), c(b1b1, b1b2, b2b2)))
dimnames(tab) <- list(data = c("expected", "observed"), 
					  genotype = c("b1b1", "b1b2", "b2b2"))
Xsq <- chisq.test(tab)

x <- c(b1b1, b1b2, b2b2)
HW.test <- HWChisq(x,verbose=TRUE)


     M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
     dimnames(M) <- list(gender = c("F", "M"),
                         party = c("Democrat","Independent", "Republican"))
     (Xsq <- chisq.test(M))  # Prints test summary

     Xsq$observed   # observed counts (same as M)
     Xsq$expected   # expected counts under the null
     Xsq$residuals  # Pearson residuals
     Xsq$stdres     # standardized residuals