### PIPELINE FROM UKBIOBANK
load("G:/phenotype estimation/F__phenotype estimation_trybd.Rdata")
x1807 <- irnt(trybd[,"f.1807.0.0"])
head(x1807)
irnt <- function(cts_variable) {
  set.seed(1234) # This is the same as was used by PHESANT - for checking.
  n_cts <- length(which(!is.na(cts_variable)))
  quantile_cts <- (rank(cts_variable, na.last = "keep", ties.method = "random") - 0.5) / n_cts
  # use the above to check, but also use frank for the real thing
  cts_IRNT <- qnorm(quantile_cts)	
  return(cts_IRNT)
}

### regular pattern
# > a <- "x2190_0_0"
# > gsub("^x", "", a)
# [1] "2190_0_0"
# > varx <- gsub("^x", "", a)
# > varx <- gsub("_[0-9]+$", "", varx)
# > varx
# [1] "2190_0"
# > varxShort <- gsub("^x", "", a)
# > varxShort
# [1] "2190_0_0"
# > varxShort <- gsub("_[0-9]+_[0-9]+$", "", varxShort)
# > varxShort
# [1] "2190"



