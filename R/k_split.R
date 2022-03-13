k_split <- function(response,fact,training){
  set.seed(1234)
  # If the factor has 2 level it returns levels of the factor.
  # Else it uses factorMerger to finding group names.
  if (length(levels(training[, fact])) == 2) {
    return(levels(training[, fact]))
  } else
    merged <- factorMerger::mergeFactors(
      response = as.numeric(training[, response]),
      factor = training[, fact] ,
      method = "fast-adaptive",
      family = "gaussian"
    )

  # levels counts of the factor
  nlevel <- length(levels(training[, fact]))
  # "Splits factor levels into non-overlapping clusters based on a factorMerger object."
  # default parameters are stat = "GIC", value = 2.
  # "By default, cutree returns factor partition corresponding to
  # the optimal GIC model (with the lowest GIC)."
  # It returns a factor vector.
  cutedtree <- cutTree(merged)

  # cutedtree gives a vector.
  # kgroups contains the levels names of the finding with factorMerger.
  # Example :  "(Prag)(Wola)(Ursy)" "(Bemw)(Blny)(Urss)" "(Zlbr)"  "(Mktw)(Ocht)"  "(Srdm)"
  # (Level names coded with numbers(1,2,3,...) in sub_learner function)
  kgroups <- levels(cutedtree)
  # level count of the k groups.
  k <- length(kgroups)

  # Factor merger gives group names in parentheses and it use abbreviations.
  # Until the end of the for loop these names are matched with string operations.
  # Inside the group_names object, the levels names generated as a result of the split are listed.
  d <- str_split(merged$map$recoded, boundary("word"))
  maps <- merged$map
  maps$recoded <- unlist(d)
  splited <- str_split(kgroups, boundary("word"))
  group_names <- NULL
  for (j in 1:k) {
    matchedind <- match(unlist(splited[j]), maps$recoded)
    group_names[j] <- list(maps$original[matchedind])

  }

  return(group_names)
}
