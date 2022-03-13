check.packages <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <-
  c(
    "rpart",
    "stringr",
    "factorMerger",
    "MLmetrics",
    "dplyr",
    "ranger",
    "DALEX",
    "caret",
    "breakDown"
  )

check.packages(packages)
