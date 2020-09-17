###########Description###########
#This function executes aovp() from the lmPerm library. It performs a permutation test based on the standard F-statistic from a one-way ANOVA test and prints the result.

###########Function Parameters###########
# Required*** (string) file_path_and_name -> The path and name of the file you wish to read in
# Required*** (string) score_col_name -> Name of the column that holds the empirical data
# Required*** (string) group_col_name -> Name of the column that holds the categorical data
# Optional*** (Double) stopping_rule -> The stopping rule in aovp() is controlled by a parameter, Ca, which has a default value of 0.1. That is, the sampling stops when the estimated standard deviation falls below 0.1 of the estimated p-value. Setting this parameter overrides the default value of Ca.
# Optional*** (Int) max_iter -> The maximum number of iterations to be performed by aovp() is controlled by a parameter, maxIter, which has a default value of 5000. Setting this parameter will override the default value of maxIter.

###########Function Output###########
#A printed summary of the aovp() permutation test

lmPerm_aovp <- function(file_path_and_name, score_col_name, group_col_name, stopping_rule, max_iter){
  #libraries
  library(lmPerm)
  
  #setting the seed for reproducibility
  set.seed(1086)
  
  #parse the data
  if (grepl(".csv", file_path_and_name)){
    data <- read.csv(file = as.character(file_path_and_name))
  }
  if (grepl(".txt", file_path_and_name)){
    data <- read.delim(file = as.character(file_path_and_name), header = TRUE, sep="\t")
  }
  if (grepl(".xlsx", file_path_and_name)){
    library(readxl)
    sheet_name <- readline(prompt = "Enter the name of the sheet you would like to parse: ")
    data <- read_xlsx(as.character(file_path_and_name), sheet = as.character(sheet_name))
  }
  
  score_accesor <- paste("data$", score_col_name, sep="")
  data_scores <- eval(parse(text = score_accesor))
  
  group_accesor <- paste("data$", group_col_name, sep="")
  data_groups<- eval(parse(text = group_accesor))
  
  # Perform the permutation test and print the results
  if (!missing(stopping_rule) && !missing(max_iter)){
    result <- summary(aovp(data_scores ~ as.factor(data_groups), perm = "Exact", seqs = FALSE, maxIter = max_iter, Ca = stopping_rule))
  }
  if (!missing(stopping_rule) && missing(max_iter)){
    result <- summary(aovp(data_scores ~ as.factor(data_groups), perm = "Exact", seqs = FALSE, Ca = stopping_rule))
  }
  if (missing(stopping_rule) && !missing(max_iter)){
    result <- summary(aovp(data_scores ~ as.factor(data_groups), perm = "Exact", seqs = FALSE, maxIter = max_iter))
  }
  if (missing(stopping_rule) && missing(max_iter)){
    result <- summary(aovp(data_scores ~ as.factor(data_groups), perm = "Exact", seqs = FALSE, maxIter = 5000))
  }
  
  print(result)
}