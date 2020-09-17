########################################################################################################################################################
# DISCLAIMER: This code was written by David C. Howell at the University of Vermont. I've simply added a few extensions and/or reapplied it to fit our situation
# Author: Dr. David C. Howell
# Date:  06/28/2015
# Title: Randomization Test with Multiple Groups
# Type: R code
# Source: https://www.uvm.edu/~statdhtx/StatPages/R/RandomizationTestsWithR/RandomOneway/RandomOneway.html
#########################################################################################################################################################

###########Description###########
#This code performs a permutation test based on the the H-statistic (which follows a theoretical Chi distribution for k-1 degrees of freedom)
#from a Kruskal-Wallis test and produces a histogram of the observed test statistic, along with a superimposed density curve of the theoretical Chi-distribution
#for the appropriate degrees of freedom.

###########Function Parameters###########
# Required*** (string) file_path_and_name -> The path and name of the file you wish to read in
# Required*** (string) score_col_name -> Name of the column that holds the empirical data
# Required*** (string) group_col_name -> Name of the column that holds the categorical data
# Optional*** (Int) nreps -> The number of iterations to be performed in the permutation test. If this parameter isn't set, the default will be 5000 iterations.

###########Function Output###########
#A printed summary of the permutation test and a histogram of the observed test statistic.

perm_test_kw_Chi <- function(file_path_and_name, score_col_name, group_col_name, nreps){
  #Libraries
  library(foreach)
  library(doParallel)
  library(iterators)
  
  #Setting the seed for reproducibility
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
  
  # THE REST OF THE CODE PERFORMS THE PERMUTATION TEST
  
  data_groups <- as.factor(data_groups)
  if (missing(nreps) || is.null(nreps)){
    nreps <- 5000
  }
  N <- length(data_scores)
  n.i <- as.vector(table(data_groups))   # Create vector of sample sizes
  k <- length(n.i)
  
  
  model <- kruskal.test(data_scores, data_groups)
  obt.Chi <- model$statistic    # Our obtained Chi  statistic
  obt.p <- model$p.value           # Our obtained p-value
  
  cat("The obtained value of Chi from the Kruskal-Wallis test is ",obt.Chi, "\n")
  cat("This has an associated probability of ", obt.p,  "\n")
  
  cores=detectCores()
  cl <- makeCluster(cores[1])
  registerDoParallel(cl)
  
  
  Chi_stats<-foreach(i = 1:nreps, .combine='c') %dopar% {
    newScore <- sample(data_scores)
    newModel <- kruskal.test(newScore, data_groups)
    samp.Chi <- newModel$statistic
  }
  
  pvalue <- (sum(Chi_stats >= obt.Chi)/nreps)
  cat("\nThe calculated value of p from randomized samples is ",pvalue, "\n")
  
  par(mfrow = c(2,1))
  hist(Chi_stats, breaks = 50, main = "Histogram of Chi on Randomized Samples",
       xlab = "Chi value", probability = TRUE, col = "green", border = 1,
       xlim = c(0,15), ylim = c(0,1))
  legend("topright", paste("obtained.Chi = ", round(obt.Chi, digits = 8)), col=1,  cex = 0.8)
  legend("right",paste("p-value = ",round(pvalue, digits = 8)))
  arrows( 5.5, 0.8,obt.Chi,0, length = .125)
  
  Chi <- seq(0, 15,.01)
  dens <- dchisq(Chi,model[[2]])
  par(new = T)
  plot(Chi,dens, col = "red", type = "l", xlim = c(0,15), ylim = c(0,1), xlab = "", ylab = "")
  stopCluster(cl)
}