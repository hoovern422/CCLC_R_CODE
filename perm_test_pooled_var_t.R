###########Description###########
#This code performs a permutation test based on the the t-statistic from a pooled sample variance t-test and produces a histogram of the observed test statistic, 
#along with a superimposed density curve of the theoretical t-distribution for the appropriate degrees of freedom.

###########Function Parameters###########
# Required*** (string) file_path_and_name -> The path and name of the file you wish to read in
# Required*** (string) score_col_name -> Name of the column that holds the empirical data
# Required*** (string) group_col_name -> Name of the column that holds the categorical data
# Optional*** (Int) nreps -> The number of iterations to be performed in the permutation test. If this parameter isn't set, the default will be 5000 iterations.

###########Function Output###########
#A printed summary of the permutation test and a histogram of the observed test statistic.

perm_test_pooled_var_t <- function(file_path_and_name, score_col_name, group_col_name, nreps){
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
  
  init_model <- t.test(data_scores[1:n.i[1]], data_scores[(n.i[1] + 1):(n.i[1] + n.i[2])], var.equal = TRUE)
  init_t_val <- init_model$statistic
  init_p_val <- init_model$p.value
  
  cat("The intial t value is: ",init_t_val, "\n")
  cat("This has an associated probability of: ", init_p_val, "\n")
  
  cores=detectCores()
  cl <- makeCluster(cores[1])
  registerDoParallel(cl)
  
  t_vals<-foreach(i = 1:nreps, .combine='c') %dopar% {
    newScore <- sample(data_scores)
    obs_t <-(t.test(newScore[1:n.i[1]], newScore[(n.i[1] + 1):(n.i[1] + n.i[2])], var.equal = TRUE))$statistic
  }
  
  if (init_t_val >= 0){
    pvalue_left <- (sum(t_vals <= -init_t_val)/nreps)
    pvalue_right <- (sum(t_vals >= init_t_val)/nreps)
    pvalue <- pvalue_left + pvalue_right
  }
  if (init_t_val < 0){
    pvalue_left <- (sum(t_vals <= init_t_val)/nreps)
    pvalue_right <- (sum(t_vals >= -init_t_val)/nreps)
    pvalue <- pvalue_left + pvalue_right
  }
  
  cat("\nThe calculated value of p from the observed t values is: ",pvalue, "\n")
  
  par(mfrow = c(2,1))
  hist(t_vals, breaks = 50, main = "Histogram of (Pooled Sample Variance) observed t-statistic on Randomized Samples",
       xlab = "t value", probability = TRUE, col = "green", border = 1,
       xlim = c(-7,7), ylim = c(0,1))
  
  if (init_t_val >= 0){
    legend("topright", paste("obtained.t = ", round(init_t_val, digits = 8)), col=1,  cex = 0.8)
    legend("right",paste("pvalue_right = ",round(pvalue_right, digits = 8)))
    arrows( 5.5, 0.8,init_t_val,0, length = .125)
    legend("topleft", paste("-obtained.t = ", round(-init_t_val, digits = 8)), col=1,  cex = 0.8)
    legend("left",paste("pvalue_left = ",round(pvalue_left, digits = 8)))
    arrows( -5.5, 0.8,-init_t_val,0, length = .125)
    legend("top",paste("two-sided pvalue = ",round(pvalue, digits = 8)))
  }
  if (init_t_val < 0){
    legend("topleft", paste("obtained.t = ", round(init_t_val, digits = 8)), col=1,  cex = 0.8)
    legend("left",paste("pvalue_left = ",round(pvalue_left, digits = 8)))
    arrows( -5.5, 0.8,init_t_val,0, length = .125)
    legend("topright", paste("-obtained.t = ", round(-init_t_val, digits = 8)), col=1,  cex = 0.8)
    legend("right",paste("pvalue_right = ",round(pvalue_right, digits = 8)))
    arrows( 5.5, 0.8,-init_t_val,0, length = .125)
    legend("top",paste("two-sided pvalue = ",round(pvalue, digits = 8)))
  }
  
  t <- seq(-7, 7,.01)
  dens <- dt(t, init_model[[2]]) 
  
  par(new = T)
  plot(t,dens, col = "red", type = "l", xlim = c(-7,7), ylim = c(0,1), xlab = "", ylab = "")
  stopCluster(cl)
}
