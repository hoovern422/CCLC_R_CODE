###########Description###########
#This function performs Levene's test and prints a summary of the test.

###########Function Parameters###########
# Required*** (String) file_path_and_name -> The path and name of the file you wish to read in
# Required*** (String)score_col_name -> Name of the column that holds the empirical data
# Required*** (String) group_col_name -> Name of the column that holds the categorical data

###########Function Output###########
#A printed summary of Levene's test

levene_test<- function(file_path_and_name, score_col_name, group_col_name){
  #Libraries
  library(car)
  
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
  
  #print the test results
  print(summary(leveneTest(data_scores ~ as.factor(data_groups))))
}
