###########Description###########
#This function initializes all of the other functions I've written in the current environment.

###########Function Parameters###########
#Required*** (string) folder_path -> path to the folder where all of the scripts are stored.

###########Function Output###########
# Creates instances of function objects to be used in the global environment

initialize_functions<-function(folder_path){
  file_path1 <- paste(folder_path, "/perm_test_standard_F.R", sep="")
  source(file = as.character(file_path1))

  file_path2 <- paste(folder_path, "/perm_test_satterthwaite_F.R", sep ="") 
  source(file = as.character(file_path2))

  file_path3 <- paste(folder_path, "/perm_test_welch_t.R", sep ="") 
  source(file = as.character(file_path3))

  file_path4 <- paste(folder_path, "/perm_test_pooled_var_t.R", sep ="") 
  source(file = as.character(file_path4))

  file_path5 <- paste(folder_path, "/perm_test_kw_Chi.R", sep ="") 
  source(file = as.character(file_path5))

  file_path6 <- paste(folder_path, "/lmPerm_aovp.R", sep ="") 
  source(file = as.character(file_path6))

  file_path7 <- paste(folder_path, "/levene_test.R", sep ="") 
  source(file = as.character(file_path7))

}