complete <- function(directory, id = 1:332) {
    # create a list of files
    files_list <- list.files(directory, full.names=TRUE)   
    # build a list where each element is a dataframe corresponding to the locations of interest (id)
    list_of_filedata<-lapply(files_list[id], read.csv)
    # iterate over the list of dataframes and build vector of complete cases for each dataframe
    complete_cases_list<-lapply(list_of_filedata, complete.cases)
    # count the number of complete cases in each vector
    counts<-lapply(complete_cases_list, sum)
    # convert the list of lists into a vector
    nobs<-c(do.call("cbind",counts)) 
    # convert vectors into a data frame
    data.frame(id, nobs)
}