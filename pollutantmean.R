pollutantmean <- function(directory, pollutant, id = 1:332) {
    # create a list of files
    files_list <- list.files(directory, full.names=TRUE)   
    # build a list where each element is a dataframe corresponding to the locations of interest (id)
    list_of_filedata<-lapply(files_list[id], read.csv)
    # create a single data frome from the list
    all_data <- do.call(rbind, list_of_filedata)
    # get the mean of the pollutant
    mean(all_data[ ,pollutant], na.rm=TRUE)
}