corr <- function(directory, threshold = 0) {
    # create a list of files
    files_list <- list.files(directory, full.names=TRUE)   
    # build a list where each element is a dataframe corresponding to the locations of interest (id)
    list_of_filedata<-lapply(files_list, read.csv)
    # iterate over the list of dataframes and build vector of complete cases for each dataframe
    complete_cases_list<-lapply(list_of_filedata, complete.cases)
    # count the number of complete cases in each vector
    counts<-lapply(complete_cases_list, sum)
    # convert the list of lists into a vector
    nobs<-c(do.call("cbind",counts)) 
    # convert nsource("corr.R")obs into a logical vector with comparison to threshold
    met_threshold <- nobs>threshold
    # count how many monitors met the threshold
    count_met_theshold <- sum(met_threshold)
    if (count_met_theshold ==0) { # if no monitors met the threshold, return an empty numeric vector
        vec <- vector(mode="numeric")
    }
    else { # there is at least one monitor that met the threshold
        # filter list to only those dataframes that met threshold
        filedata_above_threshold <- list_of_filedata[met_threshold]
        # drop Date and ID columns from each data frame
        filedata_above_threshold <- lapply(filedata_above_threshold, subset, select=c(sulfate,nitrate))
        # correlate the nitrate and sulfate values for each monitor
        correlation_list <- lapply(filedata_above_threshold, cor, use="complete.obs")
        # result is a 2x2 matrix with the value I need in 1:2 or 2:1
        # strip the data frames to just the correlation value
        # first select only the sulfate column
        correlation_list <- lapply(correlation_list, subset, select=sulfate)
        # then select only the second row
        correlation_list <- lapply(correlation_list, subset, subset=c(FALSE, TRUE))
        # now create a single vector of correlations
        correlation_list <- c(do.call("cbind",correlation_list))
    }
}