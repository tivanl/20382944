read_all_rds <- function (folder, pattern_is = "*.rds"){

    # We want to avoid error messages
    silent_csv <- function(FILE){
        quiet_RDS <- purrr::quietly(readRDS)
        Data <- quiet_RDS(FILE)
        Data$result
    }
    silent_rbind <- function(x, y){
        quiet_rbind <- purrr::quietly(rbind)
        Data <- quiet_rbind(x, y)
        Data$result
    }
    #create a vector containing all the file names
    library(readr)
    files <- as.vector(list.files(folder, pattern= pattern_is, all.files=FALSE, full.names=FALSE))
    #get the length of the vector which indicates the amount of files
    x = length(files)
    #create the dataframe that will be used and given as output
    # it reads in the last data file from the folder
    dataframe <- silent_RDS(paste0(folder, files[x]))
    #subtract one to go to the second to last data folder in the file
    x = x -1
    # this while-loop adds the new data file to the data frame by rows
    while(x > 0){
        holder <- silent_RDS(paste0(folder, files[x]))
        dataframe <- silent_rbind(dataframe, holder)
        x = x - 1
    }
    dataframe
}