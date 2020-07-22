#' Get read index
#'
#' \code{get_read_index} attempts to recognise the read index of a fastq file in
#' a string and returns the string required by hca metadata standard, i.e. one of
#' index1, index2, read1, read2.
#'
#' Currently recognised patterns are: R1, R2, I1, I2
#'
#' @param file_name the string filename
#' @export
get_read_index <- function(file_name){
  read_string <- str_extract(file_name, "(R1|I1|R2|I2)")
  if(is.na(read_string)){
    stop("could not recognise read index identifier.")
  }
  if(read_string == "R1"){
    return("read1")
  } else if (read_string == "I1"){
    return("index1")
  } else if (read_string == "I2"){
    return("index2")
  } else if (read_string == "R2"){
    return("read2")
  } else {
    stop("could not recognise read index identifier.")
  }
}

#' Get lane index
#'
#' \code{get_lane_index} attempts to recognise the lane index of a fastq file by
#'  recognising the lane index of the file.
#'
#'  The regex used to recognise the lane is: \code{[-_]L\\d+}
#'
#' @param file_name the string filename
#' @export
get_lane_index <- function(file_name){
  lane_string <- str_extract(file_name, pattern = regex("[-_]L\\d+"))
  if(is.na(lane_string)){
    stop("Could not recognise lane in file name")
  } else{
    return(str_sub(lane_string, -1))
  }
}

#' Get file format
#'
#' \code{get_file_format} attempts to recognise the file format of the file by
#' looking for common fastq patterns.
#'
#' currently searches for \code{fq.gz | fastq.gz}
#'
#' @param file_name the string filename
#' @export
get_file_format <- function(file_name){
  format_string <- str_extract(file_name, "(fq\\.gz|fastq\\.gz)")
  if(is.na(format_string)){
    stop("could not recognise file format.")
  }
  else {
    return("fastq.gz")
  }
}
