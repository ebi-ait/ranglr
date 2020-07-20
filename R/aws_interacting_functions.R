#' Connect to EC2
#'
#' \code{ssh_ec2} uses ssh to connect to the EC2 to be able to access s3 buckets.
#'
#' The user must already have permissions to access the EC2 already set up.
#'
#' @param username string
#' @param ec2_url ec2 url, `tool.archive.data.humancellatlas.org` by default
#' @return connection ssh connection object
#' @export
ssh_ec2 <- function(username, ec2_url="tool.archive.data.humancellatlas.org") {
  user_url <- paste0(username, "@", ec2_url)
  connection <- ssh::ssh_connect(user_url)
  return(connection)
}


#' Set up hca-util
#'
#' \code{setup_hca_util} creates a virtual environment and installs hca-util
#' @export
setup_hca_util <- function(){
  reticulate::virtualenv_create(envname="ranglr_env")
  reticulate::virtualenv_install(envname="ranglr_env", packages = "hca-util",
                                 pip_options = "--upgrade")
  system(paste0(reticulate::virtualenv_root(), "/ranglr_env/bin/hca-util -h"))
  if (! "[profile hca-util]" %in% read.delim("~/.aws/config", header = F)$V1){
    message("hca-util probably needs to be configured")
    message("follow the instructions here: https://github.com/ebi-ait/hca-documentation/wiki/How-to-administrate-upload-areas-using-hca-util#configuration")
  }
}

#' List files in S3
#'
#' \code{list_s3_files} lists the s3 files in a given s3 bucket.
#'
#' Assumes you have installed and configured the \code{setup_hca_util} command.
#' Can optionally provide the path to where you have installed hca-util
#'
#' @param s3_uuid the url of the s3 upload area.
#' @param hca_util_path the path to the hca-util installation
#' @return a tibble of files in bucket
#' @export
# TODO: write code to ensure it doesn't error or at least hide the error message
list_s3_files <- function(s3_uuid,
                          hca_util_path = paste0(reticulate::virtualenv_root(),
                                                 "/ranglr_env/bin/hca-util")){
  tryCatch({
    this_select_command <- paste0(hca_util_path, " select ", s3_uuid)
    this_list_command <- paste0(hca_util_path, " list")
    out_select <- system(command = this_select_command)
    out_list <- system(command = this_list_command, intern = TRUE)
    filename_df <- out_list %>%
      tibble::enframe() %>%
      tidyr::separate(col = value, sep = "/",
                      into = c("uuid", "file_name")) %>%
      dplyr::filter(!is.na(file_name)) %>%
      dplyr::select(file_name)
    filename_df <- filename_df[-1,]
    return(filename_df)
  },
  error=function(cond){
    message("s3 probably empty, original error message:")
    message(cond)
    return(NA)
  }
  )
}

#' Create s3 upload area
#'
#' \code{create_s3} uses the hca-util tool to create an s3 upload area for a
#' particular project.
#'
#' Assumes you have installed and configured the \code{setup_hca_util} command.
#' Can optionally provide the path to where you have installed hca-util
#'
#' @param project_shorthand the `shortname of the dataset e.g. my-cool-project
#' @param hca_util_path the path to installed hca-util
#' @param permissions allowed actions (permissions) on new area. u for upload, x
#'  for delete and d for download. Default is ux
#' @param profile specify name of admin profile, otherwise uses default
#' @return the uuid of the s3 bucket
#' @export
create_s3 <- function(project_shorthand,
                      hca_util_path = paste0(reticulate::virtualenv_root(),
                                             "/ranglr_env/bin/hca-util"),
                      permissions = NA, profile = NA){

  this_command <- paste0(hca_util_path, " create ", project_shorthand)
  if(!is.na(permissions)){
    this_command <- paste0(this_command, " -p ", permissions)
  }
  if(!is.na(profile)) {
    this_command <- paste0(this_command, " --profile ", profile)
  }
  output = system(this_command, intern = TRUE)
  print(output)
  s3_bucket_url <- stringr::str_extract(output, pattern=".{8}-.{4}-.{4}-.{4}-.{12}")
  return(s3_bucket_url)
}

#' Sync files between hca-util and ingest upload areas
#'
#' \code(sync_s3) copies files from an hca-util upload area into an ingest
#' submission upload area
#'
#' @param source_s3 the uuid of the hca-util upload area to copy from
#' @param target_s3 the full address of the ingest submission upload area
#' @param hca_util_path the path to installed hca-util
#' @param profile specify name of admin profile, otherwise uses default
#' @export
sync_s3 <- function(source_s3, target_s3, profile = NA,
                    hca_util_path = paste0(reticulate::virtualenv_root(),
                                           "/ranglr_env/bin/hca-util")){
  this_select_command <- paste0(hca_util_path, " select ", source_s3)
  this_sync_command <- paste0(hca_util_path, " sync ", target_s3)
  if(!is.na(profile)) {
    this_sync_command <- paste0(this_sync_command, " --profile ", profile)
  }
  system(this_select_command)
  system(this_sync_command)
}

#' List upload areas
#'
#' \code(list_upload_areas) lists all the areas in the hca-util upload area
#' space.
#'
#' @param hca_util_path the path to installed hca-util
#' @param profile specify name of admin profile, otherwise uses default
#' @export
list_upload_areas <- function(hca_util_path = paste0(reticulate::virtualenv_root(),
                                                      "/ranglr_env/bin/hca-util"),
                              profile = NA){
  this_command <- paste0(hca_util_path, " list -b")
  if(!is.na(profile)){
    this_command <- paste0(this_command, " --profile ", profile)
  }
  system(this_command)
}

