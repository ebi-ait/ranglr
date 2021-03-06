% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aws_interacting_functions.R
\name{list_upload_areas}
\alias{list_upload_areas}
\title{List upload areas}
\usage{
list_upload_areas(
  hca_util_path = paste0(reticulate::virtualenv_root(), "/ranglr_env/bin/hca-util"),
  profile = NA
)
}
\arguments{
\item{hca_util_path}{the path to installed hca-util}

\item{profile}{specify name of admin profile, otherwise uses default}
}
\description{
\code{list_upload_areas} lists all the areas in the hca-util upload area
space.
}
