% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_reference_index.R
\name{write_section}
\alias{write_section}
\title{Write a section of documentation}
\usage{
write_section(dir_in, yaml, index)
}
\arguments{
\item{dir_in}{Character. Path to root of Stata package directory.}

\item{yaml}{YAML file loaded by \code{yaml::yaml.load_file()}}

\item{index}{Index in the YAML of the object to write.}
}
\description{
Sections contain one or more of the following
\itemize{
\item Title (required)
\item Description (optional)
\item Sub-section (optional). See \code{write_sub_section} for more details.
\item Contents (required, if not part of sub-section)
}
}
