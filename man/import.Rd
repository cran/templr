% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.R
\name{import}
\alias{import}
\title{Dependencies loader, supports many protocols like github:, gitlab:, ... using remotes::instal_...
Will create a local '.lib' directory to store packages installed}
\usage{
import(..., lib.loc = NULL, trace = function(...) cat(paste0(..., "\\n")))
}
\arguments{
\item{...}{dependencies/libraries/packages to load}

\item{lib.loc}{use to setup a dedicated libPath directory to install packages}

\item{trace}{display info}
}
\value{
(list of) load status of packages (TRUE/FALSE)
}
\description{
Dependencies loader, supports many protocols like github:, gitlab:, ... using remotes::instal_...
Will create a local '.lib' directory to store packages installed
}
\examples{
if(interactive()){
  import('VGAM')
}
}
