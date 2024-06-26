% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sx_centreline_manual.R
\name{sx_centreline_manual}
\alias{sx_centreline_manual}
\alias{sx_centerline_manual}
\title{Creates or obtains the centreline of the channel object}
\usage{
sx_centreline_manual(channel)

sx_centerline_manual(channel)
}
\arguments{
\item{channel}{channel object}

\item{centerline}{shapefile containing the centerline linestring geometries
If not supplied, the function will delineate a centerline based on the channel
object}
}
\description{
\strong{Manual version}: currently using centerline R package.
}
\details{
Creates or obtains the centreline of the channel object. \code{sx_centerline()}
is an alias for \code{sx_centreline()}.
}
