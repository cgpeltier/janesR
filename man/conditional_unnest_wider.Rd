% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conditional_unnest_wider.R
\name{conditional_unnest_wider}
\alias{conditional_unnest_wider}
\title{conditional_unnest_wider}
\usage{
conditional_unnest_wider(data, variable)
}
\arguments{
\item{data}{Country filter for equipment}

\item{variable}{Query filter for equipment}
}
\value{
Unnests_wider if variable is in dataset, doesn't if not.
}
\description{
conditionally unnests_wider. Helper function.
}
