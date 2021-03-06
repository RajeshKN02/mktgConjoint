% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ms.prediction.R
\name{utilities.of.profiles}
\alias{utilities.of.profiles}
\title{utilities.of.profiles}
\usage{
utilities.of.profiles(profiles, ratings, bundles)
}
\arguments{
\item{profiles}{a data frame with the description of competitors' existing product profiles}

\item{ratings}{a data frame with all clients' ratings}

\item{bundles}{a data frame with all product profiles rated by clients}
}
\value{
utilities.profiles     a matrix with the prediction of utilities
}
\description{
This function computes the utility of each profile (given rivals' profiles and new profiles
) for each consumer.  The data frame profiles is the
data of existing profiles for which we are going to compute the market shares
is a tale of product profiles in the market (rows) by
attributes (columns), ratings is a data frame of clients (rows) by bundles
rated (colums), bundles is the data frame of bundles (rows) by
attributes (colums) rated by clients.
}
\examples{
data(osc)
names(osc)
#this function is called by market share function.
osc.uop <-utilities.of.profiles(osc$market.profiles, osc$ratings, osc$bundles)
head(osc.uop)
}

