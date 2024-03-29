% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connectOverDistanceV2.R
\name{cercaElastico}
\alias{cercaElastico}
\title{Connect intervals that are close enough
Generates a smaller dataframe of connected intervals using a data frame of given intervals and a distance that allows tow of them to be connected}
\usage{
cercaElastico(to1, from2, distanciaNoche = dhours(4),
  distanciaDia = dminutes(30), earlyMorning = c(1, 9))
}
\arguments{
\item{distanciaNoche}{distance between ttwo intervals to allo the connection of both in just one.}

\item{distanciaDia}{distance between ttwo intervals to allo the connection of both in just one.}

\item{interval}{Initial dataframe of intervals to be connected}
}
\value{
A dataframe of intervals, having less or equal rows tan the original
}
\description{
Connect intervals that are close enough
Generates a smaller dataframe of connected intervals using a data frame of given intervals and a distance that allows tow of them to be connected
}
