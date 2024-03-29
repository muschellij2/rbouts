% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basicVariables.R
\name{INA_B30}
\alias{INA_B30}
\title{Summarises Inactivity (INA, <1.5METs) bout of 30 min without breaks of LPA30s+. Activities such as lying, sitting, standing, moving slowly.
Criteria: 30-minute-bout with >90\% of inactivity, treashold ENMO <40mg, but can't include breaks of LPA30s+.}
\usage{
INA_B30(df, limSup = 40/1000)
}
\arguments{
\item{df}{Dataframe of epochs with two columns: datetime and ENMO}
}
\value{
a list with a summary of the periods matching the criteria
}
\description{
Summarises Inactivity (INA, <1.5METs) bout of 30 min without breaks of LPA30s+. Activities such as lying, sitting, standing, moving slowly.
Criteria: 30-minute-bout with >90\% of inactivity, treashold ENMO <40mg, but can't include breaks of LPA30s+.
}
