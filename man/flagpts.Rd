% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interactive.R
\name{flagpts}
\alias{flagpts}
\title{flagpts
Flag outliers interactively}
\usage{
flagpts(d, R = 10)
}
\arguments{
\item{d}{a DT returned by argos_prepare()}

\item{R}{point radius}
}
\description{
flagpts
Flag outliers interactively
}
\examples{
x <- dbq(q = "Select * from ARGOS.2018_PESA where tagID ='52753' ") |> argos_prepare()
flagpts(x)
}
