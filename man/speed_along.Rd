% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{speed_along}
\alias{speed_along}
\title{speed along points
compute speed along locations by reference}
\usage{
speed_along(
  x,
  .lat = "latitude",
  .lon = "longitude",
  .dt = "locationDate",
  .grp = "tagID",
  .measure = "haversine",
  clean = TRUE,
  ...
)
}
\arguments{
\item{.lat}{lat (default to ARGOS tables nomenclature)}

\item{.lon}{lon (default to ARGOS tables nomenclature)}

\item{.dt}{locationDate (default to ARGOS tables nomenclature)}

\item{.grp}{tagID (default to ARGOS tables nomenclature)}

\item{.measure}{measure pass to geodist}

\item{clean}{keep .dst and .deltaT?, default to FALSE}
}
\value{
a new colum in x: speed_kmh
Compute speed on a DT that has lat, lon and datetime
}
\description{
speed along points
compute speed along locations by reference
}
\examples{
#' d = dbq(q = "Select * from ARGOS.2018_PESA where tagID ='52753' ")
z = argos_prepare(d)
speed_along(z)
}
