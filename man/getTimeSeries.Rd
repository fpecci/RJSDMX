% Generated by roxygen2 (4.0.1): do not edit by hand
\name{getTimeSeries}
\alias{addProvider}
\alias{getCodes}
\alias{getDSDIdentifier}
\alias{getDimensions}
\alias{getFlows}
\alias{getProviders}
\alias{getSDMX}
\alias{getTimeSeries}
\alias{sdmxHelp}
\title{RJSDMX functions}
\usage{
getTimeSeries(provider, id, start = "", end = "")

getFlows(provider, pattern = "")

getDSDIdentifier(provider, dataflow)

addProvider(name, agency, endpoint, needsCredentials = F)

getDimensions(provider, dataflow)

getCodes(provider, flow, dimension)

getSDMX(provider, id, start = "", end = "")

getProviders()

sdmxHelp()
}
\arguments{
\item{provider}{provider ID: "ECB", "OECD", "EUROSTAT", "ISTAT", "IMF".}

\item{dataflow}{a dataflow, e.g. "nama_nace64_c" .}

\item{name}{name of new SDMX provider}

\item{agency}{name of agency.}

\item{endpoint}{endpoint location.}

\item{needsCredentials}{boolean if the provider needs credentials.}

\item{id}{a time series id.}

\item{start}{start period of the time series.}

\item{end}{end period of time series.}
}
\description{
Various functions to use Java library \code{SDMX.jar}
}
\details{
\itemize{
\item \code{getFlows} Get the list of dataflows of this provider.
\item \code{getDSDIdentifier} Get the key family of the given dataflow (after retrieving the key family).
\item \code{addProvider} Add a new sdmx provider.
\item \code{getDimensions} Get dimensions of the given dataflow (after retrieving the key family).
\item \code{getCodes} Get the codes of the given dataflow and dimension (after retrieving the key family).
\item \code{getTimeSeries} Get the time series matching the parameters.
\item \code{getSDMX} Get the time series matching the parameters.
\item \code{getProviders} Get the list of available data providers.
\item \code{sdmxHelp} Create client.
}
}
\examples{
getProviders()

getFlows("EUROSTAT")
getDSDIdentifier("EUROSTAT", "nama_nace64_c")
getDimensions("EUROSTAT", "nama_nace64_c")
ans=getTimeSeries("EUROSTAT", "nama_nace64_c.A.MIO_NAC.*.B1G.AT", , start = "1970")
plot(ans[[1]])

getFlows("OECD")
getDimensions("OECD", "REFSERIES")
ans=getTimeSeries("OECD", "REFSERIES.ITA.PPPGDP.A")
plot(ans[[1]])

sdmxHelp()
}
\author{
Attilio Mattiocco, Diana Nicoletti, Banca d'Italia
}
\seealso{
\code{\link{RJSDMX}}
}
\keyword{package}

