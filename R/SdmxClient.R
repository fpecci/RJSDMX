#' RJSDMX functions
#'
#' Various functions to use Java library \code{SDMX.jar}
#'
#' \itemize{
#' \item \code{getFlows} Get the list of dataflows of this provider.
#' \item \code{getDSDIdentifier} Get the key family of the given dataflow (after retrieving the key family).
#' \item \code{addProvider} Add a new sdmx provider.
#' \item \code{getDimensions} Get dimensions of the given dataflow (after retrieving the key family).
#' \item \code{getCodes} Get the codes of the given dataflow and dimension (after retrieving the key family).
#' \item \code{getTimeSeries} Get the time series matching the parameters.
#' \item \code{getSDMX} Get the time series matching the parameters.
#' \item \code{getProviders} Get the list of available data providers.
#' \item \code{sdmxHelp} Create client.
#' }
#'
#' @param provider provider ID: "ECB", "OECD", "EUROSTAT", "ISTAT", "IMF".
#' @param dataflow a dataflow, e.g. "nama_nace64_c" .
#' @param name name of new SDMX provider
#' @param agency name of agency.
#' @param endpoint endpoint location.
#' @param needsCredentials boolean if the provider needs credentials.
#' @param id a time series id.
#' @param start start period of the time series.
#' @param end end period of time series.
#'
#' @author Attilio Mattiocco, Diana Nicoletti, Banca d'Italia
#' @keywords package
#' @seealso \code{\link{RJSDMX}}
#' @export
#' @examples
#' getProviders()
#'
#' getFlows("EUROSTAT")
#' getDSDIdentifier("EUROSTAT", "nama_nace64_c")
#' getDimensions("EUROSTAT", "nama_nace64_c")
#' ans=getTimeSeries("EUROSTAT", "nama_nace64_c.A.MIO_NAC.*.B1G.AT", , start = "1970")
#' plot(ans[[1]])
#'
#' getFlows("OECD")
#' getDimensions("OECD", "REFSERIES")
#' ans=getTimeSeries("OECD", "REFSERIES.ITA.PPPGDP.A")
#' plot(ans[[1]])
#'
#' sdmxHelp()

getTimeSeries <- function(provider, id, start="", end="") {
  getSDMX(provider, id, start, end)
}

#' @rdname getTimeSeries
getFlows <- function(provider, pattern="") {
  jlist <- J("it.bankitalia.reri.sia.sdmx.client.SdmxClientHandler")$getFlows(provider, pattern)
  res = convertHashTable(jlist)
	return(res)
}

#' @rdname getTimeSeries
getDSDIdentifier <- function(provider, dataflow) {
  res <- J("it.bankitalia.reri.sia.sdmx.client.SdmxClientHandler")$getDSDIdentifier(provider, dataflow)
	return(.jstrVal(res))
}

#' @rdname getTimeSeries
addProvider <- function(name, agency, endpoint, needsCredentials=F) {
  J("it.bankitalia.reri.sia.sdmx.client.SdmxClientHandler")$addProvider(name, agency, endpoint, needsCredentials)
}

#' @rdname getTimeSeries
getDimensions <- function(provider, dataflow) {
  res <- J("it.bankitalia.reri.sia.sdmx.client.SdmxClientHandler")$getDimensions(provider, dataflow)
  jlist <- .jcall(res,"[Ljava/lang/Object;","toArray");
  res = convertDimList(jlist)
  return(res)
}

#' @rdname getTimeSeries
getCodes <- function(provider, flow, dimension){
  javaKeys<-J("it.bankitalia.reri.sia.sdmx.client.SdmxClientHandler")$getCodes(provider, flow, dimension)
  idSet = .jcall(javaKeys, "Ljava/util/Set;", "keySet")
  ids = .jcall(idSet, "[Ljava/lang/Object;", "toArray")
  rIDs = as.list(ids)
  nameSet = .jcall(javaKeys, "Ljava/util/Collection;", "values")
  names = .jcall(nameSet, "[Ljava/lang/Object;", "toArray")
  rNames = as.list(names)
  keyIdentifiers = lapply(rIDs, .jstrVal)
  keys = lapply(rNames, .jstrVal)
  names(keys) <- keyIdentifiers
  return(keys)
}

#' @rdname getTimeSeries
getSDMX <- function(provider, id, start="", end="") {
  res <- J("it.bankitalia.reri.sia.sdmx.client.SdmxClientHandler")$getTimeSeries(provider, id, start, end)
  ##convert to an R list
  res = convertTSList(res)
  return(res)
}

#' @rdname getTimeSeries
getProviders <- function() {
  jlist <- J("it.bankitalia.reri.sia.sdmx.client.SdmxClientHandler")$getProviders()
	res = convertStringList(jlist)
	return(res)
}

#' @rdname getTimeSeries
sdmxHelp<- function(){
  J("it.bankitalia.reri.sia.sdmx.helper.SDMXHelper")$start()
}

####################################################
## TODO: Main class for consuming SDMX web services
##
## Author: E922480
####################################################

## uncomment for local usage
##library(rJava)
##library(zoo)
##library(tcltk)
## ux
##.jinit(classpath="lib/SDMX.jar:lib/SDMX_SEC.jar")
## win
##.jinit(classpath="lib/SDMX.jar;lib/SDMX_SEC.jar")
##source("TSConverter.R");
## get the key family of the given dataflow (after retrieving the key family)
