# TODO: Basic class for converting a java.util.List of RTimeSeries (a java type of the
#		ECB_WS packag) into a R list of timeseries
#
# Author: e922480
###############################################################################

# convert a java List<String>
convertStringList <- function (javaList) {
	rList = as.list(javaList)
	numOfStrings = length(rList)
  result=unlist(lapply(rList, .jstrVal))
	return(result)
}

# convert a java List<Dimension>
convertDimList <- function (javaList) {
  rList = as.list(javaList)
  numOfDims = length(rList)
  result = list()
  if( numOfDims > 0 ) {
    for (i in 1:numOfDims)  {
      name=.jcall(rList[[i]],"Ljava/lang/String;","getId");
      codes=.jcall(rList[[i]],"Ljava/lang/String;","getCodeList");
      result[[name]] = codes
    }
  }
  return(result)
}

# convert a java HashTable<String, String>
convertHashTable <- function (javaList) {
	idSet = .jcall(javaList,"Ljava/util/Set;","keySet");
	ids = .jcall(idSet,"[Ljava/lang/Object;","toArray");
	rIDs = as.list(ids)

	nameSet = .jcall(javaList,"Ljava/util/Collection;","values");
	names = .jcall(nameSet,"[Ljava/lang/Object;","toArray");
	rNames = as.list(names)

  flowIdentifiers = lapply(rIDs, .jstrVal)
	result = lapply(rNames, .jstrVal)
	names(result) <- flowIdentifiers
	return(result)
}

# convert a java List<PortableTimeSeries>
convertTSList <- function (javaList) {

	rList = as.list(javaList);
	numOfTS = length(rList);
	#print(paste("Found ", numOfTS, " timeseries"));
	#result = list();
	names = lapply(X=rList, FUN=getNames)
	result = lapply(X=rList, FUN=convertSingleTS)
  names(result) <- names
	#if( numOfTS > 0 ) {
# 		for (i in 1:numOfTS)  {
# 			s = .jcast(rList[[i]], new.class = "it/bankitalia/reri/sia/sdmx/api/PortableTimeSeries", check = TRUE);
# 			name = .jcall(s,"Ljava/lang/String;","getName", evalString = TRUE);
# 			#print(paste("Name: ", name));
# 			freq = .jcall(s,"Ljava/lang/String;","getFrequency", evalString = TRUE);
# 			#print(paste("Frequency: ", freq));
# 			dimensions = .jcall(s,"[Ljava/lang/String;","getDimensionsArray", evalArray = TRUE,
# 					evalString = TRUE);
# 			#print(paste("Dimensions: ", dimensions));
# 			attributes = .jcall(s,"[Ljava/lang/String;","getAttributesArray", evalArray = TRUE,
# 					evalString = TRUE);
# 			#print(paste("Attributes: ", attributes));
# 			timeSlots = .jcall(s,"[Ljava/lang/String;","getTimeSlotsArray", evalArray = TRUE,
# 					evalString = TRUE);
# 			#print(paste("timeSlots: ", timeSlots));
# 			observationsJ = .jcall(s,"[Ljava/lang/Double;","getObservationsArray", evalArray = TRUE);
# 			status = .jcall(s,"[Ljava/lang/String;","getStatusArray", evalArray = TRUE,
# 					evalString = TRUE);
# 			numOfObs = length(observationsJ);
# 			numOfTimes = length(timeSlots);
# 			if( numOfObs > 0 && numOfObs == numOfTimes){
# 				observations = array();
# 				# print(paste("numOfObs: ", numOfObs))
# 				if(numOfObs != 0){
# 					for(k in 1:numOfObs){
# 						observations[k] = .jcall(observationsJ[[k]],"D","doubleValue");
# 					}
# 				}
# 				timeSeries = makeSDMXts(name, freq, timeSlots, observations, attributes, dimensions, status);
# 				result[[name]] = timeSeries;
# 			}
# 			else{
# 				print(paste("Number of observations and time slots equal to zero, or not matching: ", numOfObs, " ", numOfTimes));
# 			}
# 		}
	#}
	return(result);
}

getNames<-function(ttss){
  s = .jcast(ttss, new.class = "it/bankitalia/reri/sia/sdmx/api/PortableTimeSeries", check = TRUE);
  name = .jcall(s,"Ljava/lang/String;","getName", evalString = TRUE);
  return(name)
}

convertSingleTS<-function(ttss){
    s = .jcast(ttss, new.class = "it/bankitalia/reri/sia/sdmx/api/PortableTimeSeries", check = TRUE);
    name = .jcall(s,"Ljava/lang/String;","getName", evalString = TRUE);
    freq = .jcall(s,"Ljava/lang/String;","getFrequency", evalString = TRUE);
    dimensions = .jcall(s,"[Ljava/lang/String;","getDimensionsArray", evalArray = TRUE,
                        evalString = TRUE);
    attributes = .jcall(s,"[Ljava/lang/String;","getAttributesArray", evalArray = TRUE,
                        evalString = TRUE);
    timeSlots = .jcall(s,"[Ljava/lang/String;","getTimeSlotsArray", evalArray = TRUE,
                       evalString = TRUE);
    observationsJ = .jcall(s,"[Ljava/lang/Double;","getObservationsArray", evalArray = TRUE);
    status = .jcall(s,"[Ljava/lang/String;","getStatusArray", evalArray = TRUE,
                    evalString = TRUE);
    numOfObs = length(observationsJ);
    numOfTimes = length(timeSlots);
    if( numOfObs > 0 && numOfObs == numOfTimes){
      observations = sapply(observationsJ, .jcall,"D","doubleValue")
#       observations = array();
#       if(numOfObs != 0){
#         for(k in 1:numOfObs){
#           observations[k] = .jcall(observationsJ[[k]],"D","doubleValue");
#         }
#       }
      tts = makeSDMXTS(name, freq, timeSlots, observations, attributes, dimensions, status);
    }
    else{
      print(paste("Number of observations and time slots equal to zero, or not matching: ", numOfObs, " ", numOfTimes));
    }
    return(tts)
}

# parameters used:
# Tsname = name to be given to the series
# freq = sdmx frequency
# times= array of dates
# values= array of values
# series_attr_names= list of names of series attributes
# series_attr_values= list of values of series attributes
# status= list of values of status attribute
# convert the frequency from SDMX-like codelist to numeric, e.g. 'M'-> 12 etc..
makeSDMXTS<- function (TSname,freq,times,values,series_attr, series_dims, status) {

	if(length(values > 0)) {

		if (is.null(freq)) {
			print ("Frequency is NULL. Irregular timeseries defined")
			tmp_ts <- zoo(values, order.by=times)
		}
		else {
      if(freq == 'A'){
        tmp_ts<- zoo(values, order.by = as.integer(times),frequency=1)
      } else if(freq == 'M'){
        tmp_ts<- zoo(values, order.by = as.yearmon(times),frequency=12)
      } else if(freq == 'Q'){
        tmp_ts<- zoo(values, order.by = as.yearqtr(sub(pattern='-', replacement=' ', times)),frequency=4)
      } else {
        print (paste('Frequency ',freq, '. Does not allow the creation of a strictly regular time series. Irregular timeseries defined', sep=''))
	      tmp_ts <- zoo(values, order.by=as.Date(times))
      }
	}
	}
	else{
		# if the time series is empty
		tmp_ts <- zoo()
	}

# 	# define ts attributes
# 	attr(tmp_ts,"Id") <- TSname
# 	attr(tmp_ts,"Name") <- TSname

	# define ts attributes
	attr(tmp_ts,"STATUS") <- status

	# define the timeseries attributes (dimensions + attributes)
	if (length(series_dims) >0){
		for (l in 1:length(series_dims)) {
			attrComp=(unlist(strsplit(series_dims[l], "=", fixed = TRUE)))
			attr(tmp_ts,attrComp[1]) <- (attrComp[2])
		}
	}
	if (length(series_attr) >0){
		for (l in 1:length(series_attr)) {
			attrComp=(unlist(strsplit(series_attr[l], "=", fixed = TRUE)))
			attr(tmp_ts,attrComp[1]) <- (attrComp[2])
		}
	}
	return(tmp_ts)

}


                                        # Bo Werth::2014-08-12
sdmxTS2DF <- function(SDMXTS,provider,timevar="date",numeric=TRUE) {
    require(reshape2)
    id <- sapply(strsplit(names(SDMXTS[1]), "[.]"), "[[", 1)
    dimnames <- names(getDimensions(provider, id))
    SDMXTS <- do.call("merge.zoo", SDMXTS)
    SDMXTS.df <- as.data.frame(SDMXTS)
                                        # add functions to convert date to R date format
    SDMXTS.df[[timevar]] <- rownames(SDMXTS.df)
    if (numeric==TRUE) SDMXTS.df[[timevar]] <- as.numeric(SDMXTS.df[[timevar]])
    SDMXTS.df.m <- suppressWarnings(melt(SDMXTS.df, id.vars = c(timevar)))
    X <- strsplit(as.character(SDMXTS.df.m$variable), "[.]")
    for (d in seq(along = dimnames)) {
        SDMXTS.df.m[dimnames[d]]  <- sapply(X, '[[', d+1)
    }
    SDMXTS.df.m <- SDMXTS.df.m[,!colnames(SDMXTS.df.m)=="variable"]
    return(SDMXTS.df.m)
}
