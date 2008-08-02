
# vh.get
# - vh.getVhayu
# 
# vh.flexrecef
# - vh.connect (see above)
#
# vh.connect
# - vh.loadDLL
#   - vh.findDLL
#
# vh.getOption and vh.options used in many routines
# vh.defaultOptions returns the default options

VhSetServer <- function(a,b) .Call("VhSetServer",a,b, PACKAGE = "VhayuR")
VhExecuteQuery <- function(a,b,c,d) .Call("VhExecuteQuery",a,b,c,d, PACKAGE = "VhayuR")
VhGetRecords <- function(a,b,c,d,e,f,g) .Call("VhGetRecords", a,b,c,d,e,f,g, PACKAGE = "VhayuR")
VhRollSetup <- function(a,b,c,d,e) .Call("VhRollSetup", a, b,c,d,e, PACKAGE = "VhayuR")
VhRollBySymbology<- function(a,b,c,d,e,f,g) .Call("VhRollBySymbology", a,b,c,d,e,f,g, PACKAGE = "VhayuR")
VhRollBySymbolList<- function(a,b,c,d,e) .Call("VhRollBySymbolList", a,b,c,d,e, PACKAGE = "VhayuR")
VhRollBySchedule<- function(a,b) .Call("VhRollBySchedule", a,b, PACKAGE = "VhayuR")

# VhSetServer("192.0.2.185",7070)

# returns character vector
# - with no args its vector of flex record names
# - with scalar character string argument its flex record definition
# e.g. View(vh.flexrecdf()
# e.g. View(vh.flexrecdf("1DayFuturesBar")
vh.flexrecdef <- function(x = NULL) {

	stopifnot(length(x) <= 1)
	if (!vh.getOption("loaded")) vh.connect()
	if (length(x) == 0 || identical(x, "")) {
		s <- "return [join [vh_flexrecdefnames] \\n]"
		unlist(strsplit(unlist(VhExecuteQuery(s, 1, 0, 0)), "\n"))
	} else {
		s <- "return [join [join [lindex [vh_flexrecdefinitions /frDefname %s] 0] \\n] \\n]"
		unlist(strsplit(unlist(VhExecuteQuery(sprintf(s, x), 1, 0, 0)), "\n"))
	}

}

vh.fileSearch <- function(file, datapath = vh.getOption("datapath"), ...) { 
    if (file == basename(file)) {
        Find(file.exists, file.path(datapath, file))
    } else {
        if (file.exists(file)) file
    }
}

vh.findDLL <- function(dllpath = vh.getOption("dllpath"), dllname = "VhayuR.dll", verbose = vh.getOption("verbose")) {

	# if vh.option dllpath not set then use path made up of:
	# - home directory, i.e. ~
	# - vh.'s package's libs directory
	# - /Vhayu/.../R_*

	if (is.null(dllpath)) {
		toolkitdll <- if (file.exists("/Vhayu"))
			tail(dir(path = "/Vhayu", full.names = TRUE, recursive = TRUE,
				pattern = paste("^", dllname, "$", sep = "")), 1)
		toolkitdir <- if (is.character(toolkitdll)) dirname(toolkitdll)
		dllpath <- c(file.path(path.expand("~")),
				system.file("libs", package = "vh."), toolkitdir)
	}

	# remove empty components
	dllpath <- dllpath[dllpath != ""]

    if (isTRUE(verbose)) cat(paste("vh.loadDLL. Searching for", dllname, "in:"),
		paste(paste("   ", dllpath), collapse = "\n"), sep= "\n")
    vh.fileSearch(dllname, dllpath)
}

vh.loadDLL <- function(dllpath = vh.getOption("dllpath"), 
	verbose = vh.getOption("verbose")) {

	pathname <- vh.findDLL(dllpath = dllpath, verbose = verbose)

    if (is.null(pathname)) 
		stop("VhayuR.dll not found on ", dllpath, call. = TRUE)

    dyn.load(pathname)
    vh.options(loaded = TRUE)
    if (isTRUE(verbose)) cat("vh.loadDLL. Have loaded:\n  ", pathname, "\n")
	
    invisible(pathname)

}

vh.connect <- function(server = vh.getOption("server"),  
	port = vh.getOption("port"), ...) {

	if (!vh.getOption("loaded")) {
		if (vh.getOption("verbose")) cat("Loading vh..dll\n") 
		vh.loadDLL(...)
	}
	if (vh.getOption("verbose")) cat("Connecting to server\n") 
	VhSetServer(server, port)
	vh.options(connected = TRUE, loaded = TRUE)
	vh.options("server", "port")

}

	
# get data from file or Vhayu based on vh.option "demo"
# e.g. vh.get("series1", FUN = as.Date)
# e.g. vh.get("series1", FUN = as.yearmon)
# e.g. vh.get("series1") # default is POSIXct
vh.get.zoo <- function(x, ...) {

	# get from Vhayu or file
	if (isTRUE(vh.getOption("demo"))) {
		vh.getFile.zoo(x, ...)
	} else vh.getVhayu.zoo(x, ...)

}

vh.get.data.frame <- function(x, ...) {

	# get from Vhayu or file
	if (isTRUE(vh.getOption("demo"))) {
		vh.getFile.data.frame(x, ...)
	} else vh.getVhayu.data.frame(x, ...)

}

vh.getFileArgs <- function(x, datapath, ...) {

	# explicit arguments override vh. options
	dataFile <- vh.options()
	args <- if (is.null(dataFile)) list(...)
	else modifyList(dataFile, list(...))

	# search for filename followed by nothing, .csv, .dat or .txt
	p <- vh.fileSearch(x, datapath)
	if (is.null(p)) p <- vh.fileSearch(paste(x, "csv", sep = "."), datapath)
	if (is.null(p)) p <- vh.fileSearch(paste(x, "dat", sep = "."), datapath)
	if (is.null(p)) p <- vh.fileSearch(paste(x, "txt", sep = "."), datapath)

	if (is.null(p)) 
		stop("file ", x, " not found on path\n", 
			paste(datapath, collapse = ", "), call. = TRUE)


	# if header not specified set it to TRUE if first line has a letter
	# if sep not specified set it to "," if comma in first line

	if (is.null(args$header) || is.null(args$sep)) line1 <- readLines(p, n = 1)

	if (is.null(args$header)) args$header <- regexpr("[[:alpha:]]", line1) > 0
	if (is.null(args$sep)) args$sep <- if (regexpr(",", line1) > 0) ","  else ""

	if (!is.null(args$FUN)) args$FUN <- match.fun(args$FUN)

	# remove datapath and enter file
	args$datapath <- NULL
	c(file = p, args)

}

vh.getFile.zoo <- function(x, datapath = vh.getOption("datapath"), ...) {

	args <- vh.getFileArgs(x, datapath, ...)

	# if FUN and format not specified then just return time as is
	if (is.null(args$format) || identical(args$format, ""))
		if (is.null(args$FUN)) args$FUN <- force
	# enable special processing done by zoo for Date and POSIXct classes
	if (identical(args$FUN, as.Date) && 
		!is.null(args$format) && args$format != "") args$FUN <- NULL
	if (identical(args$FUN, as.POSIXct) && 
		!is.null(args$tz) && args$tz != "") args$FUN <- NULL

	# only keep arguments which are valid for read.zoo or read.table
	args <- args[names(args) %in% c(names(as.list(args(read.zoo))),
		c(names(as.list(args(read.table)))))]

	result <- do.call("read.zoo", args)
	return(result)

}

vh.getFile.data.frame <- function(x, 
	datapath = vh.getOption("datapath"), ...) {

	args <- vh.getFileArgs(x, datapath, ...)

	# only keep arguments which are valid for read.table
	args <- args[names(args) %in% names(as.list(args(read.table)))]

	result <- do.call("read.table", args)
	if (!is.null(args$FUN)) result[[1]] <- args$FUN(result[[1]])
	result

}

# initialize connection (if not already initialized)
vh.getVhayu.list <- function(x, ...) {

	if (!isTRUE(getOption("connected"))) vh.connect()

	dataVhayu <- vh.options()
	args <- if (is.null(dataVhayu)) list(...)
	else modifyList(dataVhayu, list(...))

	if (is.null(args$frDef)) stop("frDef must be specified", call. = TRUE)

	if (is.null(args$startTime)) args$startTime <- ""
	if (is.null(args$endTime)) args$endTime <- ""
	if (is.null(args$fieldNames)) args$fieldNames <- ""
	if (is.null(args$filter)) args$filter <- ""
	if (is.null(args$maxRows)) args$maxRows <- 0
	
	yy <- VhGetRecords(a = x, b = args$frDef, c = args$startTime, 
		d = args$endTime, e = args$fieldNames, f = args$filter, 
		g = args$maxRows)
	# TODO: comment out next line when finished debugging
	xx <- yy
	yy <- lapply(yy, as.numeric)

	FUN <- args$FUN
	if (!is.null(FUN)) {
		yy[[1]] <- structure(yy[[1]], class = c("POSIXt", "POSIXct"))
		FUN <- match.fun(FUN)
		yy[[1]] <- FUN(yy[[1]])
	}
	return(yy)
}

vh.getVhayu.matrix <- function(x, ...) {
	do.call(cbind, lapply(vh.getVhayu.list(x, ...), as.numeric))
}

vh.getVhayu.data.frame <- function(x, ...) {
	as.data.frame(vh.getVhayu.list(x, ...), stringsAsFactors = FALSE)
}

vh.tail1 <- function(x, n = 1, ...) tail(x, n = n, ...)

vh.getVhayu.zoo <- function(x, ...) {

	y <- vh.getVhayu.list(x, ...)

	op <- vh.options()
	args <- if (is.null(op)) list(...)
	else modifyList(op, list(...))

	if (is.null(args$aggregate)) {
		if (length(y) == 2)
			zoo(y[[2]], y[[1]])
		else zoo(do.call(cbind, y[-1]), y[[1]]) 
	} else {
		if (length(y) == 2)
			aggregate(zoo(y[[2]]), y[[1]], args$aggregate)
		else aggregate(zoo(do.call(cbind, y[-1])), y[[1]], args$aggregate)
	}
	
}

