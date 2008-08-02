
# TODO: make options more orthogonal.  multilevel scheme?

# modified from Lattice package 
# which in turn is modified from R's options/getOption code

vh.getOption <- function(name) {
    get("vh.options", envir = .vh.env)[[name]]
}

## FIXME: vh.options(foo == 1) doesn't work?
vh.options <- function(...)
{
	updateList <- function (x, val) {
		if (is.null(x)) x <- list()
		modifyList(x, val)
	}

    ## this would have been really simple if only form allowed were
    ## vh.options("foo", "bar") and
    ## vh.options(foo=1, bar=2). But it could also be
    ## vh.options(foo=1, "bar"), which makes some juggling necessary

    new <- list(...)
    if (is.null(names(new)) && length(new) == 1 && is.list(new[[1]])) new <- new[[1]]
    old <- .vh.env$vh.options
    ## any reason to prefer get("vh.options", envir = .vh.env)?

    ## if no args supplied, returns full options list
    if (length(new) == 0) return(old)

    nm <- names(new)
    if (is.null(nm)) return(old[unlist(new)]) ## typically getting options, not setting
    isNamed <- nm != "" ## typically all named when setting, but could have mix
    if (any(!isNamed)) nm[!isNamed] <- unlist(new[!isNamed])

    ## so now everything has non-"" names, but only the isNamed ones should be set
    ## everything should be returned, however

    retVal <- old[nm]
    names(retVal) <- nm
    nm <- nm[isNamed]

    ## this used to be

    ## modified <- updateList(retVal[nm], new[nm])
    ## .vh.env$vh.options[names(modified)] <- modified

    ## but then calling vh.options(foo = NULL) had no effect
    ## because foo would be missing from modified.  So, we now do:

	# updateList is defined at top of this function.
	# Its nearly the same as modifyList.
    .vh.env$vh.options <- updateList(old, new[nm])

    ## return changed entries invisibly
    invisible(retVal)
}

# set = default returns all default options
#     = unset returns default options not currently set to a non-null value
#     = set returns default options that are currently set to a non-null value
# e.g. Set all default options leaving non-default options as is
#       options(vh.defaultOptions())
#      Set all default options not currently set leaving others as is 
#       options(vh.defaultOptions("unset"))
vh.defaultOptions <- function(set = c("all", "set", "unset")) {
	defaults <- list(
		connected = FALSE, # is server connected?

		demo = FALSE, # if TRUE use files instead
		# convert times to an R class using this function
		# used by vh.getFile.* and vh.getVhayu.* routines
		FUN = as.Date,
		loaded = FALSE, # is dll loaded?

		# components of dataFile are default arguments for vh.GetDataFile
		
		# default arguments of vh.getfile. Also see FUN above.
		# If header not specified it defaults to TRUE if a-z or A-Z is
		# found on 1st line of file.
		# If sep not specified it defaults to "," if comma found on 1st line.
		# format = "%Y%m%d",
		datapath = local({
			# c(".", "...whatever.../library/VhayuR/data")
			datapath <- c(".", system.file("data", package = "RVhayu"))
			datapath[datapath != ""]
		}),
	
		# default arguments for vh.GetDataVhayu
		# startTime = "", endTime = "", filter = "", maxRecs = 0,
		frDef = "VhTrade", 
		dllpath = local({
		    dllname = "VhayuR.dll"
			toolkitdll <- if (file.exists("/Vhayu"))
				tail(dir(path = "/Vhayu", full.names = TRUE, recursive = TRUE,
					pattern = paste("^", dllname, "$", sep = "")), 1)
			toolkitdir <- if (is.character(toolkitdll)) dirname(toolkitdll)
			PATH <- strsplit(Sys.getenv("PATH"), ";")
			dllpath <- c(file.path(path.expand("~")),
					system.file("libs", package = "vh."), toolkitdir, PATH)
			dllpath <- unique(unname(unlist(dllpath)))
			dllpath[dllpath != ""]
		}),
		port = 7070,
		# retclass = "data.frame",
		# retclass = "zoo",
		# server = "192.0.2.185",
		verbose = getOption("verbose") # more output if TRUE
	)
	switch(match.arg(set),
		all = defaults,
		set = options(intersect(names(defaults), names(options()))),
		unset = options(setdiff(names(defaults), names(options()))))
}


