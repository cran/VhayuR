
.vh.env <- new.env()
assign("vh.options", list(), .vh.env)

vh.Announce <- function(pkg) {
        cat("Package ", pkg, " (", packageDescription(pkg)$Version, ") loaded.
The Brookahven Group, LLC - Trading Simulation
Copyright The Brookhaven Group, LLC 2008
All Rights Reserved
", sep = "")
}

.First.lib <- function(lib, pkg) {

	require(stats)
	require(utils)
    require(VhayuR)

	vh.options(vh.defaultOptions())
	# TODO: decide whether to leave next line or remove it
	if (!is.null(VhayuR <- getOption("VhayuR"))) do.call("vh.options", VhayuR)

    # uncomment to only show intro message if verbose TRUE
    if (interactive() || getOption("verbose")) vh.Announce(pkg)

	if (!isTRUE(vh.getOption("demo"))) {
		pathname <- vh.findDLL()
		if (is.null(pathname)) vh.install()
	}

	# set those default options which are not already set
	vh.options(vh.defaultOptions())

}



vh.install <- function(url = "ftp://ftp.vhayu.com/outgoing") {
   stopifnot(.Platform$OS.type == "windows")
   if (is.null(vh.findDLL())) {
		cat("Go to", url, 
		"\n- sign in\n- download vhayu_clienttoolkitsuiteinstaller_*.zip",
		"\n- extract files from zip file that is downloaded",
		"\n- double click on ClientToolkitSuiteSetup.exe",
		"\n- follow instructions of installer wizard\n")
	}
}
		
