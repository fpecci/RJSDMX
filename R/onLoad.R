.onLoad <- function(libname, pkgname) {
	require(rJava)
	.jpackage(pkgname, lib.loc = libname)
}