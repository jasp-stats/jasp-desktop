with_dependencies {
	CRAN = http://cran.r-project.org/src/contrib
	ARCHIVES = $$PWD/archives

	RCPP                    = Rcpp_0.11.3.tar.gz
	ArchiveRcpp.target      = $$ARCHIVES/$$RCPP
	ArchiveRcpp.commands    = cd $$ARCHIVES && wget $$CRAN/$$RCPP
	Rcpp.target             = $$JASP_R_LIB_BUILD/Rcpp
	Rcpp.commands           = $$R_EXE CMD INSTALL --library=$$JASP_R_LIB_BUILD $$ArchiveRcpp.target && touch --no-create $$JASP_R_LIB_BUILD/Rcpp
	Rcpp.depends            = ArchiveRcpp
	QMAKE_EXTRA_TARGETS     += Rcpp ArchiveRcpp
	RPackage.depends        += Rcpp

	RINSIDE                 = RInside_0.2.11.tar.gz
	ArchiveRInside.target   = $$ARCHIVES/$$RINSIDE
	ArchiveRInside.commands = cd $$ARCHIVES && wget $$CRAN/$$RINSIDE
	RInside.target          = $$JASP_R_LIB_BUILD/RInside
	# FIXME: this seems to put $$JASP_R_LIB_BUILD into .libPaths() when running an RInside interpreter
	# maybe we should give up on this trick of relocating from the build dir $$JASP_R_LIB_BUILD to
	# the library dir $$JASP_R_LIBRARY
	# UPDATE: debian packaging uses the same trick (see /usr/share/R/debian/r-cran.mk) so this makes RInside
	# unpackageable for debian. We could file a bug report with the RInside authors.
	RInside.commands        = $$R_EXE CMD INSTALL --library=$$JASP_R_LIB_BUILD $$ArchiveRInside.target && touch --no-create $$JASP_R_LIB_BUILD/RInside
	RInside.depends         = ArchiveRInside Rcpp
	QMAKE_EXTRA_TARGETS     += RInside ArchiveRInside
	RPackage.depends        += RInside

	# RInside doesn't expose its build options in a sane way,
	# so lets add them manually
	INCLUDEPATH             += $$JASP_R_LIB_BUILD/RInside/include
	# note that we link NOW against $$JASP_R_LIB_BUILD, but at runtime we need to use -rpath $$JASP_R_LIBRARY
	LIBS                    += -L$$JASP_R_LIB_BUILD/RInside/lib -lRInside -Wl,-rpath,$$JASP_R_LIBRARY/RInside/lib

	# similarly, we cannot query Rcpp:::*Flags until we've installed it. But since we're in
	# control of this packages' compilation, we might as well add them verbatim
	INCLUDEPATH             += $$JASP_R_LIB_BUILD/Rcpp/include
} else {
	QMAKE_CXXFLAGS += $$system( $$RSCRIPT -e \'cat(Rcpp:::CxxFlags())\' )
	LDFLAGS        += $$system( $$RSCRIPT -e \'cat(Rcpp:::LdFlags())\' )

	# RInside doesn't expose its build options in a sane way,
	# so lets add them manually
	INCLUDEPATH += $$R_LIB/RInside/include
	LIBS        += -L$$R_LIB/RInside/lib -lRInside -Wl,-rpath,$$R_LIB/RInside/lib
}
