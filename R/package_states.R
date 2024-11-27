

#>Five states an R package can be in:
#>source
#>bundled
#>binary
#>installed
#>in-memory

#> install.packages() moves a packaged from the source,bundled, or
#> binary states into the installed state.
#> devtools::install_github() takes a source package on github on moves it
#> into the installed state.
#> library() function loads an installed package into memory, making it available
#> for use.

#> Source Package -----
#>Just a directory of files with a specific structure.
#>Includes DESCRIPTION file, an R/ directory and so on.
#>You can find a packages source code online, often at CRAN or github.

#> Bundled Package ------
#>A package that is compressed into a single file.
#>Package bundles in R use the extension .tar.gz and are sometimes referred to as
#>"source tarballs".
#>This means that multiple files have been reduced to a single file (.tar) and then
#>compressed using gzip (.gz).
#>Bundles aren't useful on their own, but its a platform-agnostic transportaiton-friendly
#>intermediary between a source pacakge and an installed package.
#> Every CRAN package is available in bundled form via the "Package source" field
#> on its landing page.
#>You can download bundles and then unpack the bundle via the shell.
#>e.g.: tar xvf forcats_0.4.0.tar.gz
#>A decompressed bundle looks almost exactly like the source package.

#> Differences between source package and uncompressed bundle:
#> 1. Vignettes have been built, so rendered outputs appear below inst/doc/ and a
#> vignette index appears in the build/ directory
#> A local source package might contain temporary files used to save time during
#> development, like compilation artifacts in src/. These are neve in the bundle.
#> Anu files listed in .Rbuildignore are not included in the bundle. These are
#> files that faciliate your development process, but are excluded from the distribution.


#> .Rbuildignore
#> It controls which files from the source package make it into the downstream
#> forms.
#> Each line of .Rbuildignore is a Perl-compatible regular expression.
#> We usually modify .Rbuildignore with usethis::use_build_ignore() function, which
#> takes care of easy-to-forget details, such as regular expression anchoring and
#> escaping.

# Here is a non-exhaustive list of typical entries in the .Rbuildignore file for a package in the tidyverse:
#
#   ^.*\.Rproj$         # Designates the directory as an RStudio Project
#   ^\.Rproj\.user$     # Used by RStudio for temporary files
#   ^README\.Rmd$       # An Rmd file used to generate README.md
#   ^LICENSE\.md$       # Full text of the license
#   ^cran-comments\.md$ # Comments for CRAN submission
#   ^data-raw$          # Code used to create data included in the package
#   ^pkgdown$           # Resources used for the package website
#   ^_pkgdown\.yml$     # Configuration info for the package website
#   ^\.github$          # GitHub Actions workflows

#> All of the comments should not appear in the actual .Rbuildignore file.

#> Binary Package ------

#> If you want to distribute your package to an R user who doesn't have package
#> development tools, you'll need to provide a binary package.
#>The primary making and distributor of binary packages is CRAN, not an individual
#>maintainer.
#>Like a bundled package, a binary package is a single file.
#>CRAN packages are usually available in binary form for Windows and macOS.
#>When you use install.packages(), it is just downloading the binary package. You
#>could also do this via the shell.
#>The structure of binary packages is rather different from source or bundled
#>packages.

#> Installed Package -----

#>An installed package is a binary package that's been decompressed into a pacakge
#>libarary.

#> There are some useful different things to consider when installing packages.
#> Not too relevant currently. See website: https://r-pkgs.org/structure.html

#> In-Memory Package -----

#> library(package)
#>
#> Distinction between loading and attaching packages.
#> library() is not a great way to iteratively tweak and test drive a package
#> you're developing because it only works for installed packages.
#> load_all() is more useful for streamlining the package testing process.

#>A library is a directory containing installed packages, sort of like a library
#>for books.
#>The typical programming terminology is that "library" is closer to what we mean
#>by "package."
#>Bottom line: We use the library() function to load a package.
#>However, there is still a distinction between a library and a package that is
#>important to consider in package development.
#>You can have multiple libraries on your compute:
#>CALL:
#>.libPaths()
#>lapply(.libPaths(), list.dirs, recursive = FALSE, full.names = FALSE)

#> In both cases, we see two active libraries in this order:
#> A user library
#> A system-level or global library
#>
#>With this setup (typical for Windows), add-on packages from CRAN (or elsewhere)
#>or under local development are kept in the user library.
#>The core set of base and recommended packages that ship with R live
#>in the system-level library are the same on all operating systems.
#>This separation between the base R library and add-on packages makes it easy to
#>clean out your add-on packages without distrubing your base R installation.
#>

#>As your R usage becomes more sophisticaed, its common to start managing package
#>libraries with more intention.
#>Renv automates the process of managing project-specific libraries.


# Here are the main levers that control which libraries are active, in order of scope and persistence:
#
#   Environment variables, like R_LIBS and R_LIBS_USER, which are consulted at startup.
# Calling .libPaths() with one or more filepaths.
# Executing small snippets of code with a temporarily altered library search path via withr::with_libpaths().
# Arguments to individual functions, like install.packages(lib =) and library(lib.loc =).

#> library() should NEVER be used inside a package.
#> Packages and scripts rely on different mechanisms for declaring their dependencies
#> and this is one of the biggest adjustments you need to make


