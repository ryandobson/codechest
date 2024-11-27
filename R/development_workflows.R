

#> When/why should you create a package?
#> PRODUCT: your life will be better when this functionality is implemented
#> formally, in a package.
#> PROCESS: greater mastery of R will more you more effective in your work.


#> There are plenty of reasons to make your own package even if there is already
#> work that is similar.
#> You become an expert by first building simple things and tinkering around.
#> You can also evaluate the suitability of existing tools. You can explore their defaults
#> and play around with edge case behavior.
#> Packages might technically do what you need but might be very unergonomic.
#> You can develop your own implementation or write wrapper functions that smooth
#> over the sharp edges.

#> If you work falls into a well-defined domain, educate yourself about the existing
#> R packages.
#> Do similar style packages follow a specific design package?
#> Are there specific data structures that are common as the primary input and output?
#> e.g., Spatial data analysis has a very active R community.
#> In modeling the hardhat package provides scaffolding for creating a modeling
#> package that plays well with the tidymodels ecosystem.
#> Your package will be more useful if it fits into other packages better.


#> Naming Packages:
#> Formal Requirements:
#> 1. The name can only consist of letters, numbers, and periods
#> 2. It must start with a letter
#> 3. It cannot end with a period
#> i.e., no hypens or underscores. Recommended to not use periods either.

#> See website for practical naming considerations: https://r-pkgs.org/workflow101.html
#> can use the "available" package to evalute a few things
#> install.packages("available")
#> library(available)
#> available("yourpackagename")
#available("codechest")
#Appears to be a pretty solid name. Not taken and descriptive
#no weird capitalization either.


#> STRONGLY RECOMMEND that you keep the top-level of your source package
#> as the working directory of your R process.
#> This happens by default, so its really a recommendation to not fiddle with your
#> working directory in your developmental workflow.

#>The basic idea is that by leaving working directory alone, you are encouraged to
#>write paths that convey intent explicitly instead of implicitly.
#>A sure sign of reliance on implicit paths is incessant fiddling with your
#>working directory, because you are using setwd() to manually fulfill the assumptions
#>that are implicit in your paths.
#>Its tricky to write implicit paths that work across all package states.

#> Path helpers:
#> testthat::test_path()
#> and
#> fs::path_package()
#> rprojroot package is also extremely useful.
#> Using these tools helps you build resilient paths that hold up across a wide
#> variety of situations during development.

#> Use load_all() to test and tweak your package!
#> With devtools attached and
#> working directory set to top-level of your source package ...
#> load_all()
#> ... now experiment with the functions in your package

#> load_all() simply simulates taking the source package code state and taking it into
#> a in memory package state. It allows for quick and easy testing.
#> It allows you to test your own internal functions without using codechest::function
#> or to be tempted to define your functions in the global environment
#> You can also call functions from other packages that you've imported into your
#> NAMESPACE, without being tempted to attach these dependencies via library().

#> check() ad R CMD check
#> R CMD check is the official method for checking that an R package is valid.
#> It is essential to pass this if you are to submit to CRAN.
#> Highly recommended to hold yourself to this standard even if you don't intend
#> to release your package on CRAN.
#> They recommend to run R CMD check in the R console via devtools:
#> devtools::check()
#> Recommended to run R CMD check here because its faster and raisese the chance
#> that you will check your package early and often.
#> The sooner you find a problem, the easier it is to fix!

#> If you use GitHub (see section 20.1) you can set things up so that
#> R CMD check runs automatically every time you push.

#> What happens inside devtools::check() ?
#> Ensures the documentation is up-to-date by running devtools::document()
#>Bundles the package before checking it.
#>Sets the NOT_CRAN environment variable to TRUE - thus skipping those tests

#> ERRORs: severe problems that should be fixed regardless of whether you submit
#> to CRAN or not.
#> WARNINGs: Likely problems you must fix if submitting to CRAN (and a good
#> idea to look into even if you're not).
#> NOTEs: mild problems, or, in a few cases, just an observation.
#> If submitting to CRAN, you should strive to eliminate all notes, even if they
#> are false positives. Sometimes you can tolerate a note if eliminating it will
#> be a net negative to your package.


#> Strongly recommend to not depend on the tidyverse meta-package in a package.
#> Instead, it is better to identify the specific pacakge(s) you actually use.
#> e.g., Imports:
#>            dplyr
#>

#> "no visible binding" note is peculiar of using dplyr and unquoted variable names
#>dplyr makes heavy use of non-standard evaluation, behind the scenes dplyr allows
#>the user to use bare (not quoted) variable names. This is purely to make the user
#>experience friendly.
#>You can fix this note by adding in globalvariations:
#>utils::globalVariables(c("bare_variable_name1", "bare_variable_name2"))
#>or by doing:
#>bare_var_1 <- Biocbare_variable_2 <- NULL
#>If you use options 1, you should also put utils in Imports



#> You need to adopt a different mindset when defining functions inside a package.
#> Try to avoid making any changes to the user's overall state.
#> If such changes are unavoidable, make sure to reverse them (if possible) or to
#> document them explicitly (if related to the function's primary purpose).

#> "Finding the package within"
#> You typically start with a collection of idiosyncratic and related R scripts
#> scattered across different projects.
#> Over time, you begin to notice that certain needs come up over and
#> over again.
#> Each time you revisit a similar analysis, you might try to elevate your game a
#> bit. You refacotr copy/paste-style code using more robust patterns and start to
#> encapsulate key "moves" in helper functions, which might eventually migrate into
#> their own file.
#> Once you reach this stage, you're in a great position to take the next step and
#> create a package.
#>

#> Package code is different
#>Package code requires new ways of working with functions in other packages.
#>The DESCRIPTION file is the principal way to declare dependencies (don't use library
#>in your package!)
#>If you want data or files to be persistently available, there are package specific
#>methods of storage and retrieval.
#>It's necessary to be explicit about which functions are user-facing and which
#>are internal helps. By default, functions are not exported for use by others.
#>A new level of discipline is required to ensure that code runs at the intended time




