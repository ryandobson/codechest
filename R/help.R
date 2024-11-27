
#library(devtools)

#R.Version()
#> Checking System Updates:
# devtools::dev_sitrep()

#> Checing if Rtools path is working properly:
#Sys.getenv("PATH")
#Sys.which("gcc")


#> CAUTION:
#> If you use devtool functions in the script it will result in infinite recurions
#> because most calls are referencing the package script, which now also includes
#> the very call that you just ran.
#> For this reason. Only run devtool things in the console.

#> How many functions in R script? ------
#> To begin, its fine to write one function per script and to name the script
#> the name of the function.
#> However, it is perfectly acceptable and good practice as the package increases
#> in size to put multiple similar functions in the same script file.

#> Naming Functions ------
#> If a function is a wrapper function (i.e., it slightly edits how a current
#> function in existence works), rename it the same name as the original
#> function and append something to it. e.g., str_split() to str_split_one()

#> Renaming  Functions/Files -----
#> A good convention is to name the R file after the function it defines.
#>CALL:
#>rname_files("current file name", "new file name")
#>If you update a function and change the name of the file don't forget to
#>ensure you update the documentation information and test file!
#>Before testing a function again call document()
#>When you remove something from the NAMESPACE by updating the file name/function
#>you will get a warning when calling document() that the object is listed
#>as an export but isn't present. You can safely ignore that warning.



#> GitHub Documentation Help:
#> CALL:
#> use_readm_rmd()
#> NOTE - I've already done this.
#> This creates the rmd file that I can edit and write comments and code examples
#> on GitHub. A handy feature so people can use your function.
#>If you update the rmd file you will need to render it before
#>committing changes to GitHub.
#>Rendering the README.Rmd file:
#>build_readme()

#Testing ----
#> Call:
#> devtools::load_all()
#> In the console to make all of your packages functions
#> available. This does not make them exist in the global
#> environment but you can call your functions to test them.

#exists("kbl_descV1", where = globalenv(), inherits = FALSE)
#>If the above call returns "True" this means that you are still
#>a script oriented workflow and sourcing your functions.
#>That isn't what you want and you'll have to adjust (see book, all good currently)

#> load_all() simulates the process of building, installing, and attaching
#> your package.
#> As your package becomes more complex (e.g., calling different dependencies) using
#> load_all() is very useful for testing things as the package develops.
#> You could also literally build and install the package but that would waste a
#> lot of valuable time.


#> There is another important element for testing things:
#> CALL:
#> check()
#> You can also run "R CMD check" in the shell (terminal).
#> ESSENTIAL:
#> Read the output of the check!
#> Deal with problems early and often. The longer you go between full
#> checks the more problematic things become.
#> My initial warning suggests that I have undefined imports. I need to import
#> the required packages.


#> Function Documentation
#> CALL:
#> document()
#>This call automatically uses roxygen2 to update your documentation.
#>You need to have the specific documentation filled out above your functions.
#>You can then use ?kbl_descV1 to get the function documentation!


#> You can then install the Package:
#CALL:
#> install()

#> We have already informally tested the kbl_descV1 function, but we can also
#> formalize this.
#> This means that we express a concrete expectation about the correct output
#> for a specific input.
#> CALL:
#> use_testthat()
#> This initializes the unit testing machinery for your package.
#> It adds a few things to your package. But you still need to write the
#> actual tests.
#> The helper use_test() opens and/or creates a test file.
#> You can provide the file's basename or, if you are editing the relevant source
#> file in Rstudio, it will be automatically generated.
#> You can provide the basename explicitly:
#> use_test("kbl_descV1")
#> This creates the file: tests/testthat/test-kbl_descV1.R".
#> If it already exists, it just opens the file.
#> It will produce an example for you, delete the example and replace it with your
#> own code.
#> You will need to provide the expected output.
#> Going forward, your tests will mostly run en masse and at arms length
#> with:
#> test()

#> Its a good idea to use the "covr" package to track what proportion
#> of your package's source code is exercised by the tests.

#> Declaing Dependencies -------
#> You will need to use a function from another package in your own package.
#> There are package specific methods for declaring the other packages needed.
#> If you plan to submit to CRAN, this even applies to functions in packages you
#> think of as always available (e.g., base R things like stats::median())

#> Adding a dependency call:
#> use_package("package")
#> I'll call a few now in the console:
#> use_package("dplyr")
#> use_package("tibble")
#> use_package("psych")
#> use_package("kableExtra")
#> After calling these in the console it adds a line in the
#> DESCRIPTION document that calls the import.
#> Thus, I do not need to do this elsewhere now.

#> Functions Called Once at Package Creation ------
#> create_package()
# use_git()
# use_mit_license()
# use_testthat()
# use_github()
# use_readme_rmd()

#> Functions Called as Package Develops ------
#> These functions will be called on a regular basis as you add functions and tests
#> or take on dependencies.
# use_r()
# use_test()
# use_package()

#> Functions Called Frequently as Package Develops ------
#> You will call these multiple times per day or per hour during
#> package development:
# load_all()
# document()
# test()
# check()

