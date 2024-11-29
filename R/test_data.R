

#> Its very useful to include data in your package in order
#> to test different functions.

#> There are also packages that are exclusively data. This can
#> be a nice way to share example data across multiple packages.

#> If you want to store R data objects, put them in data/
#>
#>If you want to store R objects for your own use as a developer,
#>put them in R/sysdata.rda
#>This is the best way to store internal data that your functions need.
#>
#>If you want to store raw, non-R-specific dataset, put it in
#>inst/extdata/
#>For example, readr and readxl each use this mechanism to provide a
#>collection of delimited files and excel workbooks.


#> Exported data ------
#> Most commonly, you will store data in data/ and each file here
#> will be a .rda file created by save() and each of these .rda files will
#> contain a single R object with the same name as the file.
#> Easiest way to do this is with this call:
#> my_pkg_data <- sample(1000)
#> usethis::use_data(my_pkg_data)
#>
#> The above snippet creates a data/my_pkg_data.rda inside the source of the
#> pkg and adds LazyData: TRUE in your DESCRIPTION.
#> This makes the my_pkg_data R object available to users of the pkg via
#> pkg::my_pkg_data.
#>You only need to execute this once (or every time you want to update the data file)
#>
#>Some Recommendations:
#>Store one R object in each data/*.rda file
#>Use the same name for that object and its .rda file
#>Use lazy-loading by default.
#ryanhonorthesis <- read.csv("../../R.learning/osf.thesis/data/raw.osf.thesis.data.csv")


#> Preserve the origin story of package data
#>
#>Often, the data you include in data/ is a cleaned version of raw data you've
#>gathered from elsewhere.
#>Highly recommended to take the time to include the code used to do this in the
#>source version of your package.
#>This makes it easy to update or reproduce your version of the data.
#>The data-creataing script is also a natural place to leave comments about
#>important properties of the data (i.e., which features are important for
#>downstream useage in package documentation).
#>
#>Suggested to keep this code in one or more .R files below data-raw/
#>You don't want it in the bundled version of your package, so it should be listed
#>in .Rbuildignore
#>usethis has a convienence function to achieve this:
#>usethis::use_data_raw()
#>usethis::use_data_raw("my_pkg_data")
#>

#> Documenting Datasets ------
#>Objects in data/ are effectively exported (with slightly different mechanisms
#>than NAMESPACE, but the details are not important).
#>This means they must be documented.
#>Instead of documenting the data directly, you document the name of the dataset
#>and save it in R/
#>Two important roxygen tags:
#>@format gives an overview of the dataset. For data frames, you should include a
#>definition list that describes each variable. Usually a good idea to describe
#>variable units here.
#>@source provides details of where you got the data, often a URL.
#>NEVER @export a dataset.

#> Non-ASCII characters in data
#> data files often contain string variables.
#> If you can constrain these strings to only use ASCII characters, it makes things
#> a lot easier.
#> There are many legitimate reasons to have Non-ASCII characters in dfs though.
#> In thise case, its recommended to embrace the UTF-8 Everywhere manifesto
# http://utf8everywhere.org/
#> By default, the DESCRIPTION file always assumes UTF-8.
#> Make sure that the strings in your package data have the intended coding
#> is something to accomplish in your data preparation code.
#> In the R scripts below data-raw/ you can use Encoding() to learn the current
#> encoding of the elements in a character vector.
#> Functions such as encutf8() or iconv() can convert between encodings.


#> Internal Data ------
#>Sometimes your package functions needs access to pre-computed data.
#>If you put these in /data, they will also be available to users, which isn't appropriate.
#>
#>Sometimes the objects are small and simple enough that you can define them with c()
#>or data.frame() in the code below R/, perhaps in R/data.R.
#>Larger or more complicated objects should be stored in your packages internal
#>data in R/sysdata.rda, so they are lazy loaded on demand.

#> Easiest way to create R/sysdata.R is to use:
#> internal_this <- ...
#> internal_that <- ...
#> usethis::use_data(internal_this, interal_that, internal = TRUE)
#>Unlike in data/ where you use one .rda file per exproted data object, here you
#>store all of your internal data objects together in the single file R/sysdata/rda
#>This makes objects internal_this and internal_that available for use inside of
#>the functions below R/ and in the tests.


#> Raw data file ------
#>
#> The main reason to inclue such files is when a key part of a package's
#> functionality is to act on an external file.
#> For example, readr is designed to read rectangular data out of delimited files.
#> Thus, you need a raw delimited file to test that function on.

#>It is also common for data pacakges to provide the raw csv version of the package
#>data that is also provided as an R object.
#>This has two payoffs:
#> (1) Gives teachers and others more to work with once they decide to use a specific dataset
#> (2) If package data evolves over time, having the raw csv (or other plain text)
#> file helps make it easier to see what has changed.

#> File paths -----

#> The path to a package file found below extdata/ clearly depends on the local
#> environment (i.e., it can depend on where installed packages live on that machine)
#>The base function system.file() can report the full paths to files distributed
#>within an R package.
#>It is also useful to list the files distirbuted with an R package
#> system.file("extdata", package = "readxl") |> list.files()
# system.file("extdata", "clippy.xls", package = "readxl")

#> These filepaths represent yet another workflow dilemma:
#> When you're developing your package, you engage with it in it source form,
#> but your users engage with it as an installed package.
#>
#>devtools provides a shim for base::system.file() that is activated by load_all()
#>fs::path_package()
#>It has a few added features compared to system.file()
#>It errors if the filepath does not exist (instead of returning an empty string)
#>It throws distinct errors when the package does not exist vs. when the file does
#>not exist within the package
#>During development, it works for interactive calls, calls from within the loaded
#>package's namespace, and even for calls originating in dependencies.
#>
#fs::path_package("extdata", package = "idonotexist")
#> Error: Can't find package `idonotexist` in library locations:
#>   - '/home/runner/work/_temp/Library'
#>   - '/opt/R/4.4.2/lib/R/site-library'
#>   - '/opt/R/4.4.2/lib/R/library'

#fs::path_package("extdata", "I_do_not_exist.csv", package = "readr")
#> Error: File(s) '/home/runner/work/_temp/Library/readr/extdata/I_do_not_exist.csv' do not exist

#fs::path_package("extdata", "chickens.csv", package = "readr")
#> /home/runner/work/_temp/Library/readr/extdata/chickens.csv


#>pkg_example() Path Helpers ------
#>User friendly wrappers around system.file() or fs::path_package()
#>
#>Internal State ------
#>Sometimes theres information that multiple functions from your package need
#>access to that:
#>Must be dermined at load time (or even later), not at build time. It might even be dynamic
#>Doesn't make sense to pass via a function argument. Often it's some obscure detail
#>that a user shouldn't even know about.
#>
#>A great way to manage such data is to use an environment.
#>The environment must be created at build time, but you can populate it with values
#>after the package has been loaded and update those values over the course of an
#>R session.
#>See book and this blog:
#>https://trestletech.com/2013/04/package-wide-variablescache-in-r-package/
#>for more information on creating environments for use.
#>They are useful when you need to cache some user information at each session.


#> Persistent User Data -------
#>Sometimes you want user data to persist across R sessions.
#>Not very common, but sometimes useful.
#>For the data to persist this way, it has to be stored on disk and the big question
#>is where to write such a file.
#>Primary function you should use to derive acceptable locations for user data:
#>tools::R_user_dir()
#>
#>Always ask yourself:
#>does this data REALLY need to persist?
#>Do you really need to be the one responsible for storing it?





