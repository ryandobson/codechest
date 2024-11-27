

#>First principle of making a package is that all R code goes
#>in the R/ directory.


#> Organize functions into files -----
#>
#>The file name should be meaningful and convey which functions are defined
#>within.
#>The two extremes are bad:
#>Don't put all functions into one file
#>and
#>Don't put each function into its own separate file
#>
#>If a function is large enough, its fine to have it in its own file.
#>However, often times you will incorporate several similar functions into
#>the same R script.
#>Other functions that don't fit well into other areas can also be in their
#>own file.
#>Otherwise, you can have families of similar functions.
#>Or you an have a main function and its helper functions
#>
#>Another file you often see is R/utils.R
#>This is a common place to define small utilities that are used inside multiple
#>package functions. Since they serve as helpers to multiple functions, placing
#>them in utils.R makes them easier to find when you return to your package after
#>a long break.
#>
#>Good question to ask yourself:
#>Is it hard to predict which file a function lives in?
#>Then its time to separate and reorganize your files and reconsider how
#>you are naming your functions and files.
#>
#>The organization of functions within files is less important because there are
#>search tools that make that easy enough to do.
#>
#>Another reminder: Use load_all() to test your package frequently!
#>
#>Code Style -----
#>
#>Use the tidyverse style guide (saved to R chrome folder)
#>Use the styler package
#>You can restyle lots of things easily with styler.
#>If you don't use GitHub though it can be hard to see exactly what
#>styler changed about your code!
#>Be careful and double check that the changes don't cause issues.


#> Understand when code is executed ----
#>
#>In a script, code is run...when you run it.
#>The code in a package is run when the package is built.
#>This has big implications for how you write the code in a package!
#>Package code should only create objects, the vast majority of which will
#>be functions.
#>Functions in your package will be used in situations that you didn't
#>imagine. This means that your functions need to be thoughtful in the way that
#>they interact with the outside world.
#>
#>The main takeaway is that functions that assess or expose the capabilities of
#>your package on a user's system must fully execute on your user's system. It is
#>fairly easy to accidentally rely on results that were cached at build time, quite
#>possible on a different machine.

# Here is a non-exhaustive list of other functions that should be used with caution:
#
#   options()
# par()
# setwd()
# Sys.setenv()
# Sys.setlocale()
# set.seed() (or anything that changes the state of the random number generator)


#> Isolate side effects ----
#>
#>Creating plots and printing output to the console are two ways of affecting the
#>global R environment.
#>Often you can't avoid these because they are important, but its good practice
#>to isolate them in functions that only produce output.
#>For example, if you separate data preparation and plotting into two functions,
#>others can use your data prep work to create new visualizations.


#> When you do need side-effects ------
#> You can use special functions onLoad() and onAttach()
#> to help with this.
#>
#> Constant Health Checks -----

#> (1) edit one or more files below R/
#> (2) document()
#> (3) load_all()
#> (4) Run some examples interactively
#> (5) test() (or test_active_file())
#> (6) check()
#>
#> Finding and fixing 5 bugs, one at a time, right after you created each one is
#> much easier than troubleshooting all 5 at once, weeks or months
#> after you last touched the code!
#>
#>You can't use subdirectories inside of your packages R/ folder.
#>Next best thing is to use a common prefic (e.g., "abc") to signal that
#>a group of files are related.











