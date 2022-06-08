---
title: "cran-comments"
output: html_document
---
6/8/2022

Package Update (0.2.0)

R CMD Check Results

checking installed package size ... NOTE
    installed size is  9.4Mb
    sub-directories of 1Mb or more:
      extdata   8.5Mb
      
       * The external data in this package has been cut down to a bare minimum but due to the nature of the data being processed by this package (images and video), the data is inherantly large and difficult to reduce any further

 checking top-level files ... NOTE
  Non-standard files/directories found at top level:
    'CODE_OF_CONDUCT.md' 'CONTRIBUTING.md' 'SECURITY.md'
    
    * These are standard template files required by my organization (Government of Canada). If there is a better way to integrate them into the package please let me know
    
    

23/10/2020
Notes from third submission:



* Please write TRUE and FALSE instead of T and F. (Please don't use "T" or "F" as vector names.) e.g.: man/vp_plot_matrix.Rd, man/vp_plot_unkn.Rd
    * completed
* Please always add all authors, contributors and copyright holders in the Authors@R field with the appropriate roles. e.g.: R Klaver
    * R. Klaver was removed as his contribution to functions was not significant


07/28/2020
Notes from second submission:

* If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) doi:...
        *  As of now there is no published work to reference in the description. When a publication becomes available (likely before the next version of the package), I will update.

* Please rather use the Authors@R field and declare Maintainer, Authors
and Contributors with their appropriate roles with person() calls.
        * Adjusted, thanks!
        

* \dontrun{} should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \dontrun{} adds the comment
("# Not run:") as a warning for the user.
Does not seem necessary.
Please unwrap the examples if they are executable in < 5 sec, or replace
\dontrun{} with \donttest{}.
          * There are 3 examples which use \dontrun{} due to their external file requirements, the examples show only how a user would call a list of files in and read them but the files are not provided within the package (to keep package size to a minimum)
      

* You write information messages to the console that cannot be easily
suppressed.
        * print() and cat() commands have been changed to message() or warning() where possible. Some functions use cat() because they do not actually print messages to the console but they create diagnostic text files for the user during complex procedures. Other functions are interactive and require user input.
        
        
* Please make sure that you do not change the user's options, par or
working directory.
        * The only instance of par being changed has been edited to include a reverting to the user's original par on exit.
        
        
* Is this email adress a mailing list?
        * No this email is not a mailing list, we used a generic 'vprrcontact' because this is a collaborative project with multiple people needing access.



07/17/2020
From first submission:

*   Found the following (possibly) invalid URLs:
     URL: https://gccode.ssc-spc.gc.ca/dfo-mar-odis/visual-plankton/matlab
       From: inst/doc/VPR_processing.html
       Status: Error
       Message: libcurl error code 28:
                Failed to connect to gccode.ssc-spc.gc.ca port 443: Connection
timed out

  * removed URL
  
* Is there some reference about the method you can add in the Description
field in the form Authors (year) <doi:.....>?

  * As of now there is no published work to reference in the description. When a publication becomes available (likely before the next version of the package), I will update. 
  
---
07/14/2020
## Test environments
* local Windows install, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.0


## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:
* checking installed package size ... NOTE
    installed size is  9.1Mb
    sub-directories of 1Mb or more:
      extdata   8.5Mb
      
      
        * The external data in this package has been cut down to a bare minimum but due to the nature of the data being processed by this package (images and video), the data is inherantly large and difficult to reduce any further
        
        
Note from RHub check
  * Possibly mis-spelled words in DESCRIPTION:
  Bedford (9:54)
  WHOI (9:18)
  * Bedford is a place name and WHOI is the name of a research institute
  
  * This is my first submission of a package to CRAN, thank you!
