CRAN Submission Procedure
~~~~~~~~~~~~~~~~~~~~~~~~~

 1. Edit inst/ChangeLog to reflect version number, date and list of changes
 2. Edit DESCRIPTION to reflect version number and date
 3. From this directory, build source tar with::

    $ R CMD build --resave-data seas

 4. Check build::

    $ R CMD check --as-cran seas_x.y-z.tar.gz

 5. Upload and submit package to https://cran.r-project.org/submit.html
