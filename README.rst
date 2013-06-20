====
seas
====
:Author: Mike Toews

CRAN Submission Procedure
~~~~~~~~~~~~~~~~~~~~~~~~~

 1. Edit inst/ChangeLog to reflect version number, date and list of changes
 2. Edit DESCRIPTION to reflect version number and date
 3. From this directory, build source tar with::

    $ R CMD build seas

 4. Check build::

    $ R CMD check --as-cran seas_x.y-z.tar.gz

 5. Upload to CRAN FTP::

    $ curl -T seas_0.4-2.tar.gz ftp://cran.r-project.org/incoming/

 6. Send an email::

    :To: CRAN@R-project.org
    :Subject: CRAN submission seas x.y-z

    Hi,

    I have uploaded a new version of seas to CRAN.

    Thanks,

    -Mike
