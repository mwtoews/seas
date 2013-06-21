/* Included with seas package for R
 * (c) Mike Toews 2007, 2013
 * License: BSD
 */

#include <stdio.h>
#include <stdlib.h>
#include <R.h>
#include <Rinternals.h>

void writeHELP(char **filename, char **header, int *type, int *startyear,
               int *nyear, double *val)
{
    int syear, eyear, yearlength;
    int yday, year, sl, ym, i;
    char *fy, *fv, *fn;
    fn = *filename;

    switch(*type){
    case 1: /* precipitation */
        fy = "%10i";
        fv = "%5.2f";
        break;
    case 2: /* precipitation Visual HELP */
        fy = "%10i";
        fv = "%6.1f";
        break;
    case 3: /* temperature */
        fy = "%5i";
        fv = "%6.1f";
        break;
    case 4: /* temperature Visual HELP */
        fy = "%5i";
        fv = "%6.1f";
        break;
    case 5: /* solar */
        fy = "%5i";
        fv = "%6.2f";
        break;
    case 6: /* solar Visual HELP */
        fy = "%5i";
        fv = "%9.2f";
        break;
    default:
        error("Unknown type!");
    }

    /* Output file */
    FILE *fp;
    if ((fp = fopen(fn, "w")) == NULL){
        error("Can't open file");
    }

    syear = *startyear;
    eyear = *startyear + *nyear;
    ym = 0;
    fprintf(fp, "%s", *header);
    fprintf(fp, "\n");

    for (year=syear; year < eyear; year++)
    {
        yearlength = 365;
        /* Determine if this is a leap year, and if so, add 1 day */
        if ((year % 4 == 0 && year % 100 != 0) || year % 400 == 0)
        {
            yearlength++;
        }
        yday = 0;
        for (sl=1; sl <= 37; sl++)
        {
            fprintf(fp, fy, year);
            for (i=0; i < 10; i++)
            {
                if (yday + 1 <= yearlength)
                {
                    fprintf(fp, fv, val[ym + yday++]);
                }
                else
                {
                    fprintf(fp, fv, 0.0);
                }
            }
            fprintf(fp, fy, sl);
            fprintf(fp, "\n");
        }
        ym += yday;
    }

    fclose(fp);
}
