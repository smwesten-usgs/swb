#define PJ_LIB__
#include <projects.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <locale.h>

/************************************************************************/
/*                            pj_init_and_transform()                   */
/*                                                                      */
/* Creates two pj objects representing the two projections of interest  */
/* and then makes the call to pj_transform. This hides C pointers from  */
/* external packages such as those written in fortran.                  */
/************************************************************************/
int pj_init_and_transform( const char *from_projection, const char *to_projection, \
   long point_count, double *x, double *y) {

	 projPJ pj_from, pj_to;
	 int i;
	 int p;

    if (!(pj_from = pj_init_plus(from_projection)) ) {
	    printf("\nPROJ4 ERROR: There was a problem creating a PROJ4 object; "
	      "something is\nwrong with the PROJ4 string you supplied."
	      "\nOffending string: '%s'",from_projection);
        exit(1);
	}

    if (!(pj_to = pj_init_plus(to_projection)) ) {
	    printf("\nPROJ4 ERROR: There was a problem creating a PROJ4 object; "
	      "something is\nwrong with the PROJ4 string you supplied."
	      "\nOffending string: '%s'",to_projection);
        exit(1);
	}

    p = pj_transform(pj_from, pj_to, point_count, 1, x, y, NULL );

    pj_free(pj_to);
    pj_free(pj_from);

	return(p);
}
