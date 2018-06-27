# Coding and Development Conventions {#conventions}

The authors have attempted to follow the conventions described below in the development of the SWB code.

## Data Types

In order to make linking with C libraries more straightforward, the SWB project defines all variables in terms of their equivalent C types. The Fortran 2003 ISO_C_BINDING module is used throughout the code as a means to define the variable types. SWB variables types are defined as:

Fortran KIND      | ISO_C_BINDING type
------------------|---------------
real              | c_float
double precision  | c_double
integer           | c_int
logical           | c_bool


## Variable Names ##

 - **integer**: names begin with *i*
      \n  example: iCount
 - **real**: names begin with *r* or *f* (float)
      \n  example: rValue
 - **double precision**: names begin with *dp*
      \n  example: dpValue
 - **logical**: names begin with *l*
      \n  example: lMatch
 - **character**: names begin with *s*
      \n  example: sFileName
 - **pointer**: names begin with *p*; applies to pointer of any type
      \n  example: pGrid

## Parameter Names ##

  - Parameter names are generally entirely UPPERCASE letters
  - Normal parameter names begin with the letters specified above, depending on the type of the parameter
  - Constants used throughout the code to specify program options are composed of UPPERCASE letters without a type prefix
  - Constants specifying fortran logical unit numbers (for i/o) have prefix \em LU_
  - Parameters of derived type have prefix \em T_

## Doxygen ##

Suggested minimum Doxygen elements to be inserted immediately before each subroutine or function:

 - \@brief
 - \@details
 - \@param[in]
 - \@param[out]
 - \@param[in/out]
 - \@return

