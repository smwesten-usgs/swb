#!/usr/bin/python

import numpy as np
import os
from datetime import datetime, date, time
from matplotlib import pyplot as P
from matplotlib import colors

rRLE_MULT = 10000.

def writeArcASCII(filename, iNX, iNY, rX0, rY0, rGridCellSize, npVals):
    ofp = open(filename,'w')        
    
    ofp.write(str("NCOLS         {0}\n").format(iNX))
    ofp.write(str("NROWS         {0}\n").format(iNY))
    ofp.write(str("XLLCORNER     {0}\n").format(str(rX0)))
    ofp.write(str("YLLCORNER     {0}\n").format(str(rY0)))
    ofp.write(str("CELLSIZE      {0}\n").format(str(rGridCellSize)))
    ofp.write("NODATA_VALUE  -9999.0\n")

    ofp.close()    
    # reopen file in 'append' mode and pass this filehandle to np.savetxt
    ofp = open(filename,'a')
    np.savetxt(ofp, npVals, delimiter=" ", fmt = "%f9.3")
    
def readSWBbinary(ifp):
    """
    RLE routine ported from FORTRAN SWB code. 
    
    Each subsequent call to this routine returns a grid associated with a days worth of output.
    """
    iCurr = np.fromfile(ifp,dtype='i',count=1)
    lRun = False
    iByteCount = 0
    
    rValues = np.zeros(iNumElements,dtype=np.float32)
    
    while True:
        iPrevious = iCurr
        iCurr = np.fromfile(ifp,dtype='i',count=1)
        if iPrevious <> iCurr:
            iByteCount += 1
            lRun = False
            rValues[iByteCount - 1] = iPrevious / rRLE_MULT
        elif iPrevious == iCurr:
            lRun = True
            iByteCount += 1
            rValues[iByteCount -1] = iCurr / rRLE_MULT               
            iRepeat = np.fromfile(ifp,dtype='i',count=1)
            for j in range(iRepeat,0,-1):
                iByteCount += 1
                rValues[iByteCount - 1] = iCurr / rRLE_MULT   
        else:
            print "Problem with loop logic.  Terminating."
            exit
            
        if iByteCount == iNumElements:
            break
        
#    print "  min:    {0:.3f}".format(float(rValues.min()))
#    print "  mean:   {0:.3f}".format(rValues.mean())    
#    print "  max:    {0:.3f}".format(float(rValues.max())    )
    
    return rValues

def makePlot(rPlotVals, iYYYY, iMM, iDD, fileprefix, sTitleTxt):
    """ 
    Call Matplotlib functions and methods to produce a filled contour plot of 
    SWB output.
    """
    P.figure()
    im = P.imshow(rPlotVals, interpolation = 'nearest', vmin=0.,vmax=0.8)
    P.xlim(0,iNX-1)
    P.ylim(0,iNY-1)
    P.colorbar(im) 
    P.axis('off')
    P.title(sTitleTxt)
    P.savefig(fileprefix + '.png')

    
#os.chdir('D:/SMWData/Source_Code/SWB_TEST_CASES/Peterson_June_2013/output')

#
# ------------------ BEGIN MAIN CODE --------------------
#



ifp = open('output/swb__RUNOFF_OUTSIDE.bin','rb')

# read in HEADER values from Fortran binary file
iNX = np.fromfile(ifp,dtype='i',count=1)
iNY = np.fromfile(ifp,dtype='i',count=1)
iDataType = np.fromfile(ifp,dtype='i',count=1)
rGridCellSize = np.fromfile(ifp,dtype='d',count=1)
iLengthUnits = np.fromfile(ifp,dtype='i',count=1)
sSWBCompilationDate = np.fromfile(ifp,dtype='a15',count=1)
iVariableNumber = np.fromfile(ifp,dtype='i',count=1)
iRLE_MULT = np.fromfile(ifp,dtype='i',count=1)
rRLE_OFFSET = np.fromfile(ifp,dtype='f',count=1)
rX0, rX1 = np.fromfile(ifp,dtype='d',count=2)
rY0, rY1 = np.fromfile(ifp,dtype='d',count=2)

iLengthOfPROJ4String = np.fromfile(ifp,dtype='i',count=1)
formatStr = "a{0}".format(iLengthOfPROJ4String[0])

print "'" + formatStr + "'\n"

sPROJ4_string = np.fromfile(ifp,dtype=formatStr,count=1)

iStartMM, iStartDD, iStartYYYY = np.fromfile(ifp,dtype='i',count=3)
iEndMM, iEndDD, iEndYYYY = np.fromfile(ifp,dtype='i',count=3)


#V = np.linspace(0.,0.5,10)

print "\nGrid summary:\n"
print "iNX:              {0}".format(iNX)
print "iNY:              {0}".format(iNY)
print "iDataType:        {0}".format(iDataType)
print "rGridCellSize:    {0}".format(rGridCellSize)
print "iLengthUnits:        {0}".format(iLengthUnits)
print "rX0, rX1:         ({0:.2f},{1:.2f})".format(rX0, rX1)
print "rY0, rY1:         ({0:.2f},{1:.2f})".format(rY0, rY1)
print "Start Date:       {0}/{1}/{2}".format(iStartMM,iStartDD,iStartYYYY)
print "End Date:         {0}/{1}/{2}".format(iEndMM,iEndDD,iEndYYYY)

iNumElements = iNX * iNY

iEndDate = date(2100, 02, 28)

while True:
    
    # read in the current day, month, year, and day of year from file
    iDD, iMM, iYYYY, iDOY = np.fromfile(ifp,dtype='i',count=4)

    iCurrDate = date(iYYYY, iMM, iDD)
    
    fileprefix = str(iYYYY)+"_"+str(iMM)+"_"+str(iDD)
    sTitleTxt = "current date: {0}/{1}/{2}    day of year = {3}".format(iMM,iDD,iYYYY,iDOY)
    rValues = readSWBbinary(ifp)    
    npPlotVals = np.flipud(rValues.reshape(iNY,iNX,order='C'))
    # good reference for new and old Python format specifiers: http://mkaz.com/solog/python/python-string-format.html
    print "current date: %i/%i/%i day of year = %i; min = %.3f  mean = %.3f max = %.3f" % (iMM,iDD,iYYYY,iDOY, 
                                                                                               npPlotVals.min(), 
                                                                                               npPlotVals.mean(), 
                                                                                               npPlotVals.max())
    
    makePlot(npPlotVals, iYYYY, iMM, iDD, fileprefix, sTitleTxt)

    filename = fileprefix + '.asc'
    ofp = open(filename,'w')    
    writeArcASCII(filename, iNX, iNY, rX0, rY0, rGridCellSize, npPlotVals)
    ofp.close()    
    ofp = file(filename,'a')    
            
    if iCurrDate >= iEndDate:
        break
        
