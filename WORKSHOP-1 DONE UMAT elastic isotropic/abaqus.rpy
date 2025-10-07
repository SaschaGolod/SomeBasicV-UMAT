# -*- coding: mbcs -*-
#
# Abaqus/CAE Release 2022 replay file
# Internal Version: 2021_09_15-19.57.30 176069
# Run by asasc on Mon Apr 21 13:12:38 2025
#

# from driverUtils import executeOnCaeGraphicsStartup
# executeOnCaeGraphicsStartup()
#: Executing "onCaeGraphicsStartup()" in the site directory ...
#: Executing "onCaeGraphicsStartup()" in the home directory ...
from abaqus import *
from abaqusConstants import *
session.Viewport(name='Viewport: 1', origin=(0.0, 0.0), width=113.943748474121, 
    height=105.562042236328)
session.viewports['Viewport: 1'].makeCurrent()
session.viewports['Viewport: 1'].maximize()
from caeModules import *
from driverUtils import executeOnCaeStartup
executeOnCaeStartup()
#: Executing "onCaeStartup()" in the home directory ...
openMdb('Workshop1.cae')
#: The model database "C:\Users\asasc\NextcloudAlterOrdner\Tutorial\Introduction to UMAT and VUMAT Subroutines-Part1\ABAQUS\WORKSHOP-1 DONE\Workshop1.cae" has been opened.
session.viewports['Viewport: 1'].setValues(displayedObject=None)
session.viewports['Viewport: 1'].partDisplay.geometryOptions.setValues(
    referenceRepresentation=ON)
p = mdb.models['ABAQUS'].parts['Beam']
session.viewports['Viewport: 1'].setValues(displayedObject=p)
a = mdb.models['ABAQUS'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(
    optimizationTasks=OFF, geometricRestrictions=OFF, stopConditions=OFF)
mdb.jobs['ABAQUS'].writeInput(consistencyChecking=OFF)
#: The job input file has been written to "ABAQUS.inp".
mdb.jobs['PleaseError'].writeInput(consistencyChecking=OFF)
#: The job input file has been written to "PleaseError.inp".
mdb.jobs['UMAT'].writeInput(consistencyChecking=OFF)
#: The job input file has been written to "UMAT.inp".
