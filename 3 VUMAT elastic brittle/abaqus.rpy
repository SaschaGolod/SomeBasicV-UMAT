# -*- coding: mbcs -*-
#
# Abaqus/CAE Release 2022 replay file
# Internal Version: 2021_09_15-19.57.30 176069
# Run by asasc on Tue Sep 30 09:37:22 2025
#

# from driverUtils import executeOnCaeGraphicsStartup
# executeOnCaeGraphicsStartup()
#: Executing "onCaeGraphicsStartup()" in the site directory ...
#: Executing "onCaeGraphicsStartup()" in the home directory ...
from abaqus import *
from abaqusConstants import *
session.Viewport(name='Viewport: 1', origin=(0.0, 0.0), width=97.0453109741211, 
    height=116.294448852539)
session.viewports['Viewport: 1'].makeCurrent()
session.viewports['Viewport: 1'].maximize()
from caeModules import *
from driverUtils import executeOnCaeStartup
executeOnCaeStartup()
#: Executing "onCaeStartup()" in the home directory ...
openMdb('Workshop1.cae')
#: The model database "C:\Users\asasc\NextcloudAlterOrdner\Tutorial\Introduction to UMAT and VUMAT Subroutines-Part1\ABAQUS\WORKSHOP-3 Done VUMAT elastic brittle\Workshop1.cae" has been opened.
session.viewports['Viewport: 1'].setValues(displayedObject=None)
session.viewports['Viewport: 1'].partDisplay.geometryOptions.setValues(
    referenceRepresentation=ON)
p = mdb.models['UMAT'].parts['Part-1']
session.viewports['Viewport: 1'].setValues(displayedObject=p)
a = mdb.models['UMAT'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
o3 = session.openOdb(
    name='C:/Users/asasc/NextcloudAlterOrdner/Tutorial/Introduction to UMAT and VUMAT Subroutines-Part1/ABAQUS/WORKSHOP-3 Done VUMAT elastic brittle/UMAT.odb')
#* OdbError: The database is from a previous release of Abaqus. 
#* Run abaqus -upgrade -job <newFileName> -odb <oldOdbFileName> to upgrade it.
session.viewports['Viewport: 1'].assemblyDisplay.setValues(
    optimizationTasks=OFF, geometricRestrictions=OFF, stopConditions=OFF)
from  abaqus import session
session.upgradeOdb(
    "C:/Users/asasc/NextcloudAlterOrdner/Tutorial/Introduction to UMAT and VUMAT Subroutines-Part1/ABAQUS/WORKSHOP-3 Done VUMAT elastic brittle/UMAT.odb", 
    "C:/Users/asasc/AppData/Local/Temp/UMAT1759217875.113.odb", )
from  abaqus import session
o7 = session.openOdb(
    'C:/Users/asasc/AppData/Local/Temp/UMAT1759217875.113.odb')
#: Model: C:/Users/asasc/AppData/Local/Temp/UMAT1759217875.113.odb
#: Number of Assemblies:         1
#: Number of Assembly instances: 0
#: Number of Part instances:     1
#: Number of Meshes:             1
#: Number of Element Sets:       3
#: Number of Node Sets:          3
#: Number of Steps:              1
session.viewports['Viewport: 1'].setValues(displayedObject=o7)
session.viewports['Viewport: 1'].odbDisplay.display.setValues(plotState=(
    CONTOURS_ON_DEF, ))
session.viewports['Viewport: 1'].view.setValues(session.views['Front'])
session.viewports['Viewport: 1'].view.setValues(nearPlane=157.384, 
    farPlane=204.442, width=136.544, height=61.2344, viewOffsetX=2.04533, 
    viewOffsetY=0.60899)
session.viewports['Viewport: 1'].animationController.setValues(
    animationType=TIME_HISTORY)
session.viewports['Viewport: 1'].animationController.play(duration=UNLIMITED)
session.viewports['Viewport: 1'].animationController.setValues(
    animationType=NONE)
mdb.save()
#: The model database has been saved to "C:\Users\asasc\NextcloudAlterOrdner\Tutorial\Introduction to UMAT and VUMAT Subroutines-Part1\ABAQUS\WORKSHOP-3 Done VUMAT elastic brittle\Workshop1.cae".
