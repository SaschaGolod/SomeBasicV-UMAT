# -*- coding: mbcs -*-
#
# Abaqus/CAE Release 2022 replay file
# Internal Version: 2021_09_15-19.57.30 176069
# Run by asasc on Mon Apr 14 21:13:50 2025
#

# from driverUtils import executeOnCaeGraphicsStartup
# executeOnCaeGraphicsStartup()
#: Executing "onCaeGraphicsStartup()" in the site directory ...
#: Executing "onCaeGraphicsStartup()" in the home directory ...
from abaqus import *
from abaqusConstants import *
session.Viewport(name='Viewport: 1', origin=(0.0, 0.0), width=103.321876525879, 
    height=111.809257507324)
session.viewports['Viewport: 1'].makeCurrent()
session.viewports['Viewport: 1'].maximize()
from caeModules import *
from driverUtils import executeOnCaeStartup
executeOnCaeStartup()
#: Executing "onCaeStartup()" in the home directory ...
openMdb('WORKSHOP2.cae')
#: The model database "C:\Users\asasc\NextcloudAlterOrdner\Tutorial\Introduction to UMAT and VUMAT Subroutines-Part1\ABAQUS\WORKSHOP-2\WORKSHOP2.cae" has been opened.
session.viewports['Viewport: 1'].setValues(displayedObject=None)
session.viewports['Viewport: 1'].partDisplay.geometryOptions.setValues(
    referenceRepresentation=ON)
p = mdb.models['ABAQUS'].parts['LAMINA']
session.viewports['Viewport: 1'].setValues(displayedObject=p)
a = mdb.models['ABAQUS'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(
    optimizationTasks=OFF, geometricRestrictions=OFF, stopConditions=OFF)
del mdb.jobs['UMAT']
mdb.Job(name='UMAT', model='UMAT', description='', type=ANALYSIS, atTime=None, 
    waitMinutes=0, waitHours=0, queue=None, memory=90, memoryUnits=PERCENTAGE, 
    getMemoryFromAnalysis=True, explicitPrecision=SINGLE, 
    nodalOutputPrecision=SINGLE, echoPrint=OFF, modelPrint=OFF, 
    contactPrint=OFF, historyPrint=OFF, 
    userSubroutine='PLANESTRESS-ORTHOTROPIC.for', scratch='', 
    resultsFormat=ODB, numThreadsPerMpiProcess=1, multiprocessingMode=DEFAULT, 
    numCpus=1, numGPUs=0)
mdb.jobs['UMAT'].submit(consistencyChecking=OFF)
#: The job input file "UMAT.inp" has been submitted for analysis.
#: Job UMAT: Analysis Input File Processor completed successfully.
#: Job UMAT: Abaqus/Standard completed successfully.
#: Job UMAT completed successfully. 
mdb.jobs['ABAQUS'].submit(consistencyChecking=OFF)
#: The job input file "ABAQUS.inp" has been submitted for analysis.
#: Job ABAQUS: Analysis Input File Processor completed successfully.
#: Job ABAQUS: Abaqus/Standard completed successfully.
#: Job ABAQUS completed successfully. 
a = mdb.models['UMAT'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
a = mdb.models['UMAT'].rootAssembly
a.unlock()
session.viewports['Viewport: 1'].partDisplay.setValues(sectionAssignments=ON, 
    engineeringFeatures=ON)
session.viewports['Viewport: 1'].partDisplay.geometryOptions.setValues(
    referenceRepresentation=OFF)
p1 = mdb.models['UMAT'].parts['LAMINA']
session.viewports['Viewport: 1'].setValues(displayedObject=p1)
a = mdb.models['UMAT'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
o3 = session.openOdb(
    name='C:/Users/asasc/NextcloudAlterOrdner/Tutorial/Introduction to UMAT and VUMAT Subroutines-Part1/ABAQUS/WORKSHOP-2/UMAT.odb')
#: Model: C:/Users/asasc/NextcloudAlterOrdner/Tutorial/Introduction to UMAT and VUMAT Subroutines-Part1/ABAQUS/WORKSHOP-2/UMAT.odb
#: Number of Assemblies:         1
#: Number of Assembly instances: 0
#: Number of Part instances:     1
#: Number of Meshes:             1
#: Number of Element Sets:       4
#: Number of Node Sets:          3
#: Number of Steps:              1
session.viewports['Viewport: 1'].setValues(displayedObject=o3)
session.viewports['Viewport: 1'].makeCurrent()
a = mdb.models['UMAT'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
session.viewports['Viewport: 1'].setValues(
    displayedObject=session.odbs['C:/Users/asasc/NextcloudAlterOrdner/Tutorial/Introduction to UMAT and VUMAT Subroutines-Part1/ABAQUS/WORKSHOP-2/UMAT.odb'])
session.viewports['Viewport: 1'].odbDisplay.display.setValues(plotState=(
    CONTOURS_ON_DEF, ))
session.viewports['Viewport: 1'].view.setValues(session.views['Front'])
a = mdb.models['UMAT'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Step-1')
session.viewports['Viewport: 1'].assemblyDisplay.setValues(loads=ON, bcs=ON, 
    predefinedFields=ON, connectors=ON)
session.viewports['Viewport: 1'].view.setValues(nearPlane=122.1, 
    farPlane=160.742, width=132.808, height=58.0564, viewOffsetX=1.18658, 
    viewOffsetY=-0.140599)
session.viewports['Viewport: 1'].partDisplay.setValues(sectionAssignments=OFF, 
    engineeringFeatures=OFF)
session.viewports['Viewport: 1'].partDisplay.geometryOptions.setValues(
    referenceRepresentation=ON)
p1 = mdb.models['UMAT'].parts['LAMINA']
session.viewports['Viewport: 1'].setValues(displayedObject=p1)
p = mdb.models['UMAT'].Part(name='LAMINA-failed', 
    objectToCopy=mdb.models['UMAT'].parts['LAMINA'])
mdb.models['UMAT'].parts['LAMINA-failed'].Unlock(reportWarnings=False)
del mdb.models['UMAT'].parts['LAMINA']
mdb.models['UMAT'].parts.changeKey(fromName='LAMINA-failed', toName='LAMINA')
import assembly
mdb.models['UMAT'].rootAssembly.regenerate()
p1 = mdb.models['UMAT'].parts['LAMINA']
session.viewports['Viewport: 1'].setValues(displayedObject=p1)
p = mdb.models['UMAT'].parts['LAMINA']
session.viewports['Viewport: 1'].setValues(displayedObject=p)
session.viewports['Viewport: 1'].partDisplay.setValues(sectionAssignments=ON, 
    engineeringFeatures=ON)
session.viewports['Viewport: 1'].partDisplay.geometryOptions.setValues(
    referenceRepresentation=OFF)
p1 = mdb.models['UMAT'].parts['LAMINA']
session.viewports['Viewport: 1'].setValues(displayedObject=p1)
layupOrientation = mdb.models['UMAT'].parts['LAMINA'].datums[2]
mdb.save()
#: The model database has been saved to "C:\Users\asasc\NextcloudAlterOrdner\Tutorial\Introduction to UMAT and VUMAT Subroutines-Part1\ABAQUS\WORKSHOP-2\WORKSHOP2.cae".
a = mdb.models['UMAT'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(loads=OFF, bcs=OFF, 
    predefinedFields=OFF, connectors=OFF, adaptiveMeshConstraints=ON)
o3 = session.openOdb(
    name='C:/Users/asasc/NextcloudAlterOrdner/Tutorial/Introduction to UMAT and VUMAT Subroutines-Part1/ABAQUS/WORKSHOP-2/UMAT.odb')
session.viewports['Viewport: 1'].setValues(displayedObject=o3)
session.viewports['Viewport: 1'].makeCurrent()
a = mdb.models['UMAT'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
session.viewports['Viewport: 1'].setValues(
    displayedObject=session.odbs['C:/Users/asasc/NextcloudAlterOrdner/Tutorial/Introduction to UMAT and VUMAT Subroutines-Part1/ABAQUS/WORKSHOP-2/UMAT.odb'])
session.viewports['Viewport: 1'].assemblyDisplay.setValues(
    adaptiveMeshConstraints=OFF)
session.viewports['Viewport: 1'].odbDisplay.setPrimaryVariable(
    variableLabel='SDV1', outputPosition=INTEGRATION_POINT, )
session.viewports['Viewport: 1'].odbDisplay.contourOptions.setValues(
    numIntervals=2, maxAutoCompute=OFF, maxValue=1, minAutoCompute=OFF, 
    minValue=0)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=0)
a = mdb.models['UMAT'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(
    adaptiveMeshConstraints=ON)
mdb.models['UMAT'].steps['Step-1'].setValues(initialInc=0.1, maxInc=0.1)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(
    adaptiveMeshConstraints=OFF)
mdb.jobs['UMAT'].submit(consistencyChecking=OFF)
#: The job input file "UMAT.inp" has been submitted for analysis.
#: Job UMAT: Analysis Input File Processor completed successfully.
#: Job UMAT: Abaqus/Standard completed successfully.
#: Job UMAT completed successfully. 
session.viewports['Viewport: 1'].setValues(
    displayedObject=session.odbs['C:/Users/asasc/NextcloudAlterOrdner/Tutorial/Introduction to UMAT and VUMAT Subroutines-Part1/ABAQUS/WORKSHOP-2/UMAT.odb'])
o3 = session.openOdb(
    name='C:/Users/asasc/NextcloudAlterOrdner/Tutorial/Introduction to UMAT and VUMAT Subroutines-Part1/ABAQUS/WORKSHOP-2/UMAT.odb')
#: Model: C:/Users/asasc/NextcloudAlterOrdner/Tutorial/Introduction to UMAT and VUMAT Subroutines-Part1/ABAQUS/WORKSHOP-2/UMAT.odb
#: Number of Assemblies:         1
#: Number of Assembly instances: 0
#: Number of Part instances:     1
#: Number of Meshes:             1
#: Number of Element Sets:       4
#: Number of Node Sets:          3
#: Number of Steps:              1
session.viewports['Viewport: 1'].setValues(displayedObject=o3)
session.viewports['Viewport: 1'].makeCurrent()
session.viewports['Viewport: 1'].odbDisplay.display.setValues(plotState=(
    CONTOURS_ON_DEF, ))
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=0)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=1)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=2)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=3)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=4)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=5)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=6)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=7)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=8)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=9)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=10)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=10)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=10)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=10)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=10)
session.viewports['Viewport: 1'].odbDisplay.setPrimaryVariable(
    variableLabel='SDV1', outputPosition=INTEGRATION_POINT, )
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=0)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=1)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=2)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=3)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=4)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=5)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=6)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=7)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=8)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=9)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=10)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=10)
a = mdb.models['UMAT'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(
    adaptiveMeshConstraints=ON)
mdb.models['UMAT'].steps['Step-1'].setValues(initialInc=0.01, minInc=1e-12, 
    maxInc=0.05)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(
    adaptiveMeshConstraints=OFF)
mdb.jobs['UMAT'].submit(consistencyChecking=OFF)
#: The job input file "UMAT.inp" has been submitted for analysis.
#: Job UMAT: Analysis Input File Processor completed successfully.
#: Job UMAT: Abaqus/Standard completed successfully.
#: Job UMAT completed successfully. 
session.viewports['Viewport: 1'].setValues(
    displayedObject=session.odbs['C:/Users/asasc/NextcloudAlterOrdner/Tutorial/Introduction to UMAT and VUMAT Subroutines-Part1/ABAQUS/WORKSHOP-2/UMAT.odb'])
o3 = session.openOdb(
    name='C:/Users/asasc/NextcloudAlterOrdner/Tutorial/Introduction to UMAT and VUMAT Subroutines-Part1/ABAQUS/WORKSHOP-2/UMAT.odb')
#: Model: C:/Users/asasc/NextcloudAlterOrdner/Tutorial/Introduction to UMAT and VUMAT Subroutines-Part1/ABAQUS/WORKSHOP-2/UMAT.odb
#: Number of Assemblies:         1
#: Number of Assembly instances: 0
#: Number of Part instances:     1
#: Number of Meshes:             1
#: Number of Element Sets:       4
#: Number of Node Sets:          3
#: Number of Steps:              1
session.viewports['Viewport: 1'].setValues(displayedObject=o3)
session.viewports['Viewport: 1'].makeCurrent()
session.viewports['Viewport: 1'].odbDisplay.setPrimaryVariable(
    variableLabel='SDV1', outputPosition=INTEGRATION_POINT, )
session.viewports['Viewport: 1'].odbDisplay.display.setValues(
    plotState=CONTOURS_ON_DEF)
session.viewports['Viewport: 1'].view.setValues(session.views['Front'])
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=1)
session.viewports[session.currentViewportName].odbDisplay.setFrame(
    step='Step-1', frame=3)
a = mdb.models['UMAT'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
o3 = session.openOdb(
    name='C:/Users/asasc/NextcloudAlterOrdner/Tutorial/Introduction to UMAT and VUMAT Subroutines-Part1/ABAQUS/WORKSHOP-2/ABAQUS.odb')
#: Model: C:/Users/asasc/NextcloudAlterOrdner/Tutorial/Introduction to UMAT and VUMAT Subroutines-Part1/ABAQUS/WORKSHOP-2/ABAQUS.odb
#: Number of Assemblies:         1
#: Number of Assembly instances: 0
#: Number of Part instances:     1
#: Number of Meshes:             1
#: Number of Element Sets:       4
#: Number of Node Sets:          3
#: Number of Steps:              1
session.viewports['Viewport: 1'].setValues(displayedObject=o3)
session.viewports['Viewport: 1'].makeCurrent()
a = mdb.models['UMAT'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
session.viewports['Viewport: 1'].setValues(
    displayedObject=session.odbs['C:/Users/asasc/NextcloudAlterOrdner/Tutorial/Introduction to UMAT and VUMAT Subroutines-Part1/ABAQUS/WORKSHOP-2/ABAQUS.odb'])
session.viewports['Viewport: 1'].odbDisplay.display.setValues(plotState=(
    CONTOURS_ON_DEF, ))
session.viewports['Viewport: 1'].odbDisplay.setPrimaryVariable(
    variableLabel='TSAIH', outputPosition=INTEGRATION_POINT, )
session.Viewport(name='Viewport: 2', origin=(3.86250019073486, 
    3.04351854324341), width=221.610946655273, height=104.921295166016)
session.viewports['Viewport: 2'].makeCurrent()
session.viewports['Viewport: 2'].maximize()
session.viewports['Viewport: 1'].restore()
session.viewports['Viewport: 2'].restore()
session.viewports['Viewport: 1'].setValues(origin=(0.0, 3.04351806640625), 
    width=124.404693603516, height=108.765739440918)
session.viewports['Viewport: 2'].setValues(origin=(124.404693603516, 
    3.04351806640625), width=124.404693603516, height=108.765739440918)
odb = session.odbs['C:/Users/asasc/NextcloudAlterOrdner/Tutorial/Introduction to UMAT and VUMAT Subroutines-Part1/ABAQUS/WORKSHOP-2/UMAT.odb']
session.viewports['Viewport: 2'].setValues(displayedObject=odb)
session.viewports['Viewport: 2'].odbDisplay.display.setValues(plotState=(
    CONTOURS_ON_DEF, ))
session.viewports['Viewport: 2'].odbDisplay.setPrimaryVariable(
    variableLabel='SDV1', outputPosition=INTEGRATION_POINT, )
session.viewports['Viewport: 2'].odbDisplay.contourOptions.setValues(
    maxAutoCompute=ON, minAutoCompute=ON)
session. linkedViewportCommands.setValues(linkViewports=True)
session.viewports['Viewport: 2'].odbDisplay.contourOptions.setValues(
    maxAutoCompute=OFF, minAutoCompute=OFF)
session.viewports['Viewport: 2'].odbDisplay.contourOptions.setValues(
    maxAutoCompute=ON)
session.viewports['Viewport: 2'].animationController.setValues(
    animationType=TIME_HISTORY)
session.viewports['Viewport: 2'].animationController.play(duration=UNLIMITED)
session.viewports['Viewport: 2'].animationController.stop()
session.viewports['Viewport: 2'].odbDisplay.setPrimaryVariable(
    variableLabel='S', outputPosition=INTEGRATION_POINT, refinement=(INVARIANT, 
    'Mises'), )
mdb.save()
#: The model database has been saved to "C:\Users\asasc\NextcloudAlterOrdner\Tutorial\Introduction to UMAT and VUMAT Subroutines-Part1\ABAQUS\WORKSHOP-2\WORKSHOP2.cae".
