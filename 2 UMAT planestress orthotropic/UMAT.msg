1

   Abaqus 2022                                  Date 14-Apr-2025   Time 21:20:04
   For use by TECHNISCHE UNIVERSITAET HAMBURG under license from Dassault Systemes or its subsidiary.

                                                                                 
                                                                                 
 STEP    1     INCREMENT     1     STEP TIME    0.00    


                        S T E P       1     S T A T I C   A N A L Y S I S


                                                                                          

     AUTOMATIC TIME CONTROL WITH -
          A SUGGESTED INITIAL TIME INCREMENT OF                1.000E-02
          AND A TOTAL TIME PERIOD OF                            1.00    
          THE MINIMUM TIME INCREMENT ALLOWED IS                1.000E-12
          THE MAXIMUM TIME INCREMENT ALLOWED IS                5.000E-02

     LINEAR EQUATION SOLVER TYPE         DIRECT SPARSE

 CONVERGENCE TOLERANCE PARAMETERS FOR FORCE    
     CRITERION FOR RESIDUAL FORCE     FOR A NONLINEAR PROBLEM          5.000E-03
     CRITERION FOR DISP.    CORRECTION IN A NONLINEAR PROBLEM          1.000E-02
     INITIAL VALUE OF TIME AVERAGE FORCE                               1.000E-02
     AVERAGE FORCE     IS TIME AVERAGE FORCE    
     ALTERNATE CRIT. FOR RESIDUAL FORCE     FOR A NONLINEAR PROBLEM    2.000E-02
     CRITERION FOR ZERO FORCE     RELATIVE TO TIME AVRG. FORCE         1.000E-05
     CRITERION FOR RESIDUAL FORCE     WHEN THERE IS ZERO FLUX          1.000E-05
     CRITERION FOR DISP.    CORRECTION WHEN THERE IS ZERO FLUX         1.000E-03
     CRITERION FOR RESIDUAL FORCE     FOR A LINEAR INCREMENT           1.000E-08
     FIELD CONVERSION RATIO                                             1.00    
     CRITERION FOR ZERO FORCE     REL. TO TIME AVRG. MAX. FORCE        1.000E-05
     CRITERION FOR ZERO DISP.    RELATIVE TO CHARACTERISTIC LENGTH     1.000E-08

 CONVERGENCE TOLERANCE PARAMETERS FOR MOMENT   
     CRITERION FOR RESIDUAL MOMENT    FOR A NONLINEAR PROBLEM          5.000E-03
     CRITERION FOR ROTATION CORRECTION IN A NONLINEAR PROBLEM          1.000E-02
     INITIAL VALUE OF TIME AVERAGE MOMENT                              1.000E-02
     AVERAGE MOMENT    IS TIME AVERAGE MOMENT   
     ALTERNATE CRIT. FOR RESIDUAL MOMENT    FOR A NONLINEAR PROBLEM    2.000E-02
     CRITERION FOR ZERO MOMENT    RELATIVE TO TIME AVRG. MOMENT        1.000E-05
     CRITERION FOR RESIDUAL MOMENT    WHEN THERE IS ZERO FLUX          1.000E-05
     CRITERION FOR ROTATION CORRECTION WHEN THERE IS ZERO FLUX         1.000E-03
     CRITERION FOR RESIDUAL MOMENT    FOR A LINEAR INCREMENT           1.000E-08
     FIELD CONVERSION RATIO                                             1.00    
     CRITERION FOR ZERO MOMENT    REL. TO TIME AVRG. MAX. MOMENT       1.000E-05

     VOLUMETRIC STRAIN COMPATIBILITY TOLERANCE FOR HYBRID SOLIDS       1.000E-05
     AXIAL STRAIN COMPATIBILITY TOLERANCE FOR HYBRID BEAMS             1.000E-05
     TRANS. SHEAR STRAIN COMPATIBILITY TOLERANCE FOR HYBRID BEAMS      1.000E-05
     SOFT CONTACT CONSTRAINT COMPATIBILITY TOLERANCE FOR P>P0          5.000E-03
     SOFT CONTACT CONSTRAINT COMPATIBILITY TOLERANCE FOR P=0.0         0.100    
     CONTACT FORCE ERROR TOLERANCE FOR CONVERT SDI=YES                 1.00    
     DISPLACEMENT COMPATIBILITY TOLERANCE FOR DCOUP ELEMENTS           1.000E-05
     ROTATION COMPATIBILITY TOLERANCE FOR DCOUP ELEMENTS               1.000E-05

 EQUILIBRIUM WILL BE CHECKED FOR SEVERE DISCONTINUITY ITERATIONS

 TIME INCREMENTATION CONTROL PARAMETERS:
     FIRST EQUILIBRIUM ITERATION FOR CONSECUTIVE DIVERGENCE CHECK              4
     EQUILIBRIUM ITERATION AT WHICH LOG. CONVERGENCE RATE CHECK BEGINS         8
     EQUILIBRIUM ITERATION AFTER WHICH ALTERNATE RESIDUAL IS USED              9
     MAXIMUM EQUILIBRIUM ITERATIONS ALLOWED                                   16
     EQUILIBRIUM ITERATION COUNT FOR CUT-BACK IN NEXT INCREMENT               10
     MAXIMUM EQUILIB. ITERS IN TWO INCREMENTS FOR TIME INCREMENT INCREASE      4
     MAXIMUM ITERATIONS FOR SEVERE DISCONTINUITIES                            50
     MAXIMUM ATTEMPTS ALLOWED IN AN INCREMENT                                  5
     MAXIMUM DISCON. ITERS IN TWO INCREMENTS FOR TIME INCREMENT INCREASE      50
     MAXIMUM CONTACT AUGMENTATIONS FOR *SURFACE BEHAVIOR,AUGMENTED LAGRANGE   50
     CUT-BACK FACTOR AFTER DIVERGENCE                                 0.2500    
     CUT-BACK FACTOR FOR TOO SLOW CONVERGENCE                         0.5000    
     CUT-BACK FACTOR AFTER TOO MANY EQUILIBRIUM ITERATIONS            0.7500    
     CUT-BACK FACTOR AFTER TOO MANY SEVERE DISCONTINUITY ITERATIONS   0.2500    
     CUT-BACK FACTOR AFTER PROBLEMS IN ELEMENT ASSEMBLY               0.2500    
     INCREASE FACTOR AFTER TWO INCREMENTS THAT CONVERGE QUICKLY        1.500    
     MAX. TIME INCREMENT INCREASE FACTOR ALLOWED                       1.500    
     MAX. TIME INCREMENT INCREASE FACTOR ALLOWED (DYNAMICS)            1.250    
     MAX. TIME INCREMENT INCREASE FACTOR ALLOWED (DIFFUSION)           2.000    
     MINIMUM TIME INCREMENT RATIO FOR EXTRAPOLATION TO OCCUR          0.1000    
     MAX. RATIO OF TIME INCREMENT TO STABILITY LIMIT                   1.000    
     FRACTION OF STABILITY LIMIT FOR NEW TIME INCREMENT               0.9500    
     TIME INCREMENT INCREASE FACTOR BEFORE A TIME POINT                1.000    
     GLOBAL STABILIZATION CONTROL IS NOT USED

          PRINT OF INCREMENT NUMBER, TIME, ETC., EVERY    1  INCREMENTS

     THE MAXIMUM NUMBER OF INCREMENTS IN THIS STEP IS                     100

     LINEAR EXTRAPOLATION WILL BE USED

     CHARACTERISTIC ELEMENT LENGTH      2.00    

     DETAILS REGARDING ACTUAL SOLUTION WAVEFRONT REQUESTED

     DETAILED OUTPUT OF DIAGNOSTICS TO DATABASE REQUESTED

     PRINT OF INCREMENT NUMBER, TIME, ETC., TO THE MESSAGE FILE EVERY     1  INCREMENTS

     COLLECTING MODEL CONSTRAINT INFORMATION FOR OVERCONSTRAINT CHECKS

     COLLECTING STEP CONSTRAINT INFORMATION FOR OVERCONSTRAINT CHECKS


  INCREMENT     1 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                      0.992       TIME AVG. FORCE       0.992    
 LARGEST RESIDUAL FORCE            -5.733E-13   AT NODE        569   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -7.523E-02   AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.       -7.523E-02   AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     1.418E-04   TIME AVG. MOMENT      1.418E-04
 LARGEST RESIDUAL MOMENT           -1.208E-15   AT NODE        623   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -1.017E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION    -1.017E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E-02,  FRACTION OF STEP COMPLETED  1.000E-02
 STEP TIME COMPLETED       1.000E-02,  TOTAL TIME COMPLETED        1.000E-02


  INCREMENT     2 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       2.98       TIME AVG. FORCE        1.98    
 LARGEST RESIDUAL FORCE             2.061E-13   AT NODE        153   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -7.523E-02   AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.       -3.512E-14   AT NODE         24   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     8.691E-04   TIME AVG. MOMENT      5.055E-04
 LARGEST RESIDUAL MOMENT            5.718E-16   AT NODE        279   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -1.017E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION    -1.183E-15   AT NODE          4   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT
          TIME INCREMENT MAY NOW INCREASE TO    1.500E-02

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.000E-02,  FRACTION OF STEP COMPLETED  2.000E-02
 STEP TIME COMPLETED       2.000E-02,  TOTAL TIME COMPLETED        2.000E-02


  INCREMENT     3 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  1.500E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =  0.10000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =  0.10000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       5.21       TIME AVG. FORCE        3.06    
 LARGEST RESIDUAL FORCE            -3.677E-13   AT NODE        467   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.113       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.        2.638E-14   AT NODE         24   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     1.521E-03   TIME AVG. MOMENT      8.440E-04
 LARGEST RESIDUAL MOMENT           -1.133E-15   AT NODE        439   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -1.526E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION     8.886E-16   AT NODE          4   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT
          TIME INCREMENT MAY NOW INCREASE TO    2.250E-02

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  1.500E-02,  FRACTION OF STEP COMPLETED  3.500E-02
 STEP TIME COMPLETED       3.500E-02,  TOTAL TIME COMPLETED        3.500E-02


  INCREMENT     4 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  2.250E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       8.56       TIME AVG. FORCE        4.44    
 LARGEST RESIDUAL FORCE            -4.405E-13   AT NODE        389   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.169       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.       -4.979E-17   AT NODE        672   DOF  1
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     2.497E-03   TIME AVG. MOMENT      1.257E-03
 LARGEST RESIDUAL MOMENT            1.726E-15   AT NODE        336   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -2.289E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION    -4.961E-17   AT NODE        650   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT
          TIME INCREMENT MAY NOW INCREASE TO    3.375E-02

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  2.250E-02,  FRACTION OF STEP COMPLETED  5.750E-02
 STEP TIME COMPLETED       5.750E-02,  TOTAL TIME COMPLETED        5.750E-02


  INCREMENT     5 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  3.375E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       13.6       TIME AVG. FORCE        6.26    
 LARGEST RESIDUAL FORCE             6.359E-13   AT NODE        255   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.254       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.        1.067E-16   AT NODE        647   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     3.962E-03   TIME AVG. MOMENT      1.798E-03
 LARGEST RESIDUAL MOMENT           -2.870E-15   AT NODE        467   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -3.433E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION     7.117E-17   AT NODE         19   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT
          TIME INCREMENT MAY NOW INCREASE TO    5.000E-02

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  3.375E-02,  FRACTION OF STEP COMPLETED  9.125E-02
 STEP TIME COMPLETED       9.125E-02,  TOTAL TIME COMPLETED        9.125E-02


  INCREMENT     6 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       21.0       TIME AVG. FORCE        8.72    
 LARGEST RESIDUAL FORCE             1.052E-12   AT NODE        649   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.376       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.        1.887E-16   AT NODE         25   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     6.133E-03   TIME AVG. MOMENT      2.521E-03
 LARGEST RESIDUAL MOMENT           -5.524E-15   AT NODE        572   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -5.086E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION     1.410E-16   AT NODE        572   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E-02,  FRACTION OF STEP COMPLETED  0.141    
 STEP TIME COMPLETED       0.141    ,  TOTAL TIME COMPLETED        0.141    


  INCREMENT     7 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       28.5       TIME AVG. FORCE        11.5    
 LARGEST RESIDUAL FORCE             1.066E-12   AT NODE        388   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.376       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.       -2.192E-16   AT NODE        672   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     8.304E-03   TIME AVG. MOMENT      3.347E-03
 LARGEST RESIDUAL MOMENT           -5.611E-15   AT NODE        649   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -5.086E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION    -1.814E-16   AT NODE        572   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E-02,  FRACTION OF STEP COMPLETED  0.191    
 STEP TIME COMPLETED       0.191    ,  TOTAL TIME COMPLETED        0.191    


  INCREMENT     8 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       35.9       TIME AVG. FORCE        14.6    
 LARGEST RESIDUAL FORCE             1.322E-12   AT NODE        150   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.376       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.       -2.356E-16   AT NODE        649   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     1.048E-02   TIME AVG. MOMENT      4.238E-03
 LARGEST RESIDUAL MOMENT           -8.027E-15   AT NODE        517   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -5.086E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION     1.749E-16   AT NODE         20   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E-02,  FRACTION OF STEP COMPLETED  0.241    
 STEP TIME COMPLETED       0.241    ,  TOTAL TIME COMPLETED        0.241    


  INCREMENT     9 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       43.4       TIME AVG. FORCE        17.8    
 LARGEST RESIDUAL FORCE            -1.180E-12   AT NODE        361   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.376       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.        8.993E-16   AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     1.265E-02   TIME AVG. MOMENT      5.172E-03
 LARGEST RESIDUAL MOMENT            1.182E-14   AT NODE        467   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -5.086E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION     2.132E-16   AT NODE         22   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E-02,  FRACTION OF STEP COMPLETED  0.291    
 STEP TIME COMPLETED       0.291    ,  TOTAL TIME COMPLETED        0.291    


  INCREMENT    10 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       50.8       TIME AVG. FORCE        21.1    
 LARGEST RESIDUAL FORCE            -9.805E-13   AT NODE        230   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.376       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.       -9.008E-16   AT NODE        338   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     1.482E-02   TIME AVG. MOMENT      6.137E-03
 LARGEST RESIDUAL MOMENT           -1.148E-14   AT NODE        545   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -5.086E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION     3.506E-16   AT NODE        650   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E-02,  FRACTION OF STEP COMPLETED  0.341    
 STEP TIME COMPLETED       0.341    ,  TOTAL TIME COMPLETED        0.341    


  INCREMENT    11 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       58.2       TIME AVG. FORCE        24.5    
 LARGEST RESIDUAL FORCE            -1.123E-12   AT NODE        512   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.376       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.        5.159E-16   AT NODE        312   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     1.699E-02   TIME AVG. MOMENT      7.123E-03
 LARGEST RESIDUAL MOMENT           -1.433E-14   AT NODE        490   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -5.086E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION    -3.880E-16   AT NODE        650   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E-02,  FRACTION OF STEP COMPLETED  0.391    
 STEP TIME COMPLETED       0.391    ,  TOTAL TIME COMPLETED        0.391    


  INCREMENT    12 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       65.7       TIME AVG. FORCE        27.9    
 LARGEST RESIDUAL FORCE            -9.948E-13   AT NODE        253   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.376       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.       -6.782E-16   AT NODE         52   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     1.916E-02   TIME AVG. MOMENT      8.126E-03
 LARGEST RESIDUAL MOMENT           -1.631E-14   AT NODE        232   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -5.086E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION     3.732E-16   AT NODE        571   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E-02,  FRACTION OF STEP COMPLETED  0.441    
 STEP TIME COMPLETED       0.441    ,  TOTAL TIME COMPLETED        0.441    


  INCREMENT    13 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       73.1       TIME AVG. FORCE        31.4    
 LARGEST RESIDUAL FORCE             1.194E-12   AT NODE        387   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.376       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.        1.254E-15   AT NODE         26   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     2.133E-02   TIME AVG. MOMENT      9.142E-03
 LARGEST RESIDUAL MOMENT            1.813E-14   AT NODE        467   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -5.086E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION    -5.110E-16   AT NODE         25   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E-02,  FRACTION OF STEP COMPLETED  0.491    
 STEP TIME COMPLETED       0.491    ,  TOTAL TIME COMPLETED        0.491    


  INCREMENT    14 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       80.6       TIME AVG. FORCE        34.9    
 LARGEST RESIDUAL FORCE             1.165E-12   AT NODE        623   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.376       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.       -1.950E-15   AT NODE         52   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     2.352E-02   TIME AVG. MOMENT      1.017E-02
 LARGEST RESIDUAL MOMENT           -2.106E-14   AT NODE        258   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -5.086E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION     4.951E-16   AT NODE        468   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E-02,  FRACTION OF STEP COMPLETED  0.541    
 STEP TIME COMPLETED       0.541    ,  TOTAL TIME COMPLETED        0.541    


  INCREMENT    15 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       88.0       TIME AVG. FORCE        38.4    
 LARGEST RESIDUAL FORCE            -1.393E-12   AT NODE        571   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.376       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.        1.320E-15   AT NODE         77   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     2.569E-02   TIME AVG. MOMENT      1.120E-02
 LARGEST RESIDUAL MOMENT            2.797E-14   AT NODE        571   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -5.086E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION     5.474E-16   AT NODE         26   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E-02,  FRACTION OF STEP COMPLETED  0.591    
 STEP TIME COMPLETED       0.591    ,  TOTAL TIME COMPLETED        0.591    


  INCREMENT    16 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       95.5       TIME AVG. FORCE        42.0    
 LARGEST RESIDUAL FORCE            -1.194E-12   AT NODE         99   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.376       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.       -1.027E-15   AT NODE        104   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     2.787E-02   TIME AVG. MOMENT      1.225E-02
 LARGEST RESIDUAL MOMENT            1.985E-14   AT NODE        620   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -5.086E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION     6.818E-16   AT NODE        650   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E-02,  FRACTION OF STEP COMPLETED  0.641    
 STEP TIME COMPLETED       0.641    ,  TOTAL TIME COMPLETED        0.641    


  INCREMENT    17 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       103.       TIME AVG. FORCE        45.6    
 LARGEST RESIDUAL FORCE             9.379E-13   AT NODE        622   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.376       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.        1.055E-15   AT NODE        104   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     3.004E-02   TIME AVG. MOMENT      1.329E-02
 LARGEST RESIDUAL MOMENT            2.658E-14   AT NODE        362   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -5.086E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION    -8.214E-16   AT NODE        650   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E-02,  FRACTION OF STEP COMPLETED  0.691    
 STEP TIME COMPLETED       0.691    ,  TOTAL TIME COMPLETED        0.691    


  INCREMENT    18 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       110.       TIME AVG. FORCE        49.2    
 LARGEST RESIDUAL FORCE            -1.194E-12   AT NODE        124   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.376       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.       -1.519E-15   AT NODE        233   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     3.221E-02   TIME AVG. MOMENT      1.434E-02
 LARGEST RESIDUAL MOMENT            3.366E-14   AT NODE        363   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -5.086E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION     7.706E-16   AT NODE        442   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E-02,  FRACTION OF STEP COMPLETED  0.741    
 STEP TIME COMPLETED       0.741    ,  TOTAL TIME COMPLETED        0.741    


  INCREMENT    19 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       118.       TIME AVG. FORCE        52.8    
 LARGEST RESIDUAL FORCE            -1.137E-12   AT NODE        205   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.376       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.        5.052E-16   AT NODE        117   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     3.439E-02   TIME AVG. MOMENT      1.540E-02
 LARGEST RESIDUAL MOMENT            2.382E-14   AT NODE        640   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -5.086E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION     8.988E-16   AT NODE        338   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E-02,  FRACTION OF STEP COMPLETED  0.791    
 STEP TIME COMPLETED       0.791    ,  TOTAL TIME COMPLETED        0.791    


  INCREMENT    20 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       125.       TIME AVG. FORCE        56.4    
 LARGEST RESIDUAL FORCE             1.393E-12   AT NODE        310   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.376       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.        1.520E-15   AT NODE        468   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     3.656E-02   TIME AVG. MOMENT      1.646E-02
 LARGEST RESIDUAL MOMENT            4.195E-14   AT NODE        207   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -5.086E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION     6.841E-16   AT NODE        546   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E-02,  FRACTION OF STEP COMPLETED  0.841    
 STEP TIME COMPLETED       0.841    ,  TOTAL TIME COMPLETED        0.841    


  INCREMENT    21 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       133.       TIME AVG. FORCE        60.0    
 LARGEST RESIDUAL FORCE            -9.663E-13   AT NODE        259   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.376       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.        6.425E-16   AT NODE        182   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     3.873E-02   TIME AVG. MOMENT      1.752E-02
 LARGEST RESIDUAL MOMENT           -2.762E-14   AT NODE        442   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -5.086E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION    -7.613E-16   AT NODE        468   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E-02,  FRACTION OF STEP COMPLETED  0.891    
 STEP TIME COMPLETED       0.891    ,  TOTAL TIME COMPLETED        0.891    


  INCREMENT    22 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       140.       TIME AVG. FORCE        63.7    
 LARGEST RESIDUAL FORCE             1.137E-12   AT NODE        259   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.376       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.        6.828E-16   AT NODE         13   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     4.090E-02   TIME AVG. MOMENT      1.858E-02
 LARGEST RESIDUAL MOMENT            2.866E-14   AT NODE        229   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -5.086E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION     8.140E-16   AT NODE        234   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E-02,  FRACTION OF STEP COMPLETED  0.941    
 STEP TIME COMPLETED       0.941    ,  TOTAL TIME COMPLETED        0.941    


  INCREMENT    23 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  5.000E-02
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       148.       TIME AVG. FORCE        67.3    
 LARGEST RESIDUAL FORCE             1.080E-12   AT NODE        285   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -0.376       AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.        5.575E-16   AT NODE        520   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     4.308E-02   TIME AVG. MOMENT      1.965E-02
 LARGEST RESIDUAL MOMENT           -3.073E-14   AT NODE        103   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -5.086E-03   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION     1.243E-15   AT NODE        442   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  5.000E-02,  FRACTION OF STEP COMPLETED  0.991    
 STEP TIME COMPLETED       0.991    ,  TOTAL TIME COMPLETED        0.991    


  INCREMENT    24 STARTS. ATTEMPT NUMBER  1, TIME INCREMENT  8.750E-03
 
	SYMMETRIC DIRECT SPARSE SOLVER RUNNING ON
 	1 HOST:        1 MPI RANK  x 1 THREAD
 	NUMBER OF EQUATIONS:     4056
 	NUMBER OF RHS:           1
 	NUMBER OF FLOPS:         6.606e+07

     CHECK POINT   START OF SOLVER    

     CHECK POINT  END OF SOLVER       

       ELAPSED USER TIME (SEC)      =   0.0000    
       ELAPSED SYSTEM TIME (SEC)    =   0.0000    
       ELAPSED TOTAL CPU TIME (SEC) =   0.0000    
       ELAPSED WALLCLOCK TIME (SEC) =          0

               CONVERGENCE CHECKS FOR EQUILIBRIUM ITERATION     1


 AVERAGE FORCE                       149.       TIME AVG. FORCE        70.7    
 LARGEST RESIDUAL FORCE            -3.411E-13   AT NODE        137   DOF  1
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF DISP.        -6.582E-02   AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO DISP.        2.808E-16   AT NODE        676   DOF  2
   INSTANCE: LAMINA-1                                                                        
          THE FORCE     EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 AVERAGE MOMENT                     4.346E-02   TIME AVG. MOMENT      2.064E-02
 LARGEST RESIDUAL MOMENT            2.244E-14   AT NODE        491   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST INCREMENT OF ROTATION     -8.901E-04   AT NODE        652   DOF  6
   INSTANCE: LAMINA-1                                                                        
 LARGEST CORRECTION TO ROTATION     5.380E-16   AT NODE        390   DOF  6
   INSTANCE: LAMINA-1                                                                        
          THE MOMENT    EQUILIBRIUM RESPONSE WAS LINEAR IN THIS INCREMENT

 ITERATION SUMMARY FOR THE INCREMENT:   1 TOTAL ITERATIONS, OF WHICH
   0 ARE SEVERE DISCONTINUITY ITERATIONS AND  1 ARE EQUILIBRIUM ITERATIONS.

 TIME INCREMENT COMPLETED  8.750E-03,  FRACTION OF STEP COMPLETED   1.00    
 STEP TIME COMPLETED        1.00    ,  TOTAL TIME COMPLETED         1.00    


          THE ANALYSIS HAS BEEN COMPLETED



     ANALYSIS SUMMARY:
     TOTAL OF         24  INCREMENTS
                       0  CUTBACKS IN AUTOMATIC INCREMENTATION
                      24  ITERATIONS INCLUDING CONTACT ITERATIONS IF PRESENT
                      24  PASSES THROUGH THE EQUATION SOLVER OF WHICH 
                       1  INVOLVE MATRIX DECOMPOSITION, INCLUDING
                       0  DECOMPOSITION(S) OF THE MASS MATRIX
                       1  REORDERING OF EQUATIONS TO MINIMIZE WAVEFRONT
                       0  ADDITIONAL RESIDUAL EVALUATIONS FOR LINE SEARCHES
                       0  ADDITIONAL OPERATOR EVALUATIONS FOR LINE SEARCHES
                       2  WARNING MESSAGES DURING USER INPUT PROCESSING
                       0  WARNING MESSAGES DURING ANALYSIS
                       0  ANALYSIS WARNINGS ARE NUMERICAL PROBLEM MESSAGES
                       0  ANALYSIS WARNINGS ARE NEGATIVE EIGENVALUE MESSAGES
                       0  ERROR MESSAGES



     JOB TIME SUMMARY
       USER TIME (SEC)      =   1.5000    
       SYSTEM TIME (SEC)    =  0.70000    
       TOTAL CPU TIME (SEC) =   2.2000    
       WALLCLOCK TIME (SEC) =          4
