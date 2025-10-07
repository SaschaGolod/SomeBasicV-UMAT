      SUBROUTINE VUMAT(
C Read only -
     1 NBLOCK, NDIR, NSHR, NSTATEV, NFIELDV, NPROPS, LANNEAL,
     2 STEPTIME, TOTALTIME, DT, CMNAME, COORDMP, CHARLENGTH,
     3 PROPS, DENSITY, STRAININC, RELSPININC,
     4 TEMPOLD, STRETCHOLD, DEFGRADOLD, FIELDOLD,
     5 STRESSOLD, STATEOLD, ENERINTERNOLD, ENERINELASOLD,
     6 TEMPNEW, STRETCHNEW, DEFGRADNEW, FIELDNEW,
C Write only -
     7 STRESSNEW, STATENEW, ENERINTERNNEW, ENERINELASNEW)
      
      INCLUDE 'VABA_PARAM.INC'
      
      DIMENSION PROPS(NPROPS), DENSITY(NBLOCK), COORDMP(NBLOCK),
     1 CHARLENGTH(NBLOCK), STRAININC(NBLOCK, NDIR+NSHR),
     2 RELSPININC(NBLOCK, NSHR), TEMPOLD(NBLOCK),
     3 STRETCHOLD(NBLOCK, NDIR+NSHR),DEFGRADOLD(NBLOCK,NDIR+NSHR+NSHR),
     4 FIELDOLD(NBLOCK, NFIELDV), STRESSOLD(NBLOCK, NDIR+NSHR),
     5 STATEOLD(NBLOCK, NSTATEV), ENERINTERNOLD(NBLOCK),
     6 ENERINELASOLD(NBLOCK), TEMPNEW(NBLOCK),
     7 STRETCHNEW(NBLOCK, NDIR+NSHR),DEFGRADNEW(NBLOCK,NDIR+NSHR+NSHR),
     8 FIELDNEW(NBLOCK, NFIELDV), STRESSNEW(NBLOCK,NDIR+NSHR),
     9 STATENEW(NBLOCK, NSTATEV), ENERINTERNNEW(NBLOCK),
     1 ENERINELASNEW(NBLOCK)
C
      CHARACTER*8 CMNAME     
C


C C
      parameter ( zero = 0.d0, one = 1.d0, two = 2.d0,
     * third = 1.d0 / 3.d0, half = 0.5d0, op5 = 1.5d0)
C
C For plane strain, axisymmetric, and 3D cases using
C the J2 Mises Plasticity with piecewise-linear isotropic hardening.
C
C The state variable is stored as:
C
C STATE(*,1) = equivalent plastic strain
C
C User needs to input
C props(1) Young’s modulus
C props(2) Poisson’s ratio
C props(3..) syield and hardening data
C calls vuhard for curve of yield stress vs. plastic strain
      e = props(1)
      xnu = props(2)
      twomu = e / ( one + xnu )
      alamda = xnu * twomu / ( one - two * xnu )
      thremu = op5 * twomu
      nvalue = nprops/2-1
      PRINT*,'props',props      
C
      if ( stepTime .eq. zero ) then
      do k = 1, nblock
      trace = strainInc(k,1) + strainInc(k,2) + strainInc(k,3)
      stressNew(k,1) = stressOld(k,1)
     * + twomu * strainInc(k,1) + alamda * trace
      stressNew(k,2) = stressOld(k,2)
     * + twomu * strainInc(k,2) + alamda * trace
      stressNew(k,3) = stressOld(k,3)
     * + twomu * strainInc(k,3) + alamda * trace
      stressNew(k,4)=stressOld(k,4) + twomu * strainInc(k,4)
      if ( nshr .gt. 1 ) then
      stressNew(k,5)=stressOld(k,5) + twomu * strainInc(k,5)
      stressNew(k,6)=stressOld(k,6) + twomu * strainInc(k,6)
      end if
      end do
      else
      do k = 1, nblock
      peeqOld=stateOld(k,1)
      PRINT*,'props(3)',props(3) 
      call vuhard(yieldOld, hard, peeqOld, props(3), nvalue)
      trace = strainInc(k,1) + strainInc(k,2) + strainInc(k,3)
      s11 = stressOld(k,1) + twomu * strainInc(k,1) + alamda * trace
      s22 = stressOld(k,2) + twomu * strainInc(k,2) + alamda * trace
      s33 = stressOld(k,3) + twomu * strainInc(k,3) + alamda * trace
      s12 = stressOld(k,4) + twomu * strainInc(k,4)
      if ( nshr .gt. 1 ) then
      s13 = stressOld(k,5) + twomu * strainInc(k,5)
      s23 = stressOld(k,6) + twomu * strainInc(k,6)
      end if      
      smean = third * ( s11 + s22 + s33 )
      s11 = s11 - smean
      s22 = s22 - smean
      s33 = s33 - smean
      if ( nshr .eq. 1 ) then
      vmises = sqrt( op5*(s11*s11+s22*s22+s33*s33+two*s12*s12) )
      else
      vmises = sqrt( op5 * ( s11 * s11 + s22 * s22 + s33 * s33 +
     * two * s12 * s12 + two * s13 * s13 + two * s23 * s23 ) )
      end if
C
      sigdif = vmises - yieldOld
      facyld = zero
      if ( sigdif .gt. zero ) facyld = one
      deqps = facyld * sigdif / ( thremu + hard )
C
C Update the stress
C
      yieldNew = yieldOld + hard * deqps
      factor = yieldNew / ( yieldNew + thremu * deqps )
      stressNew(k,1) = s11 * factor + smean
      stressNew(k,2) = s22 * factor + smean
      stressNew(k,3) = s33 * factor + smean
      stressNew(k,4) = s12 * factor
      if ( nshr .gt. 1 ) then
      stressNew(k,5) = s13 * factor
      stressNew(k,6) = s23 * factor
      end if
C
C Update the state variables
C
      stateNew(k,1) = stateOld(k,1) + deqps 

C Update the specific internal energy -
C
      if ( nshr .eq. 1 ) then
      stressPower = half * (
     * ( stressOld(k,1) + stressNew(k,1) ) * strainInc(k,1) +
     * ( stressOld(k,2) + stressNew(k,2) ) * strainInc(k,2) +
     * ( stressOld(k,3) + stressNew(k,3) ) * strainInc(k,3) ) +
     * ( stressOld(k,4) + stressNew(k,4) ) * strainInc(k,4)
      else
      stressPower = half * (
     * ( stressOld(k,1) + stressNew(k,1) ) * strainInc(k,1) +
     * ( stressOld(k,2) + stressNew(k,2) ) * strainInc(k,2) +
     * ( stressOld(k,3) + stressNew(k,3) ) * strainInc(k,3) ) +
     * ( stressOld(k,4) + stressNew(k,4) ) * strainInc(k,4) +
     * ( stressOld(k,5) + stressNew(k,5) ) * strainInc(k,5) +
     * ( stressOld(k,6) + stressNew(k,6) ) * strainInc(k,6)
      end if
      enerInternNew(k) = enerInternOld(k) + stressPower / density(k)  
C
C Update the dissipated inelastic specific energy -
C
      plasticWorkInc = half * ( yieldOld + yieldNew ) * deqps
      enerInelasNew(k) = enerInelasOld(k)
     * + plasticWorkInc / density(k)
      end do
      end if
C
      return
      end      

      subroutine vuhard(syield, hard, eqplas, table, nvalue)
      include 'VABA_PARAM.INC'
c
      dimension table(2, nvalue)
c
      parameter(zero=0.d0)
c
c set yield stress to last value of table, hardening to zero
c
      syield=table(1, nvalue)
      hard=zero
c
      PRINT*,'table',table
c if more than one entry, search table
c
      if(nvalue.gt.1) then
      do k1=1, nvalue-1
      eqpl1=table(2,k1+1)
      if(eqplas.lt.eqpl1) then
      eqpl0=table(2, k1)
c
c yield stress and hardening
c
      deqpl=eqpl1-eqpl0
      syiel0=table(1, k1)      
      syiel1=table(1, k1+1)
      dsyiel=syiel1-syiel0
      hard=dsyiel/deqpl
      syield=syiel0+(eqplas-eqpl0)*hard
      goto 10
      endif
      end do
10    continue
      endif
      return
      end      