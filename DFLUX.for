      SUBROUTINE DFLUX(FLUX,SOL,KSTEP,KINC,TIME,NOEL,NPT,COORDS,
     &				   JLTYP,TEMP,PRESS,SNAME)
C
      INCLUDE 'ABA_PARAM.INC'
C
      DIMENSION FLUX(2), TIME(2), COORDS(3)
      CHARACTER*80 SNAME
      
C     double precision alpha, p, x, z, r0, t, v, z0, r1, pi
C     time in seconds
C     t=time(2)
      pi=3.1416
C     the radius of laser spot
      r0=1.8
C     alpha_R in the paper
      alpha=0.99
C     power of the laser
      p=10000.0
C     the velocity of the laser in mm/sec
      v=9.8
C     defining position parameters
      X=COORDS(1)
	  Y=COORDS(2)
      Z=COORDS(3)
	  Dist=0.0
C     x component of the of the center of the laser spot
	  Y_center = 0.0
      X_center= 0.0 + ((25.0/2.55)*(TIME(2)))
C     the radius of the moving circle of the laser spot
	  YT=Y_center - Y
	  XT=X_center - X
	  Dist=SQRT((XT*XT)+(YT*YT))
C     defining flux parameters
	  if (Dist .le. 1.8) then
	     FLUX(1) =((alpha*p)/(pi*r0**2))
	  else
	     FLUX(1) = 0.0
	  endif

      
      RETURN
      END