!----------------------------------------------------------------------------------
! IMPT, ProjectFortran
!----------------------------------------------------------------------------------
! 
! MODULE: Numerical Integration
! 
!> @author
!> Module Author Alejandro Benitez
! 
! DESCRIPTION: 
!> Solves the integral of a function, giving the option for using different rules: 
! Rectangular 
! Trapezoidal 
! Mid Point 
! Simpson. 
! 
! Each one is shown as a subroutine
! 
! REVISION HISTORY:
! 03 11 2016 - Initial Version
!---------------------------------------------------------------------------------

module numerical_integration
    USE precision    
    IMPLICIT NONE

!---------------------------------------------------------------------------------
! Rectangular subroutine:
! 
! N = Number of intervals
! j = counter
! a = Lower limit of the function
! h = Uniform subinterval (delta x)
! x = Array for the different values of subintervals
! r = Array for the calculated solutions at each subinterval
! Arear = Value of the area for the rectangular rule
!---------------------------------------------------------------------------------

CONTAINS
    SUBROUTINE rectangular(N,j,h,a,Arear)
        IMPLICIT NONE
        INTEGER(ik),INTENT(IN) :: N,j		!Definition of variables at subroutine
        REAL(rk),INTENT(INOUT) :: Arear,h,a
        REAL(rk),DIMENSION(0:N) :: x,r
        x(j) = a+j*h				!Discretization
        if(j/=N) then
          r(j) = 3+2*x(j)+5*x(j)*x(j)		!Evaluation of the function at each point
          Arear = Arear + h*r(j)		!Value of area after each iterarion
        end if
    END SUBROUTINE rectangular

!---------------------------------------------------------------------------------
! Trapezoidal subroutine:
! 
! N = Number of intervals
! j = counter
! a = Lower limit of the function
! h = Uniform subinterval (delta x)
! x = Array for the different values of subintervals
! r = Array for the calculated solutions at each subinterval
! Areat = Value of the area for the trapezoidal rule
!---------------------------------------------------------------------------------

    SUBROUTINE trapezoidal(N,j,h,a,Areat)
        IMPLICIT NONE
        INTEGER(ik),INTENT(IN) :: N,j		!Definition of variables at subroutine
        REAL(rk),INTENT(INOUT) :: Areat,h,a
        REAL(rk),DIMENSION(0:N) :: x,r
        x(j) = a+j*h				!Discretization
        r(j) = 3+2*x(j)+5*x(j)*x(j)		!Evaluation of the function at each point
        if(j==0 .OR. j==N) then
            Areat = Areat + h*(r(j)/2)		!\
        else					!-> Value of area after each iteration
        Areat = Areat + h*r(j)  		!/
        end if
    END SUBROUTINE trapezoidal

!---------------------------------------------------------------------------------
! Mid Point subroutine:
! 
! N = Number of intervals
! j = counter
! a = Lower limit of the function
! h = Uniform subinterval (delta x)
! x = Array for the different values of subintervals
! Aream = Value of the area for the Mid Point rule
!---------------------------------------------------------------------------------

    SUBROUTINE mid_point(N,j,h,a,Aream)
        IMPLICIT NONE
        INTEGER(ik),INTENT(IN) :: N,j		!Definition of variables at subroutine
        REAL(rk),INTENT(INOUT) :: Aream,h,a
        REAL(rk),DIMENSION(0:N) :: x
        x(j) = (a+h)/2 + h*j			!Discretization
        if(j<N) then
          Aream=Aream+h*(3+2*x(j)+5*x(j)*x(j))	!Value of area after each iterarion
        end if
    END SUBROUTINE mid_point

!---------------------------------------------------------------------------------
! Simpson subroutine:
! 
! N = Number of intervals
! j = counter
! a = Lower limit of the function
! h = Uniform subinterval (delta x)
! x = Array for the different values of subintervals
! x1 = Array for the different values of next point of actual subinterval
! x2 = Array for the different values of next two points of actual subinterval
! 
!  l------l------l------l------l      <--- Little explanation of x,x1 & x2
!  a      x      x1     x2     b
! 
! Areas = Value of the area for the Simpson rule
!---------------------------------------------------------------------------------

    SUBROUTINE simpson(N,j,h,a,Areas)
        IMPLICIT NONE
        INTEGER(ik),INTENT(IN) :: N,j		!Definition of variables at subroutine
        REAL(rk),INTENT(INOUT) :: Areas,h,a
        REAL(rk),DIMENSION(0:N) :: x,x1,x2
        if(N==1) then				!Logical condition, Simpson needs at least 3
          Areas = 0				!points, so 0 and 1 aren't valid values for N
        end if
        x(j) = a+j*h				!\
        x1(j) = x(j)+h				!-> Discretization of x,x1 & x2
        x2(j) = x1(j)+h				!/
        if(MOD(j,2)==0 .AND. j<N) then
          Areas = Areas + (h/3)*((3+2*x(j)+5*(x(j)**2))+4*((3+2*x1(j)+5*(x1(j)**2)))+(3+2*x2(j)+5*(x2(j)**2)))
        end if					!Value of area after each iteration
    END SUBROUTINE simpson
end module numerical_integration
