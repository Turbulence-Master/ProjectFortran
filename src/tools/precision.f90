module precision
  implicit none
  ! explicit visibility declaration
  private
  !self-documentation
  public :: int1,int2,int4,int8
  public :: real4,real8
  public :: ik, rk
  ! integer kinds
  integer, parameter :: int1 = selected_int_kind(1)
  integer, parameter :: int2 = selected_int_kind(2)
  integer, parameter :: int4 = selected_int_kind(8)
  integer, parameter :: int8 = selected_int_kind(10)
  ! floating point kinds
  integer, parameter :: real4 = selected_real_kind(6)
  integer, parameter :: real8 = selected_real_kind(15)
  integer, parameter :: real16 = selected_real_kind(32)
  ! generic kinds
  integer, parameter :: ik = int4   ! generic integer kind
  integer, parameter :: rk = real8 ! generic real kind
end module precision
