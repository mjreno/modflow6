!> @brief MODFLOW 6 main entry point
!!
!! This is the main entry point for the MODFLOW 6 program.
!!
!<
program mf6
  ! -- modules
  use Mf6CoreModule
  use, intrinsic :: ieee_arithmetic, only: ieee_get_underflow_mode
  use, intrinsic :: iso_fortran_env, only: error_unit
  !
  ! -- DISPOSABLE diagnostic: report whether the FP environment uses gradual
  ! underflow (denormals) or abrupt underflow (flush-to-zero) at startup, to
  ! check whether a linked PETSc/MPI/NetCDF runtime alters this globally on
  ! the extended Windows build vs. the plain build. Not for merging. Written
  ! to stderr so it doesn't corrupt tests (e.g. test_cli_version) that parse
  ! mf6's stdout.
  block
    logical :: gradual_underflow
    call ieee_get_underflow_mode(gradual_underflow)
    write (error_unit, '(a,l1)') &
      'DISPOSABLE DEBUG: ieee gradual underflow mode = ', gradual_underflow
  end block
  !
  ! -- run
  call Mf6Run()

end program
