! $ brew install gcc
! $ gfortran -O3 -o envelopes_f08 envelopes.f08
! $ ./envelopes_f08

function single_trial(cutoff) result(value)
  ! Runs a single trial where an envelope is chosen.  If the chosen envelope has
  ! a value < cutoff, the function will switch envelopes, otherwise it will keep
  ! the envelope it has chosen. Returns the value of the envelope it ultimately 
  ! selects.
  implicit none
  double precision :: cutoff, higher_value, lower_value, value
  logical          :: picked_lower
 
  lower_value = rand() * 100.0d+0
  higher_value = 2 * lower_value
  picked_lower = (rand() < 0.5)
  if ((picked_lower .and. lower_value >= cutoff) .or. &
      (.not. picked_lower .and. higher_value < cutoff)) then
    value = lower_value
  else
    value = higher_value
 end if
end function single_trial


function multi_trial(cutoff) result(expected_value)
  ! Runs many trials at a given cutoff to approximate the expected value.
  implicit none
  integer          :: cutoff, i, num_trials
  double precision :: cutoff64, expected_value, single_trial, total

  num_trials = 10000
  cutoff64 = dble(cutoff)
  total = 0.0d+0
  do i = 1, num_trials
    total = total + single_trial(cutoff64)
  end do
  expected_value = total / dble(num_trials)
end function multi_trial


subroutine print_result(cutoff, expected_value)
  ! Formats the results nicely and outputs them.
  implicit none
  integer            :: cutoff
  double precision   :: expected_value
  character(len=100) :: fmt

  if (cutoff < 10) then
    fmt = "(A, I1, A, F9.6)"
  else if (cutoff < 100) then
    fmt = "(A, I2, A, F9.6)"
  else
    fmt = "(A, I3, A, F9.6)"
  end if
  write (*, fmt) "cutoff=", cutoff, ", expected_value=", expected_value
end subroutine print_result


program envelopes
  ! Approximates the expected value for each integral cutoff value.
  implicit none
  integer          :: cutoff
  double precision :: multi_trial

  call random_seed()
  do cutoff = 0, 200
    call print_result(cutoff, multi_trial(cutoff))
  end do
end program envelopes