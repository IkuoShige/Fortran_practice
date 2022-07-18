program main
integer :: n=0, i=1, prime_number_number_pair=0, super_prime_number_number_pair=0
write (*, *)"input number: "
read *, n
do while(i<n-6)
  if (check_prime_number_number(i)==1 .and. check_prime_number_number(i+2)==1 .and. check_prime_number_number(i+6)==1) then
    if (check_prime_number_number(i+i+2+i+6)==1) then
      write (*, *) "(", i, ",", i+2, ",", i+6, ")", "*"
      super_prime_number_number_pair = super_prime_number_number_pair + 1
      prime_number_number_pair = prime_number_number_pair + 1
    else 
      write (*, *) "(", i, ",", i+2, ",", i+6, ")"
      prime_number_number_pair = prime_number_number_pair + 1
    end if
  end if
  if (check_prime_number_number(i)==1 .and. check_prime_number_number(i+4)==1 .and. check_prime_number_number(i+6)==1) then
    if (check_prime_number_number(i+i+4+i+6)==1) then
      write (*, *) "(", i, ",", i+4, ",", i+6, ")", "*"
      super_prime_number_number_pair = super_prime_number_number_pair + 1
      prime_number_number_pair = prime_number_number_pair + 1
    else 
      write (*, *) "(", i, ",", i+4, ",", i+6, ")"
      prime_number_number_pair = prime_number_number_pair + 1
    end if
  end if
  i=i+1
end do
write(*,'(/)')
write(*,*) prime_number_number_pair, " Triplet primes"
write(*,*) super_prime_number_number_pair, " Super triplet primes"
contains
integer function check_prime_number_number(n)
  integer :: i=0
  if (n < 2) then
    check_prime_number_number = 0
    return
  end if
  i = 2
  do while(i<n)
    if (mod(n,i)==0) then
      check_prime_number_number = 0
      return
    end if
  i=i+1
  end do
  check_prime_number_number = 1
  return 
end function check_prime_number_number
end program main
