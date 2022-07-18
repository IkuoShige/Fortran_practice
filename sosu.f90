program main
integer :: n=0, i=1, sosu_pair=0, super_sosu_pair=0
write (*, *)"input number: "
read *, n
do while(i<n-6)
  if (checksosu(i)==1 .and. checksosu(i+2)==1 .and. checksosu(i+6)==1) then
    if (checksosu(i+i+2+i+6)==1) then
      write (*, *) "(", i, ",", i+2, ",", i+6, ")", "*"
      super_sosu_pair = super_sosu_pair + 1
      sosu_pair = sosu_pair + 1
    else 
      write (*, *) "(", i, ",", i+2, ",", i+6, ")"
      sosu_pair = sosu_pair + 1
    end if
  end if
  if (checksosu(i)==1 .and. checksosu(i+4)==1 .and. checksosu(i+6)==1) then
    if (checksosu(i+i+4+i+6)==1) then
      write (*, *) "(", i, ",", i+4, ",", i+6, ")", "*"
      super_sosu_pair = super_sosu_pair + 1
      sosu_pair = sosu_pair + 1
    else 
      write (*, *) "(", i, ",", i+4, ",", i+6, ")"
      sosu_pair = sosu_pair + 1
    end if
  end if
  i=i+1
end do
write(*,'(/)')
write(*,*) sosu_pair, " Triplet primes"
write(*,*) super_sosu_pair, " Super triplet primes"
contains
integer function checksosu(n)
  integer :: i=0
  if (n < 2) then
    checksosu = 0
    return
  end if
  i = 2
  do while(i<n)
    if (mod(n,i)==0) then
      checksosu = 0
      return
    end if
  i=i+1
  end do
  checksosu = 1
  return 
end function checksosu
end program main
