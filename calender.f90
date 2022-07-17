      program main
      integer ::  year=0, month=1, total=0, i=0, cnt=0
      integer ::  month_date(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)
      if (year==0) then
        print*,"西暦年を入力して下さい "
        read *, year
      end if

      i=1
      do while(i<=year-1)
        total = total + 365 + checkUruu(i)
        i=i+1
      end do
      !do month = 1, 12
      if(checkUruu(year)==1) then
        month_date(2) = month_date(2)+1
      end if
      do month = 1, 12
        print *, "	", year, "年"
        print *,  month, "月"
        write(*, fmt='(a)', advance='no') " 月 火 水 木 金 土 日"
        write(*,'(/)', advance='no')
        i = 1
        cnt = 0
        do while(i<=mod(total, 7))
          write(*, fmt='(a)', advance='no') "   "
          i=i+1
          cnt =cnt+1
        end do

        i = 1
        do while(i<=month_date(month))
          if(mod(cnt, 7) == 0) then
            write(*,'(/)',advance='no')
          end if
          write(*,'(i3)',advance='no') i
          i=i+1
          cnt=cnt+1
        end do

        write(*,'(/)')
        total = total + month_date(month)
     end do

      contains
      integer function checkUruu(i)
        if (mod(i, 400) == 0) then
          checkUruu = 1
        else if(mod(i , 4) == 0 .and. mod(i, 100) /= 0) then
          checkUruu = 1
        else
          checkUruu = 0
        end if
      end function checkUruu

      end program main
