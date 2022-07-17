      program main
      implicit none
      integer ::  year = 0, month = 1
      integer :: month_date(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)
      integer :: total = 0
      integer ::  i, cnt

      if (year==0) then
        print*,"西暦年を入力して下さい "
        read *, year
      end if

      do month = 1, 12
        i=0
        do while(i<=year-1)
          total = total + 365 + checkUruu(i)
          i=i+1
        end do

        if(checkUruu(year))
          month_date(1) = month_date(1)+1

        do while(i<month-1)
          total = total + month_date[i]
          i=i+1
        end do

        print *, "	", year, "年"
        print *,  month, "月"
        print *, " 月 火 水 木 金 土 日\n"

        i = 1
        cnt = 0
        do while(i<=mod(total, 7))
          print *, "   "
          i=i+1
          cnt =cnt+1
        end do

        i = 1
        cnt = 0
        do while(i<=month_date(month-1))
          if(cnt % 7 == 0) then
            print *, " "
          print *, i
          i=i+1
          cnt=cnt+1
        end do

        print *, " "
      end do

      contains

      function checkUruu(year) result(yes)
        if (year % 400 == 0) then
          yes = 1
          return
        else if(year % 4 == 0 && year % 100 /= 0) then
          yes = 1
          return
        end if
      end function checkUruu

      end program main
