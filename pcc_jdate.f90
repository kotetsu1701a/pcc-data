program jdate
    ! JDからユリウス暦法への変換

        implicit none

        double precision :: julian
        double precision :: dy, dt
        double precision :: year, month, day
        double precision :: hour, minute, second
        double precision :: dtime
        character(128) :: sfmt

        write(*,*)
        write(*, fmt='(a)', advance='no') 'DATE AND TIME(JST) ? '
        read(*,*) dy, dt

        year = int(dy / 10000.0d0)
        month = mod(int(dy / 100.0d0), 100)
        day = mod(dy, 100.0d0)
        hour = int(dt / 10000.0d0)
        minute = mod(int(dt / 100.0d0), 100)
        second = mod(dt, 100.0d0)

        write(*, *)
        sfmt = '(" INPUT DATE = ", i4, "年 ", i2, "月 ", i2, "日 ", i2, "時 ", i2, "分 ", i2, "秒 (JST))")'
        write(*, sfmt) int(year), int(month), int(day), int(hour), int(minute), int(second)

        call GetJulianDay(year, month, day, hour, minute, second, julian)
        call JulianToDate(julian, year, month, day)

        dtime = 24.0d0 * (day - int(day))
        hour = int(dtime)
        second = 60.0d0 * (dtime - int(dtime))
        minute = int(second)
        second = int(60.0d0 * (second - minute))

        write(*, *)
        sfmt = '("RETURN DATE = ", i4, "年 ", i2, "月 ", i2, "日 ", i2, "時 ", i2, "分 ", i2, "秒 (UTC)")'
        write(*, sfmt) int(year), int(month), int(day), int(hour), int(minute), int(second)
        write(*, *)
        
        stop
    end program jdate
    