program pcc_ec2hc
! 天体の高度・方位角を求める
    implicit none
    
    double precision, parameter :: LA =  35.788888889d0
    double precision, parameter :: LG = 139.531555556d0
    double precision, parameter :: RA = 316.166395833d0
    double precision, parameter :: DC =  38.499750000d0
    character(64), parameter :: STAR = '61 Cyg'

    double precision :: dy, dt
    double precision :: year, month, day, hour, minute, second
    double precision :: t, k, laps, long
    double precision :: meansdt, localsdt
    double precision :: al, hi
    character(255) :: sfmt

    write(*,*)
    write(*, fmt='(a)', advance='no') 'DATE AND TIME(JST) ? '
    read(*,*) dy, dt

    year = int(dy / 10000.0d0)
    month = mod(int(dy / 100.0d0), 100)
    day = mod(dy, 100.0d0)
    hour = int(dt / 10000.0d0)
    minute = mod(int(dt / 100.0d0), 100)
    second = mod(dt, 100.0d0)

    ! グリニッジ平均恒星時
    year = year - 1900.0d0
    call GetElapsTime(year, month, day, k)
    year = year + 1900.0d0
    t = k / 36525.0d0
    call MeanSiderealTime(t, meansdt)

    ! 地方恒星時
    laps = (hour - 9) * 3600.0d0 + minute * 60.0d0
    long = LG / 15.0d0
    call localsiderealtime(meansdt, Long, laps, localsdt)

    ! 高度・方位角
    call AltitudeAzimuth(localsdt, LA, RA, DC, al, hi)

    write(*, *)
    sfmt = '(i4, "年 ", i2, "月 ", i2, "日 ", i2, "時 ", i2, "分 ", i2, "秒")'
    write(*, sfmt) int(year), int(month), int(day), int(hour), int(minute), int(second)
    write(*, *)
    write(*, '(a)') STAR
    write(*, '("高度 = ", f9.6, "  方位角 = ", f10.6)') hi, al
    write(*, *)

    stop
end program pcc_ec2hc
