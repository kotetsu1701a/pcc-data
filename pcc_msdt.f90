program pcc_msdt
! 平均恒星時を求める
    
    implicit none
    
    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: RAD = 180.0d0 / PI

    double precision :: dy, dt
    double precision :: julian
    double precision :: year, month, day
    double precision :: hour, minute, second
    double precision :: sdt, rh, rm, rs, t
    double precision :: k
    character(255) :: sfmt

    julian = 0.0d0
    k = 0.0d0

    write(*,*)
    write(*, fmt='(a)', advance='no') 'DATE AND TIME(JST) ? '
    read(*,*) dy, dt

    year = int(dy / 10000.0d0)
    month = mod(int(dy / 100.0d0), 100)
    day = mod(dy, 100.0d0)
    hour = int(dt / 10000.0d0)
    minute = mod(int(dt / 100.0d0), 100)
    second = mod(dt, 100.0d0)

    ! 天体の位置と運動による
    call GetJulianDay(year, month, day, hour, minute, second, julian)
    t = (julian - 2451545.0d0) / 36525.0d0
    call MeanSiderealTime3(julian, t, sdt)

    ! 2000年以降はこちらを使用
    !call GetJulianDay(year, month, day, hour, minute, second, julian)
    !t = (julian - 2451545.0d0) / 36525.0d0
    !call MeanSiderealTime2(t, sdt)

    ! 1900年-1999年はこちらを使用
    !year = year - 1900.0d0
    !call GetElapsTime(year, month, day, k)
    !year = year + 1900.0d0
    !t = k / 36525.0d0
    !call MeanSiderealTime(t, sdt)

    write(*, *)
    sfmt = '(i4, "年 ", i2, "月 ", i2, "日 ", i2, "時 ", i2, "分 ", i2, "秒")'
    write(*, sfmt) int(year), int(month), int(day), int(hour), int(minute), int(second)

    rh = int(sdt)
    rs = 60.0d0 * (sdt - rh)
    rm = int(rs)
    rs = 60.0d0 * (rs - rm)

    write(*, *)
    write(*, '("平均恒星時 = ",i2, "h ", i2, "m ", f6.3, "s")') int(rh), int(rm), rs
    write(*, *)

    stop
end program pcc_msdt
