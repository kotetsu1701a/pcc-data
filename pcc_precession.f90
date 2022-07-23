program pcc_precession
! 歳差による恒星位置のずれ
    implicit none
    
    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: RAD = 180.0d0 / PI

    double precision, parameter :: RA = 316.208177952d0
    double precision, parameter :: DC =  38.524915886d0
    character(64), parameter :: STAR = '61 Cyg'

    double precision :: dy, dt, julian
    double precision :: year, month, day, hour, minute, second
    double precision :: t, ra1, dc1, l2, m2, n2
    double precision :: star_position(3)
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

    call GetJulianDay(year, month, day, hour, minute, second, julian)
    call BesselianYear(julian, t)
    call Precession(t, RA, DC, star_position)    

    l2 = star_position(1)
    m2 = star_position(2)
    n2 = star_position(3)

    call Quadrant(m2, l2, ra1)
    ra1 = ra1 * RAD
    dc1 = asin(n2) * RAD

    write(*, *)
    sfmt = '(i4, "年 ", i2, "月 ", i2, "日 ", i2, "時 ", i2, "分 ", i2, "秒")'
    write(*, sfmt) int(year), int(month), int(day), int(hour), int(minute), int(second)
    write(*, '("JD = ", f14.5)') julian
    write(*, *)
    write(*, '(a)') STAR
    write(*, '("赤経 = ", f14.9)') RA
    write(*, '("赤緯 = ", f14.9)') DC
    write(*, *)
    write(*, '(a)') '歳差運動 --------------------'
    write(*, '("赤経 = ", f14.9)') ra1
    write(*, '("赤緯 = ", f14.9)') dc1
    write(*, *)

    stop
end program pcc_precession
