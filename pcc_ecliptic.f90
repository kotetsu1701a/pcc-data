program pcc_ecliptic
! 平均黄道傾斜角を求める
    
    implicit none

    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: RAD = 180.0d0 / PI

    double precision, parameter :: RA =  46.093825d0
    double precision, parameter :: DC =  15.595812d0

    double precision :: julian, t
    double precision :: dy, dt
    double precision :: year, month, day
    double precision :: hour, minute, second
    double precision :: rh, rm, rs, dh, dm, ds
    double precision :: era, edc
    character(128) :: sfmt

    write(*,*)
    write(*, fmt='(a)', advance='no') 'DATE AND TIME(JST) ? '
    read(*,*) dy, dt
    write(*, *)

    year = int(dy / 10000.0d0)
    month = mod(int(dy / 100.0d0), 100)
    day = mod(dy, 100.0d0)
    hour = int(dt / 10000.0d0)
    minute = mod(int(dt / 100.0d0), 100)
    second = mod(dt, 100.0d0)

    call GetJulianDay(year, month, day, hour, minute, second, julian)

    t = (julian - 2451545.0d0) / 36525.0d0
    call EquatorialToEcliptic(t, RA, DC, era, edc)

    sfmt = '(i4, "年 ", i2, "月 ", i2, "日 ", i2, "時 ", i2, "分 ", i2, "秒")'
    write(*, sfmt) int(year), int(month), int(day), int(hour), int(minute), int(second)

    rh = int(era)
    rs = 60.0d0 * (era - rh)
    rm = int(rs)
    rs = 60.0d0 * (rs - rm)

    dh = int(edc)
    ds = 60.0d0 * (edc - dh)
    dm = int(ds)
    ds = 60.0d0 * (ds - dm)

    write(*, *)
    write(*, *) '太陽の黄経・黄緯'
    write(*, '("視黄経 = ",f10.6)') era
    write(*, '("視黄緯 = ",f10.6)') edc
    write(*, *)

    stop
    end program pcc_ecliptic
    