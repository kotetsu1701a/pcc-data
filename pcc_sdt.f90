program pcc_sdt
! 地方恒星時を求める

    implicit none

    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: RAD = 180.0d0 / PI

    double precision :: dy, dt
    double precision :: year, month, day, hour, minute, second
    double precision :: gh, gm, gs
    double precision :: sdt, rh, rm, rs
    double precision :: long, gsdt, laps

    character(255) :: sfmt

    write(*,*)
    write(*, fmt='(a)', advance='no') 'DATE AND TIME(JST) ? '
    read(*,*) dy, dt
    write(*, fmt='(a)', advance='no') 'Longitude ? '
    read(*,*) long
    write(*, fmt='(a)', advance='no') 'Greenwich sidereal time ? '
    read(*,*) gsdt

    year = int(dy / 10000.0d0)
    month = mod(int(dy / 100.0d0), 100)
    day = mod(dy, 100.0d0)
    hour = int(dt / 10000.0d0)
    minute = mod(int(dt / 100.0d0), 100)
    second = mod(dt, 100.0d0)

    gh = int(gsdt / 10000.0d0)
    gm = mod(int(gsdt / 100.0d0), 100)
    gs = mod(gsdt, 100.0d0)

    write(*, *)
    sfmt = '(i4, "年 ", i2, "月 ", i2, "日 ", i2, "時 ", i2, "分 ", i2, "秒")'
    write(*, sfmt) int(year), int(month), int(day), int(hour), int(minute), int(second)

    long = long / 15.0d0
    gsdt = gh + gm / 60.0d0 + gs / 3600.0d0
    laps = (hour - 9) * 3600.0d0 + minute * 60.0d0
    call localsiderealtime(gsdt, long, laps, sdt)

    long = long * 15.0d0
    rh = int(sdt)
    rs = 60.0d0 * (sdt - rh)
    rm = int(rs)
    rs = 60.0d0 * (rs - rm)

    write(*, '("東経 = ", f11.7)') long
    write(*, '("地方恒星時 = ",i2, "h ", i2, "m", f6.3, "s")') int(rh), int(rm), rs
    write(*, *)

    stop
end program pcc_sdt
