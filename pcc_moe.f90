program moe
! 平均黄道傾斜角を求める

    implicit none

    double precision :: julian, t, ea
    double precision :: dy, dt
    double precision :: year, month, day
    double precision :: hour, minute, second
    double precision :: eh, em, es
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

    call GetJulianDay(year, month, day, hour, minute, second, julian)
    t = (julian - 2451545.0d0) / 36525.0d0
    call MeanObliquityEcliptic(t, ea)

    write(*, *)
    sfmt = '(i4, "年 ", i2, "月 ", i2, "日 ", i2, "時 ", i2, "分 ", i2, "秒")'
    write(*, sfmt) int(year), int(month), int(day), int(hour), int(minute), int(second)

    eh = int(ea)
    es = 60.0d0 * (ea - eh)
    em = int(es)
    es = 60.0d0 * (es - em)

    write(*, *)
    write(*, '("平均黄道傾斜角 = ",i2, "h ", i2, "m ", f6.3, "s")') int(eh), int(em), es
    write(*, *)

    
    stop
end program moe
