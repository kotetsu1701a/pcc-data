program pcc_sunecliptic
! 太陽黄経の略算式による計算
    
    implicit none
    
    double precision :: dy, dt, julian
    double precision :: year, month, day, hour, minute, second
    double precision :: d, t, eh, em, es, ecl, rd
    double precision :: sun_ecliptic(2)
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
    
    d = julian - 2451545.0d0
    t = d / 36525d0
    call SunEclipticLongitude(t, sun_ecliptic)
    ecl = sun_ecliptic(1)
    rd  = sun_ecliptic(2)

    eh = int(ecl)
    es = 60.0d0 * (ecl - eh)
    em = int(es)
    es = 60.0d0 * (es - em)

    write(*, *)
    sfmt = '(i4, "年 ", i2, "月 ", i2, "日 ", i2, "時 ", i2, "分 ", i2, "秒")'
    write(*, sfmt) int(year), int(month), int(day), int(hour), int(minute), int(second)
    write(*, '("JD = ", f14.5)') julian
    write(*, *)
    write(*, '("太陽の黄経 = ", f7.3," (",i3, "° ", i2, "′ ", i2, "″ )")') ecl, int(eh), int(em), int(es)
    write(*, '("地心距離   = ", f8.5, " AU")') rd
    write(*, *)

    stop
end program pcc_sunecliptic
