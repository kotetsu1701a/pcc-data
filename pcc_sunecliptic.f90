program pcc_sunecliptic
    use pcclib
    
    implicit none
    
    double precision :: dy, dt, julian
    double precision :: year, month, day, hour, minute, second
    double precision :: t, ecl, rd
    double precision :: sun_ecliptic(2), sun_pos(3), dtime(3)
    character(255) :: dfmt

    ! 日付と時刻の入力
    write(*,*)
    write(*, fmt='(a)', advance='no') 'DATE AND TIME(JST) ? '
    read(*,*) dy, dt

    ! 年月日と時刻の抽出
    year = int(dy / 10000.0d0)
    month = mod(int(dy / 100.0d0), 100)
    day = mod(dy, 100.0d0)
    hour = int(dt / 10000.0d0)
    minute = mod(int(dt / 100.0d0), 100)
    second = mod(dt, 100.0d0)

    ! ユリウス日の計算
    julian = JulianDay(year, month, day)
    julian = julian + (hour / 24d0 + minute / 1440d0 + second / 86400d0)
    julian = julian - 0.375

    ! ユリウス日から年月日の計算（日の小数）
    dtime = Julian2date(julian)
    year = dtime(1)
    month = dtime(2)
    day = dtime(3)

    ! 太陽の黄経と動径の計算
    t = (julian - 2451545d0) / 36525d0
    sun_ecliptic =  SunEclipticLongitude(t)
    ecl = sun_ecliptic(1)
    rd  = sun_ecliptic(2)

    sun_pos = deg2dms(ecl)

    ! 出力
    dfmt = '(" ", i4, " ", i2, " ", f8.5, "      ", f14.5, "       ", f9.5,"(",i3, "° ", i2, "′ ", f3.1, "″)", f14.5)'
    write(*, *)
    write(*, *) 'Date / UTC               Julian Date                 黄経                   動径'
    write(*, *) '-----------------------------------------------------------------------------------'
    write(*, dfmt) int(year), int(month), day, julian, ecl, int(sun_pos(1)), int(sun_pos(2)), sun_pos(3), rd
    write(*, *)

    stop
end program pcc_sunecliptic
