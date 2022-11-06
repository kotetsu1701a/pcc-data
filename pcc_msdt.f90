program pcc_msdt
    use pcclib
    
    implicit none
    
    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: RAD = 180.0d0 / PI

    double precision :: julian, dy, dt
    double precision :: year, month, day, hour, minute, second
    double precision :: sdt_hour, sdt_minute, sdt_second
    double precision :: msdt(3), dtime(3)
    double precision :: t
    character(255) :: dfmt

    write(*,*)
    write(*, fmt='(a)', advance='no') 'DATE AND TIME(JST) ? '
    read(*,*) dy, dt

    year = int(dy / 10000.0d0)
    month = mod(int(dy / 100.0d0), 100)
    day = mod(dy, 100.0d0)
    hour = int(dt / 10000.0d0)
    minute = mod(int(dt / 100.0d0), 100)
    second = mod(dt, 100.0d0)

    ! ユリウス日の計算
    julian = julianDay(year, month, day)
    julian = julian + (hour / 24d0 + minute / 1440d0 + second / 86400d0)
    julian = julian - 0.375

    dtime = Julian2date(julian)
    year = dtime(1)
    month = dtime(2)
    day = dtime(3)
    
    ! 時刻引数の計算
    ! t = (julian - 2451545d0) / 36525d0
    t = (julian - 2415020d0) / 36525d0
    
    ! グリニッジ平均恒星時の計算
    ! msdt = deg2hms(rad2deg(MeanSDT80(t)))
    msdt = deg2hms(rad2deg(MeanSDT50(t)))
    sdt_hour = msdt(1)
    sdt_minute = msdt(2)
    sdt_second = msdt(3)

    ! 恒星時が範囲を超えたときの処理
    if (sdt_hour > 24d0) then
        sdt_hour = sdt_hour - 24d0
    end if
    if (sdt_second >= 60d0) then
        sdt_second = sdt_second - 60d0
        sdt_minute = sdt_minute + 1d0
    end if
    if (sdt_minute == 60d0) then
        sdt_minute = 0d0
        sdt_hour = sdt_hour + 1d0
    end if
    if (sdt_hour == 24d0) then
        sdt_hour = 0d0
    end if

    ! 出力
    dfmt = '(" ", i4, " ", i2, " ", f8.5, "      ", f14.5, "         ", i2, "  ", i2, "  ", f6.3)'
    write(*, *)
    write(*, *) 'Date / 0h UTC           Julian Date        Mean Sidereal Time'
    write(*, *) '-------------------------------------------------------------'
    write(*, *) '                                               h   m   s'
    write(*, dfmt) int(year), int(month), day, julian, int(sdt_hour), int(sdt_minute), sdt_second
    write(*, *)

    stop
end program pcc_msdt
