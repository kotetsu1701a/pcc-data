program pcc_lsdt
    use pcclib

    implicit none

    ! 地点
    character(21), parameter :: OBS_NAME = 'Tokyo Observatory PZT'          ! 地点名
    double precision, parameter :: OBS_LG(1:3) = (/9d0, 18d0, 9.936d0/)     ! 経度
    double precision, parameter :: OBS_LA(1:3) = (/35d0, 40d0, 20.707d0/)   ! 緯度
    
    ! 検証用地点
    ! character(21), parameter    :: OBS_NAME = "Tokyo Ngasawa"               ! 地点名
    ! double precision, parameter :: OBS_LG(1:3) = (/9d0, 18d0, 7.573d0/)     ! 経度
    ! double precision, parameter :: OBS_LA(1:3) = (/35d0, 47d0, 20.0d0/)     ! 緯度
    
    double precision :: dy, dt, julian
    double precision :: year, month, day, hour, minute, second
    double precision :: t, t0, t1
    double precision :: gast, lsdt, long, cv
    double precision :: msdt, sdt_hour, sdt_minute, sdt_second
    double precision :: dp, de, ecl, eq
    double precision :: sdt(3), eps(2)
    character(255) :: dfmt

    ! 日付の入力
    write(*,*)
    write(*, fmt='(a)', advance='no') 'DATE AND TIME(JST)? '
    read(*,*) dy, dt

    ! 日付と時刻を抽出
    year = int(dy / 10000.0d0)
    month = mod(int(dy / 100.0d0), 100)
    day = mod(dy, 100.0d0)
    hour = int(dt / 10000.0d0)
    minute = mod(int(dt / 100.0d0), 100)
    second = mod(dt, 100.0d0)
    
    ! ユリウス日の計算
    julian = julianDay(year, month, day)

    ! 時刻引数の計算
    t0 = julian - 2415020d0
    t1 = t0 / 36525d0
    
    ! グリニッジ視恒星時の計算
    ecl = deg2rad(iauMOE50(t1))                 ! 平均黄道傾斜角
    msdt = MeanSDT50(t1)                        ! グリニッジ平均恒星時（0 UTC）
    eps = iauNut50(t0, t1)                      ! 章動 ⊿Φと⊿εを求める
    dp = eps(1)                                 ! ⊿Φ
    de = eps(2) / 3600d0                        ! ⊿ε
    eq = dp * cos(deg2rad(de) + ecl)            ! 分点差
    sdt = deg2hms(rad2deg(msdt))
    sdt(3) = sdt(3) + (eq / 15d0)               ! 分点差を時刻の秒に加算

    ! グリニッジ視恒星時を時の小数点に換算
    gast = dms2deg(sdt(1), sdt(2), sdt(3))
    
    ! 経度を時の小数点に換算
    long = dms2deg(OBS_LG(1), OBS_LG(2), OBS_LG(3))
    
    ! 世界0時からの経過時間を時の小数点に換算
    hour = hour - 9d0;
    t = dms2deg(hour, minute, second)
    
    ! 補正値の計算
    cv = (hour * 3600d0 + minute * 60d0 + second) * 0.00273791d0
    cv = cv / 3600d0
    
    ! 地方恒星時
    lsdt = gast + long + t + cv
    sdt = deg2dms(lsdt)
    sdt_hour = sdt(1)
    sdt_minute = sdt(2)
    sdt_second = sdt(3)

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
    dfmt = '(" ", i4, "  ", i2, "  ", i2, "        ", f14.5, "         ", i2, "  ", i2, "  ", f6.3)'
    write(*, *)
    write(*, *) 'Date/                 Julian Date       Local Sidereal Time'
    write(*, *) '-----------------------------------------------------------'
    write(*, *) '    年  月  日                               h   m   s'
    write(*, dfmt) int(year), int(month), int(day), julian, int(sdt_hour), int(sdt_minute), sdt_second
    write(*, *)

    stop
end program pcc_lsdt
