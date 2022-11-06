program pcc_gast
    use pcclib

    implicit none
    
    double precision :: dy, dt, julian
    double precision :: year, month, day, hour, minute, second
    double precision :: t0, t1
    double precision :: msdt, sdt_hour, sdt_minute, sdt_second
    double precision :: dp, de, e, eq
    double precision :: sdt(3), eps(2)
    character(255) :: dfmt

    ! 日付の入力
    write(*,*)
    write(*, fmt='(a)', advance='no') 'DATE ? '
    read(*,*) dy

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
    e = iauMOE50(t1)                            ! 平均黄道傾斜角
    e = deg2rad(e)
    msdt = MeanSDT50(t1)                        ! グリニッジ平均恒星時（0 UTC）
    eps = iauNut50(t0, t1)                      ! 章動 ⊿Φと⊿εを求める
    dp = eps(1)                                 ! ⊿Φ
    de = eps(2) / 3600d0                        ! ⊿ε
    eq = dp * cos(deg2rad(de) + e)              ! 分点差
    sdt = deg2hms(rad2deg(msdt))
    sdt_hour = sdt(1)
    sdt_minute = sdt(2)
    sdt_second = sdt(3) + (eq / 15d0)           ! 分点差を時刻の秒に加算

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
    dfmt = '(" ", i4, " ", i2, " ", f8.5, "      ", f14.5, "       ", i2, "  ", i2, "  ", f6.3)'
    write(*, *)
    write(*, *) 'Date / 0h UTC           Julian Date   Greenwich Sidereal Time'
    write(*, *) '-------------------------------------------------------------'
    write(*, *) '                                             h   m   s'
    write(*, dfmt) int(year), int(month), day, julian, int(sdt_hour), int(sdt_minute), sdt_second
    write(*, *)

    stop
end program pcc_gast
