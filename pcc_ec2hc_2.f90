! 天体の高度・方位角を求める
program pcc_ec2hc
    use pcclib

    implicit none
    
    ! 恒星データ
    ! character(16), parameter :: SNM = 'α CMa  Sirius  '              ! 恒星名
    ! double precision, parameter :: SRA(1:3) = (/6d0, 42d0, 56.714d0/)   ! 赤経
    ! double precision, parameter :: SDC(1:3) = (/-16d0, 38d0, 46.36d0/)  ! 赤緯
    ! double precision, parameter :: SRA_PROPER_MOTION = -0.03791d0     ! 赤経方向の固有運動量
    ! double precision, parameter :: SDC_PROPER_MOTION = -1.2114d0      ! 赤緯方向の固有運動量
    ! double precision, parameter :: SRV = -7.6d0                       ! 視線速度
    ! double precision, parameter :: SAP = 0.377d0                      ! 年周視差

    !検証用恒星データ
    character(16), parameter :: SNM = '61 Cyg          ';           ! 恒星名
    double precision, parameter :: SRA(1:3) = (/21d0, 4d0, 39.935d0/) ! 赤経
    double precision, parameter :: SDC(1:3) = (/38d0, 29d0, 59.10d0/) ! 赤緯
    double precision, parameter :: SRA_PROPER_MOTION = 0.35227d0    ! 赤経方向の固有運動量
    double precision, parameter :: SDC_PROPER_MOTION = 3.1847d0     ! 赤緯方向の固有運動量
    double precision, parameter :: SRV = -64.3d0                    ! 視線速度
    double precision, parameter :: SAP = 0.296d0                    ! 年周視差

    ! 地点データ
    ! character(21), parameter :: OBS_NAME = 'Tokyo Observatory PZT'      ! 地点名
    ! double precision, parameter :: OBS_LG(1:3) = (/9d0, 18d0, 9.936d0/)   ! 経度
    ! double precision, parameter :: OBS_LA(1:3) = (/35d0, 40d0, 20.707d0/) ! 緯度
    ! double precision, parameter :: GAST(1:3) = (/17d0, 51d0, 24.267d0/)

    ! 検証用地点データ
    character(21), parameter :: OBS_NAME = 'Ngasawa home         '    ! 地点名
    double precision, parameter :: OBS_LG(1:3) = (/9d0, 18d0, 7.573d0/) ! 経度
    double precision, parameter :: OBS_LA(1:3) = (/35d0, 47d0, 20.0d0/) ! 緯度
    double precision, parameter :: GAST(1:3) = (/17d0, 11d0, 58.714d0/)

    double precision :: dy, dt, julian
    double precision :: t
    double precision :: year, month, day, hour, minute, second
    double precision :: ra, dc, la, al, hi
    double precision :: rh, rm, rs, dh, dm, ds
    double precision :: gasdt, lsdt, long, cv
    double precision :: sdt_hour, sdt_minute, sdt_second
    double precision :: hc(2), sdt(3)
    character(255) :: sfmt, dfmt
    character(1) :: sg

    ! 日付と時刻の入力
    write(*,*)
    write(*, fmt='(a)', advance='no') 'DATE AND TIME(JST) ? '
    read(*,*) dy, dt

    ! 日付と時刻を抽出
    year = int(dy / 10000.0d0)
    month = mod(int(dy / 100.0d0), 100)
    day = mod(dy, 100.0d0)
    hour = int(dt / 10000.0d0)
    minute = mod(int(dt / 100.0d0), 100)
    second = mod(dt, 100.0d0)

    ! グリニッジ視恒星時を時の小数点に換算
    gasdt = dms2deg(GAST(1), GAST(2), GAST(3))

    ! 経度を時の小数点に換算
    long = dms2deg(OBS_LG(1), OBS_LG(2), OBS_LG(3))

    ! 世界0時からの経過時間を時の小数点に換算
    hour = hour - 9d0;
    t = dms2deg(hour, minute, second)

    ! 補正値の計算
    cv = (hour * 3600d0 + minute * 60d0 + second) * 0.00273791d0
    cv = cv / 3600d0

    ! 地方恒星時
    lsdt = gasdt + long + t + cv
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

    ! 高度・方位角
    ra = deg2rad(hms2deg(SRA(1), SRA(2), SRA(3)))
    dc = deg2rad(dms2deg(SDC(1), SDC(2), SDC(3)))
    la = deg2rad(dms2deg(OBS_LA(1), OBS_LA(2), OBS_LA(3)))
    lsdt = deg2rad(hms2deg(sdt_hour, sdt_minute, sdt_second))
    hc = AltitudeAzimuth(lsdt, la, ra, dc)
    al = rad2deg(hc(1))
    hi = rad2deg(hc(2))

    ! 赤経・赤緯をそれぞれの変数に代入
    rh = SRA(1)
    rm = SRA(2)
    rs = SRA(3)
    dh = SDC(1)
    dm = SDC(2)
    ds = SDC(3)

    sg = '+'
    if (dh < 0) then
        dh = abs(dh)
        sg = '-'
    end if

    ! 出力
    ! 角度の譚雨を表す°記号は出力に合わせて調整すること
    sfmt = '(i4, "年 ", i2, "月 ", i2, "日 ", i2, "時 ", i2, "分 ", i2, "秒 (JST)")'
    dfmt = '(a16, "      ", i2, " ",  i2, " ", f6.3, "    ", a1, i2, " ", i2, " ",  f5.2, "     ", f10.6, "    ", f10.6)'
    write(*, *)
    write(*, sfmt) int(year), int(month), int(day), int(hour + 9), int(minute), int(second)
    write(*, '("Julian Date = ", f14.5, " UTC")') julian
    write(*, *)
    write(*, *) '星名                  赤経α   (1950)   赤緯δ          方位角　       高度'
    write(*, *) '------------------------------------------------------------------------------'
    write(*, *) '                       h  m  s         °　′  ″　　　  　°　　    　 　°'
    write(*, dfmt) SNM, int(rh), int(rm), rs, sg, int(dh), int(dm), ds, al, hi
    write(*, *)

    stop
end program pcc_ec2hc
