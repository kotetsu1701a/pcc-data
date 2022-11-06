program  pcc_aberration
    use pcclib

    implicit none

    ! 恒星データ
    character(16), parameter :: SNM = 'α CMa  Sirius  '              ! 恒星名
    double precision, parameter :: SRA(1:3) = (/6d0, 42d0, 56.714d0/)   ! 赤経
    double precision, parameter :: SDC(1:3) = (/-16d0, 38d0, 46.36d0/)  ! 赤緯
    double precision, parameter :: SRA_PROPER_MOTION = -0.03791d0     ! 赤経方向の固有運動量
    double precision, parameter :: SDC_PROPER_MOTION = -1.2114d0      ! 赤緯方向の固有運動量
    double precision, parameter :: SRV = -7.6d0                       ! 視線速度
    double precision, parameter :: SAP = 0.377d0                      ! 年周視差

    !検証用恒星データ
    ! character(16), parameter :: SNM = '61 Cyg          ';           ! 恒星名
    ! double precision, parameter :: SRA(1:3) = (/21d0, 4d0, 39.935d0/) ! 赤経
    ! double precision, parameter :: SDC(1:3) = (/38d0, 29d0, 59.10d0/) ! 赤緯
    ! double precision, parameter :: SRA_PROPER_MOTION = 0.35227d0    ! 赤経方向の固有運動量
    ! double precision, parameter :: SDC_PROPER_MOTION = 3.1847d0     ! 赤緯方向の固有運動量
    ! double precision, parameter :: SRV = -64.3d0                    ! 視線速度
    ! double precision, parameter :: SAP = 0.296d0                    ! 年周視差

    double precision :: dy, dt, julian, t, t0, t1
    double precision :: year, month, day, hour, minute, second
    double precision :: ra_proper_motion, dc_proper_motion
    double precision :: ra_deg, dc_deg, ra_rad, dc_rad, ap
    double precision :: ra_tmp, dc_tmp
    double precision :: l, m, n, ss, cc, tt
    double precision :: star_position(3)
    character(255) :: sfmt, dfmt
    character(1) :: sg, sgr

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

    ! ユリウス日を求める
    julian =  JulianDay(year, month, day)
    julian = julian + (hour / 24d0 + minute / 1440d0 + second / 86400d0)
    julian = julian - 0.375d0

    ! 1959年ベッセル年初からの経過年数
    t = BesselianYear(julian)
    t = t * 100d0

    ! 固有運動補正
    ra_rad = deg2rad(hms2deg(SRA(1), SRA(2), SRA(3)))
    dc_rad = deg2rad(dms2deg(SDC(1), SDC(2),SDC(3)))
    ra_proper_motion = SRA_PROPER_MOTION / 3600d0 * 15d0;
    dc_proper_motion = SDC_PROPER_MOTION / 3600d0;
    ap = deg2rad(SAP / 3600d0)
    star_position =  ProperMotion(t, ra_rad, dc_rad, ra_proper_motion, dc_proper_motion, ap, SRV)
    l = star_position(1)
    m = star_position(2)
    n = star_position(3)

    ! 固有運動の方向余弦から赤経・赤緯に換算する
    ss = m
    cc = l
    tt = Quadrant(ss, cc)
    ra_deg = rad2deg(tt)
    dc_deg = rad2deg(asin(n))

    ! 歳差による赤経・赤緯の変化
    t = t / 100d0
    ra_rad = deg2rad(ra_deg)
    dc_rad = deg2rad(dc_deg)
    star_position = Precession(t, ra_rad, dc_rad);
    l = star_position(1)
    m = star_position(2)
    n = star_position(3)

    ! 歳差の方向余弦から赤経・赤緯の計算
    ss = m
    cc = l
    tt = quadrant(ss, cc);
    ra_deg = rad2deg(tt);
    dc_deg = rad2deg(asin(n));

    ! 章動補正
    t0 = julian - 2415020d0
    t1 = t0 / 36525d0
    ra_rad = deg2rad(ra_deg)
    dc_rad = deg2rad(dc_deg)
    star_position = Nutation(t0, t1, ra_rad, dc_rad)
    l = star_position(1)
    m = star_position(2)
    n = star_position(3)

    ! 章動の方向余弦から赤経・赤緯の計算
    ss = m
    cc = l
    tt = quadrant(ss, cc)
    ra_deg = rad2deg(tt)
    dc_deg = rad2deg(asin(n))

    ! 視差補正
    t0 = (julian - 2451545d0) / 36525d0
    t1 = (julian - 2415020d0) / 36525d0
    ra_rad = deg2rad(ra_deg)
    dc_rad = deg2rad(dc_deg)
    ap = deg2rad(SAP / 3600d0)
    star_position = AnnualParallax(t0, t1, ra_rad, dc_rad, ap)
    l = star_position(1)
    m = star_position(2)
    n = star_position(3)

    ! 視差の方向余弦から赤経・赤緯の計算
    ss = m
    cc = l
    tt = quadrant(ss, cc)
    ra_deg = rad2deg(tt)
    dc_deg = rad2deg(asin(n))
    ra_tmp = ra_deg
    dc_tmp = dc_deg

    ! 光行差補正
    t0 = (julian - 2451545d0) / 36525d0
    t1 = (julian - 2415020d0) / 36525d0
    ra_rad = deg2rad(ra_deg)
    dc_rad = deg2rad(dc_deg)
    star_position = AnnualAberration(t0, t1, ra_rad, dc_rad)
    l = star_position(1)
    m = star_position(2)
    n = star_position(3)

    ! 光行差の方向余弦から赤経・赤緯の計算
    ss = m
    cc = l
    tt = quadrant(ss, cc);
    ra_deg = rad2deg(tt);
    dc_deg = rad2deg(asin(n));

    sgr = '+'
    if (SDC(1) < 0.0d0) then
        sgr = '-'
    end if
    sg = '+'
    if (dc_deg < 0.0d0) then
        sg = '-'
    end if

    ! 出力
    ! 角度の譚雨を表す°記号は出力に合わせて調整すること
    sfmt = '(i4, "年 ", i2, "月 ", i2, "日 ", i2, "時 ", i2, "分 ", i2, "秒 (JST)")'
    dfmt = '(a16, "        ", f10.6, "    ", a1, f9.6, "        ", f10.6, "     ", a1, f9.6)'
    write(*, *)
    write(*, sfmt) int(year), int(month), int(day), int(hour), int(minute), int(second)
    write(*, '("Julian Date = ", f14.5, " UTC")') julian
    write(*, *)
    write(*, *) '星名                   赤経α (視差補正) 赤緯δ         赤経α (光行差補正) 赤緯δ'
    write(*, *) '--------------------------------------------------------------------------------'
    write(*, *) '                         。　          。                。             。'
    write(*, dfmt) SNM, ra_tmp, sgr, abs(dc_tmp), ra_deg, sg, abs(dc_deg)
    write(*, *)

    stop
end program  pcc_aberration
