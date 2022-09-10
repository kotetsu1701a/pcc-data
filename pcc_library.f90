subroutine errorcheck(h, m, s,  option, error, message)
! 入力値のチェック
! 引　数: 時分秒
!         h: 時
!       　m: 分
!       　s: 秒
!         option: 0:時分秒 1:度分秒
! 戻り値: 角度を返す
!       　error: true or false
!         message: エラーメッセージ

    implicit none
    
    double precision, intent(in) :: s
    integer, intent(in) :: h, m, option
    character(255), intent(out) :: message
    logical, intent(out) :: error

    if (option /= 0 .and. option /= 1) then
        error = .true.
        message = "Error: optionの指定は0または1を指定してください"
    else
        if (option == 0) then
            if (h < 0 .or. h > 23) then
                error = .true.
                message = "Error: 時の指定は0〜23を指定してください"
            end if
        else
            if (h < -89 .or. h > 89) then
                error = .true.
                message = "Error: 度の指定は＋89〜-89を指定してください"
            end if
        end if

        ! 上記でエラーが発生していれば、以下のチェックに影響しない
        if ((m < 0 .or. m > 59) .or. (s < 0 .or. s > 59)) then
            error = .true.
            message = "Error: 分または秒の指定は0〜59までの範囲で指定してください"
        end if
    end if

    return
end subroutine errorcheck

subroutine hms2deg(h, m, s, degree)
    ! 時分秒を角度に換算する
    ! 引　数: 時分秒
    !         h: 時
    !       　m: 分
    !       　s: 秒
    ! 戻り値: 角度を返す
    !       　degree: 角度

    implicit none

    integer, intent(in) :: h, m
    double precision, intent(in) :: s
    double precision, intent(out) :: degree

    degree = 15.0d0 * (abs(h) + m / 60.0d0 + s / 3600.0d0)

    return
end subroutine hms2deg

subroutine dms2deg(d, m, s, degree)
! 時分秒を角度に換算する
! 引　数: 度分秒
!         d: 度
!         m: 分
!         s: 秒
! 戻り値: 角度を返す
!         degree: 角度       

    implicit none

    integer, intent(in) :: d, m
    double precision, intent(in) :: s
    double precision, intent(out) :: degree
    integer :: sign

    sign = 1
    if (d < 0) then
        sign = -1
    end if

    degree = sign * (abs(d) + m / 60.0d0 + s / 3600.0d0)

    return
end subroutine dms2deg

subroutine deg2rad(x, radian)
! 角度をラジアンに換算する
! 引　数:   角度
!       　  x: 角度
! 戻り値:   ラジアン値を返す
!       　  radian: ラジアン値 

    implicit none
        
    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: RAD = 180.0d0 / PI
    
    double precision, intent(in) :: x
    double precision, intent(out) :: radian

    radian = x / RAD

end subroutine deg2rad

subroutine rad2deg(x, degree)
! ラジアンを角度に換算する
! 引　数:   ラジアン値
!       　  x: ラジアン値
! 戻り値:   角度を返す
!       　  degree: 角度

    implicit none
        
    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: RAD = 180.0d0 / PI
    
    double precision, intent(in) :: x
    double precision, intent(out) :: degree

    degree = x * RAD

end subroutine rad2deg

subroutine LocalSiderealTime(meansiderealtime, longitude, elapstime, sdt)
! 地方恒星時の計算
! 引　数:   meansiderealtime: グリニッジ平均恒星時（時間の少数点形式）
!          longitude:        経度（時間の少数点形式）
!          elapstime:        経過時間数（秒）
! 戻り値:  恒星時を小数点形式返す
!          sdt:              地方恒星時(時間の少数点形式)

    implicit none

    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: RAD = 180.0d0 / PI
    double precision, parameter :: M2PI = 2.0d0 * PI
    
    double precision, intent(in) :: meansiderealtime, longitude, elapstime
    
    double precision :: sdt, cv, lapstime

    cv = (elapstime * 0.00273791d0) / 3600.0d0
    lapstime = elapstime / 3600.0d0

    sdt = meansiderealtime + longitude + lapstime + cv

    if (int(sdt) > 24) then
        sdt = sdt - 24.0d0
    else
        if (sdt < 0) then
            sdt = sdt + 24.0d0
        end if
    end if

    return
end subroutine Localsiderealtime

subroutine MeanSiderealTime(t, sdt)
! 世界時0時における平均恒星時の計算
! 引　数:  時刻引数
!          t        : 年
! 戻り値:  平均恒星時を小数点形式で返す
!          sdt      : 平均恒星時, 
    implicit none
    
    double precision, intent(in) :: t
    double precision, intent(out) :: sdt

    double precision :: t2

    t2 = t * t

    sdt = 6.0d0 * 3600.0d0 + 38.0d0 * 60.0d0 + 45.836d0
    sdt = (sdt + 8640184.542d0 * t + 0.0929d0 * t2) / 86400.0d0
    sdt = 24.0d0 * (sdt - int(sdt))

    if (int(sdt) > 24) then
        sdt = sdt - 24.0d0
    else
        if (sdt < 0) then
            sdt = sdt + 24.0d0
        end if
    end if

    return
end subroutine MeanSiderealTime

subroutine MeanSiderealTime2(t, sdt)
! 世界時0時における平均恒星時の計算 2000.0分点
! 引　数:  時刻引数
!          t        : 年
! 戻り値:  平均恒星時を小数点形式で返す
!          sdt      : 平均恒星時, 

    implicit none

    double precision, intent(in) :: t
    double precision, intent(out) :: sdt

    double precision :: t2, t3

    t2 = t * t
    t3 = t * t2
    sdt = 6.0d0 * 3600.0d0 + 41.0d0 * 60.0d0 + 50.54841d0
    sdt = (sdt + 8640184.812866d0 * t + 0.093104d0 * t2 - 0.0000062 * t3) / 86400.0d0
    sdt = 24.0d0 * (sdt - int(sdt))

    if (int(sdt) > 24) then
        sdt = sdt - 24.0d0
    else
        if (sdt < 0) then
            sdt = sdt + 24.0d0
        end if
    end if

    return
end subroutine MeanSiderealTime2

subroutine MeanSiderealTime3(julian, t, sdt)
! 世界時0時における平均恒星時の計算
! シリーズ現代の天文学13 天体の位置と運動より
! 引　数:  時刻引数
!          t        : 年
! 戻り値:  平均恒星時を小数点形式で返す
!          sdt      : 平均恒星時

    implicit none

    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: RAD = 180.0d0 / PI
    double precision, parameter :: M2PI = 2.0d0 * PI

    double precision, intent(in) :: julian, t
    double precision, intent(out) :: sdt

    double precision :: era, gmst
    double precision :: t2, t3, t4, t5

    t2 = t * t
    t3 = t * t2
    t4 = t * t3
    t5 = t * t4

    era = 0.7790572732640d0 + 1.00273781191135448d0 * (julian - 2451545.0d0)
    era = M2PI * (era - int(era))
    
    gmst = 0.014506d0 + 4612.156534d0 * t + 1.3915817d0 * t2 - 0.00000044d0 * t3 - 0.000029956d0 * t4 - 0.0000000368d0 * t5
    gmst = gmst / 3600.0d0

    gmst = (era * RAD) + gmst
    gmst = gmst / 15.0d0
    sdt = gmst

    if (int(sdt) > 24) then
        sdt = sdt - 24.0d0
    else
        if (sdt < 0) then
            sdt = sdt + 24.0d0
        end if
    end if

    return
end subroutine MeanSiderealTime3
    
subroutine GetJulianDay(year, month, day, hour, minute, second, julian)
! ユリウス日を求める
! 引　数:  観測年月日＋時分秒(日本標準時)
!          year        : 年
!          month       : 月
!          day         : 日
!          hour        : 時
!          minute      : 分
!          second      : 秒
! 戻り値:  ユリウス日を返す
!          julianday: ユリウス日(世界時)
!
! 計算式について
! 本ルーチンで使用している計算式は、紀元前や1582年10月15日より
! 前のユリウス日の計算には使えないので注意が必要である。
    implicit none

    double precision, intent(in) :: year, month, day, hour, minute, second
    double precision, intent(out) :: julian
    double precision :: mm, yy
    
    mm = month
    yy = year

    if (int(mm) < 3) then
        mm = mm + 12.0d0
        yy = yy - 1.0d0
    end if

    julian = int(yy * 365.25d0) + int(yy / 400.0d0) - int(yy / 100.0d0) + int(30.59d0 * (mm - 2.0d0)) + day + 1721088.5d0
    
    julian = julian + (hour / 24.0d0 + minute / 1440.0d0 + (second)/ 86400.0d0)
    julian = julian - 0.375d0
    
    return
end subroutine GetJulianDay

subroutine GetElapsTime(year, month, day, k)
! 経過日数を求める
! 引　数:  計算年月日
!          year        : 年
!          month       : 月
!          day         : 日
! 戻り値:  経過日数を返す
!          k: 経過日数
    implicit none
    
    double precision, intent(in) :: year, month, day
    double precision, intent(out) :: k

    double precision :: yy, mm

    mm = month
    yy = year
    
    if (int(mm) < 3) then
        mm = mm + 12.0d0
        yy = yy - 1.0d0
    end if

    k = int(365.0d0 * yy) + 30.0d0 * mm + day - 33.5d0 + int(3.0d0 / 5.0d0 * (mm + 1.0d0)) + int(yy / 4.0d0)

    return
end subroutine GetElapsTime

subroutine AltitudeAzimuth(localsiderealtime, latitude, ra, dc, al, hi)
! 赤経・赤緯から高度および方位角を求める
! 引　数:  地方恒星時,緯度,赤経,赤緯を角度の単位で渡す
!          localsiderealtime : 地方恒星時
!          latitude          : 緯度
!          ra                : 赤経
!          dc                : 赤緯
! 戻り値:  高度と方位角を角度で返す
!          al : 方位角(南より西回り)
!          hi : 高度

    implicit none
    
    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: RAD = 180.0d0 / PI

    double precision, intent(in) :: localsiderealtime, latitude, ra, dc
    double precision, intent(out) ::  al, hi

    double precision :: la, r1, d1, ss, cc, tt, ha

    ha = (15.0d0 * localsiderealtime) - ra
    ha = ha / RAD
    la = latitude / RAD
    r1 = ra / RAD
    d1 = dc / RAD

    hi = sin(la) * sin(d1) + cos(la) * cos(d1) * cos(ha)
    hi = asin(hi) * RAD

    ss = cos(d1) * sin(ha)
    cc = -cos(la) * sin(d1) + sin(la) * cos(d1) * cos(ha)
    call Quadrant(ss, cc, tt)
    al = tt
    al = al * RAD

    return
end subroutine AltitudeAzimuth

subroutine Quadrant(ss, cc, tt)
! 象限の判断　「マイコン宇宙講座」79頁　リスト3-13より
! 引　数:  sinとcosの値を渡す
!          ss : sin
!          cc : cos
! 戻り値:  赤経または方位角、黄経をラジアン値で返す
!          tt : 赤経または方位角、黄経

    implicit none

    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: M2PI = 2.0d0 * PI
    double precision, intent(in) :: ss, cc
    double precision :: tt

    tt = atan(ss / cc)

    if (cc < 0) then
        tt = tt + PI
    else
        if (ss < 0) then
            tt = tt + M2PI
        end if
    end if

    return
end subroutine Quadrant  

subroutine MeanObliquityEcliptic(t, ea)
! 平均黄道傾斜角を求める
! 引　数:  時刻引数
!          t : 時刻引数 (JD - 2451545.0) / 36525.0
! 戻り値:  平均黄道傾斜角を度の単位で返す
!          ea : 平均黄道傾斜角

    implicit none

    double precision, intent(in) :: t
    double precision, intent(out) :: ea

    double precision :: t2, t3, t4, t5

    t2 = t * t
    t3 = t2 * t
    t4 = t3 * t
    t5 = t4 * t

    ea = 84381.406d0 - 46.836769d0 * t - 0.0001831d0 * t2 + 0.0020034d0 * t3 - 0.000000576d0 * t4 - 0.0000000434d0 * t5
    ea = ea / 3600.0d0

    return
end subroutine MeanObliquityEcliptic

subroutine JulianToDate(julian, year, month, day)
! ユリウス日から年月日を求める　「天文計算入門」63頁 式(16.6)より
! 引　数: ユリウス日
!         julian: ユリウス日
! 戻り値: 年月日
!         year: 年
!         month: 月
!       　day: 日(小数点あり)

    implicit none

    double precision, intent(in) :: julian
    double precision, intent(out) :: year, month, day

    integer:: a, b, c, e, f, g, h


    a = int(julian + 68569.5d0)
    b = int(a / 36524.25d0)
    c = a - int(36524.25d0 * b + 0.75d0)
    e = int((c + 1) / 365.25025d0)
    f = c - int(365.25d0 * e) + 31
    g = int(f / 30.59d0)
    day = f - int(30.59d0 * g) + (julian + 0.5d0) - int(julian + 0.5d0)
    h = int(g / 11)
    month = int(g - 12 * h + 2)
    year = int(100 * (b - 49) + e + h)

    ! 12月32日になったときの処理
    if (int(month) == 12) then
        if (int(day) > 31) then
            year = year + 1.0d0
            month = (month + 1.0d0) - 12.0d0
            day = 1.0d0
        end if
    end if

    return
end subroutine JulianToDate

subroutine BesselianYear(julian, t)
! 1950のベッセル年初からの経過日数を36524.21で測ったものを返す。
! 「マイコン宇宙講座」31頁　式（2.3.1）より
! 引　数:
!         julian: ユリウス日
! 戻り値: 1950年ベッセル年初からの経過年数
!         t: 経過年数

    implicit none
    
    double precision, parameter :: JDBEPOCH = 33281.923d0

    double precision, intent(in) :: julian
    double precision, intent(out) :: t
    
    double precision :: mjd

    mjd = julian - 2400000.5d0
    t = mjd - JDBEPOCH
    t = t * (0.00002737909288d0 + 1.25013d-17 * t)
    return
end subroutine BesselianYear

subroutine ProperMotion(t, ra0, dc0, ra_proper0, dc_proper0, ap0, rv, star_position)
! 恒星の位置計算(固有運動補正)
! 「天体の位置計算 増補版」P.43 - P.46
! 引　数:  引数については以下の通り
!          julian       : ユリウス日
!          ra           : 赤経(°)
!          dc           : 赤緯(°)
!          ra_proper    : 赤経方向の固有運動量
!          dc_proper    : 赤緯方向の固有運動量
!          ap           : 年周視差(″)
!          rv           : 視線速度(Km/秒)
! 戻り値:  固有運動によって変化した天体位置の方向余弦を返す
!          star_position(1): L1
!          star_position(2): M1
!          star_position(3): N1
! 呼　出: JulianYear(), Radians(), Degrees(), Quadrant()

    implicit none

    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: RAD = 180.0d0 / PI
    
    double precision, intent(in) :: t
    double precision, intent(in) :: ra0, dc0, ra_proper0, dc_proper0
    double precision, intent(in) :: ap0, rv
    double precision, intent(out) :: star_position(3)
    double precision :: ra, dc
    double precision :: ra_proper, dc_proper, ap
    double precision :: l1, m1, n1
    double precision :: mu0, phi, sigma
    double precision :: ma(3, 3), mb(3, 3), mc(3, 3), md(3, 1), me(3, 1)

    ra = ra0 / RAD
    dc = dc0 / RAD

    ra_proper = ra_proper0 / 3600.0d0 * 15.0d0
    dc_proper = dc_proper0 / 3600.0d0

    ap = (ap0 / 3600.0d0) / RAD

    ! 全固有運動速度の計算
    mu0 = sqrt((ra_proper**2 * cos(dc)**2) + dc_proper**2)

    ! 固有運動の向きを計算
    phi = (ra_proper * cos(dc)) / dc_proper
    phi = (atan(phi) * RAD) / RAD

    ! 1年間における恒星の移動の角距離を計算
    sigma = mu0 * t * (1.0d0 - (ap / 1.496d8) * rv * (t * (365.2422d0 * 86400.0d0)))
    sigma = sigma / RAD

    ! 方向余弦の計算
    ma(1, 1:3) = (/cos(ra), -sin(ra), 0.0d0/)
    ma(2, 1:3) = (/sin(ra),  cos(ra), 0.0d0/)
    ma(3, 1:3) = (/0.0d0,    0.0d0,   1.0d0/)

    mb(1, 1:3) = (/cos(dc),  0.0d0, -sin(dc)/)
    mb(2, 1:3) = (/0.0d0,    1.0d0,    0.0d0/)
    mb(3, 1:3) = (/sin(dc),  0.0d0,  cos(dc)/)

    mc(1, 1:3) = (/1.0d0,     0.0d0,    0.0d0/)
    mc(2, 1:3) = (/0.0d0,  cos(phi), sin(phi)/)
    mc(3, 1:3) = (/0.0d0, -sin(phi), cos(phi)/)

    md(1, 1) = cos(sigma)
    md(2, 1) = 0.0d0
    md(3, 1) = sin(sigma)

    me = matmul(matmul(ma, mb), matmul(mc, md))
    l1 = me(1, 1)
    m1 = me(2, 1)
    n1 = me(3, 1)

    star_position(1) = l1
    star_position(2) = m1
    star_position(3) = n1

    return

end subroutine ProperMotion

subroutine Precession(t, ra0, dc0, star_position)
! 歳差の計算
! 天体の位置計算 増補版 P.49 - P55
! 引　数:  時刻引数と赤経・赤緯
!          t:  ベッセル年初
!          ra0: 天体の赤経
!          dc0: 天体の赤緯
! 戻り値:  歳差運動によって変化した天体位置の方向余弦を返す
!          star_position(/0]: l2
!          star_position(/1]: m2
!          star_position(/2]: n3

    implicit none

    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: RAD = 180.0d0 / PI
    
    double precision, intent(in) :: t
    double precision, intent(in) :: ra0, dc0
    double precision, intent(out) :: star_position(3)

    double precision :: ra, dc
    double precision :: x1, x2, x3
    double precision :: l, m, n, l2, m2, n2
    double precision :: ma(3, 3), mb(3, 3), mc(3, 3), md(3, 1), me(3, 1)

    ! 赤経・赤緯から方向余弦を計算する
    ra = ra0 / RAD
    dc = dc0 / RAD
    l = cos(dc) * cos(ra)
    m = cos(dc) * sin(ra)
    n = sin(dc)

    ! 歳差定数の計算(EPOCH=1950)
    x1 = 2304.9480d0 * t + 0.302d0 * t**2 + 0.018d0 * t**3
    x2 = 2304.9480d0 * t + 1.093d0 * t**2 + 0.019d0 * t**3
    x3 = 2004.2555d0 * t - 0.426d0 * t**2 - 0.042d0 * t**3

    x1 = (x1 / 3600.0d0) / RAD
    x2 = (x2 / 3600.0d0) / RAD
    x3 = (x3 / 3600.0d0) / RAD

    ! 方向余弦の計算
    ma(1, 1:3) = (/-sin(x2), -cos(x2), 0.0d0/)
    ma(2, 1:3) = (/ cos(x2), -sin(x2), 0.0d0/)
    ma(3, 1:3) = (/0.0d0,    0.0d0,    1.0d0/)

    mb(1, 1:3) = (/1.0d0,    0.0d0,     0.0d0/)
    mb(2, 1:3) = (/0.0d0,   cos(x3),  sin(x3)/)
    mb(3, 1:3) = (/0.0d0,  -sin(x3),  cos(x3)/)

    mc(1, 1:3) = (/ sin(x1),  cos(x1),  0.0d0/)
    mc(2, 1:3) = (/-cos(x1),  sin(x1),  0.0d0/)
    mc(3, 1:3) = (/0.0d0,      0.0d0,   1.0d0/)

    md(1, 1) = l
    md(2, 1) = m
    md(3, 1) = n

    me = matmul(matmul(ma, mb), matmul(mc, md))
    l2 = me(1, 1)
    m2 = me(2, 1)
    n2 = me(3, 1)

    star_position(1) = l2
    star_position(2) = m2
    star_position(3) = n2
    
    return
end subroutine Precession

subroutine Nutation1950(dt, t, ra0, dc0, star_position)
! 章動の計算
! 天体の位置計算 増補版 P.58 - P.66
! 引　数:  ユリウス世紀
!          dt:  経過日数
!          t:  1900年1月0日正午からの経過日数をユリウス世紀数で表したもの
! 戻り値:  章動によって変化した赤経および赤緯の方向余弦を返す
!          star_position(1): l3
!          star_position(2): m3
!          star_position(3): n3

    implicit none

    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: RAD = 180.0d0 / PI

    double precision, intent(in) :: dt
    double precision, intent(in) :: t
    double precision, intent(in) :: ra0, dc0
    double precision, intent(out) :: star_position(3)

    double precision :: xpl(69, 9)
    double precision :: arg, s, c
    double precision :: dp             ! 黄経の章動
    double precision :: de             ! 黄道傾斜角の章動
    double precision :: el             ! 月の平均近点角
    double precision :: elp            ! 太陽の平均近点角
    double precision :: f              ! 昇交点から測った月の平均黄経
    double precision :: d              ! 太陽と月の平均離角
    double precision :: om             ! 月の平均昇交点黄経
    double precision :: e
    double precision :: ra, dc
    double precision :: l, m, n, l3, m3, n3
    double precision :: ma(3, 3), mb(3, 3), mc(3, 3), md(3, 1), me(3, 1)
    integer :: i

    xpl( 1, 1:9) = (/ 0d0,  0d0,  0d0,  0d0,  1d0, -17.2327d0, -0.01737d0,  9.2100d0,  0.00091d0/)
    xpl( 2, 1:9) = (/ 0d0,  0d0,  2d0, -2d0,  2d0,  -1.2729d0, -0.00013d0,  0.5522d0, -0.00029d0/)
    xpl( 3, 1:9) = (/ 0d0,  0d0,  0d0,  0d0,  2d0,   0.2088d0,  0.00002d0, -0.0904d0,  0.00004d0/)
    xpl( 4, 1:9) = (/ 0d0,  1d0,  0d0,  0d0,  0d0,   0.1261d0, -0.00031d0,  0.0000d0,  0.00000d0/)
    xpl( 5, 1:9) = (/ 0d0,  1d0,  2d0, -2d0,  2d0,  -0.0497d0,  0.00012d0,  0.0216d0, -0.00006d0/)
    xpl( 6, 1:9) = (/ 0d0, -1d0,  2d0, -2d0,  2d0,   0.0214d0, -0.00005d0, -0.0093d0,  0.00003d0/)
    xpl( 7, 1:9) = (/ 0d0,  0d0,  2d0, -2d0,  1d0,   0.0124d0,  0.00001d0, -0.0066d0,  0.00000d0/)
    xpl( 8, 1:9) = (/-2d0,  0d0,  2d0,  0d0,  1d0,   0.0045d0,  0.00000d0, -0.0024d0,  0.00000d0/)
    xpl( 9, 1:9) = (/ 2d0,  0d0,  0d0, -2d0,  0d0,   0.0045d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(10, 1:9) = (/ 0d0,  0d0,  2d0, -2d0,  0d0,  -0.0021d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(11, 1:9) = (/ 0d0,  2d0,  0d0,  0d0,  0d0,   0.0016d0, -0.00001d0,  0.0000d0,  0.00000d0/)
    xpl(12, 1:9) = (/ 0d0,  2d0,  2d0, -2d0,  2d0,  -0.0015d0,  0.00001d0,  0.0007d0,  0.00000d0/)
    xpl(13, 1:9) = (/ 0d0,  1d0,  0d0,  0d0,  1d0,  -0.0015d0,  0.00000d0,  0.0008d0,  0.00000d0/)
    xpl(14, 1:9) = (/ 0d0, -1d0,  0d0,  0d0,  1d0,  -0.0010d0,  0.00000d0,  0.0005d0,  0.00000d0/)
    xpl(15, 1:9) = (/ 2d0,  0d0, -2d0,  0d0,  0d0,   0.0010d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(16, 1:9) = (/-2d0,  0d0,  0d0,  2d0,  1d0,  -0.0005d0,  0.00000d0,  0.0003d0,  0.00000d0/)
    xpl(17, 1:9) = (/ 0d0, -1d0,  2d0, -2d0,  1d0,  -0.0005d0,  0.00000d0,  0.0003d0,  0.00000d0/)
    xpl(18, 1:9) = (/ 0d0, -2d0,  2d0, -2d0,  1d0,  -0.0004d0,  0.00000d0,  0.0002d0,  0.00000d0/)
    xpl(19, 1:9) = (/ 2d0,  0d0,  0d0, -2d0,  1d0,   0.0004d0,  0.00000d0, -0.0002d0,  0.00000d0/)
    xpl(20, 1:9) = (/ 0d0,  1d0,  2d0, -2d0,  1d0,   0.0003d0,  0.00000d0, -0.0002d0,  0.00000d0/)
    xpl(21, 1:9) = (/-2d0,  0d0,  2d0,  0d0,  2d0,  -0.0003d0,  0.00000d0,  0.0002d0,  0.00000d0/)
    xpl(22, 1:9) = (/ 1d0,  0d0,  0d0, -1d0,  0d0,  -0.0003d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(23, 1:9) = (/ 1d0, -1d0,  0d0, -1d0,  0d0,  -0.0002d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(24, 1:9) = (/ 0d0,  0d0,  2d0,  0d0,  2d0,  -0.2037d0, -0.00002d0,  0.0884d0, -0.00005d0/)
    xpl(25, 1:9) = (/ 1d0,  0d0,  0d0,  0d0,  0d0,   0.0675d0,  0.00001d0,  0.0000d0,  0.00000d0/)
    xpl(26, 1:9) = (/ 0d0,  0d0,  2d0,  0d0,  1d0,  -0.0342d0, -0.00004d0,  0.0183d0,  0.00000d0/)
    xpl(27, 1:9) = (/ 1d0,  0d0,  2d0,  0d0,  2d0,  -0.0261d0,  0.00000d0,  0.0113d0, -0.00001d0/)
    xpl(28, 1:9) = (/ 1d0,  0d0,  0d0, -2d0,  0d0,  -0.0149d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(29, 1:9) = (/-1d0,  0d0,  2d0,  0d0,  2d0,   0.0114d0,  0.00000d0, -0.0050d0,  0.00000d0/)
    xpl(30, 1:9) = (/ 0d0,  0d0,  0d0,  2d0,  0d0,   0.0060d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(31, 1:9) = (/ 1d0,  0d0,  0d0,  0d0,  1d0,   0.0058d0,  0.00000d0, -0.0031d0,  0.00000d0/)
    xpl(32, 1:9) = (/-1d0,  0d0,  0d0,  0d0,  1d0,  -0.0057d0,  0.00000d0,  0.0030d0,  0.00000d0/)
    xpl(33, 1:9) = (/-1d0,  0d0,  2d0,  2d0,  2d0,  -0.0052d0,  0.00000d0,  0.0022d0,  0.00000d0/)
    xpl(34, 1:9) = (/ 1d0,  0d0,  2d0,  0d0,  1d0,  -0.0044d0,  0.00000d0,  0.0023d0,  0.00000d0/)
    xpl(35, 1:9) = (/ 0d0,  0d0,  2d0,  2d0,  2d0,  -0.0032d0,  0.00000d0,  0.0014d0,  0.00000d0/)
    xpl(36, 1:9) = (/ 2d0,  0d0,  0d0,  0d0,  0d0,   0.0028d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(37, 1:9) = (/ 1d0,  0d0,  2d0, -2d0,  2d0,   0.0026d0,  0.00000d0, -0.0011d0,  0.00000d0/)
    xpl(38, 1:9) = (/ 2d0,  0d0,  2d0,  0d0,  2d0,  -0.0026d0,  0.00000d0,  0.0011d0,  0.00000d0/)
    xpl(39, 1:9) = (/ 0d0,  0d0,  2d0,  0d0,  0d0,   0.0025d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(40, 1:9) = (/-1d0,  0d0,  2d0,  0d0,  1d0,   0.0019d0,  0.00000d0, -0.0010d0,  0.00000d0/)
    xpl(41, 1:9) = (/-1d0,  0d0,  0d0,  2d0,  1d0,   0.0014d0,  0.00000d0, -0.0007d0,  0.00000d0/)
    xpl(42, 1:9) = (/ 1d0,  0d0,  0d0, -2d0,  1d0,  -0.0013d0,  0.00000d0,  0.0007d0,  0.00000d0/)
    xpl(43, 1:9) = (/-1d0,  0d0,  2d0,  2d0,  1d0,  -0.0009d0,  0.00000d0,  0.0005d0,  0.00000d0/)
    xpl(44, 1:9) = (/ 1d0,  1d0,  0d0, -2d0,  0d0,  -0.0007d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(45, 1:9) = (/ 0d0,  1d0,  2d0,  0d0,  2d0,   0.0007d0,  0.00000d0, -0.0003d0,  0.00000d0/)
    xpl(46, 1:9) = (/ 1d0,  0d0,  0d0,  2d0,  0d0,   0.0006d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(47, 1:9) = (/ 0d0,  0d0,  0d0,  2d0,  1d0,  -0.0006d0,  0.00000d0,  0.0003d0,  0.00000d0/)
    xpl(48, 1:9) = (/ 0d0, -1d0,  2d0,  0d0,  2d0,  -0.0006d0,  0.00000d0,  0.0003d0,  0.00000d0/)
    xpl(49, 1:9) = (/ 1d0,  0d0,  2d0,  2d0,  2d0,  -0.0006d0,  0.00000d0,  0.0003d0,  0.00000d0/)
    xpl(50, 1:9) = (/ 2d0,  0d0,  2d0, -2d0,  2d0,   0.0006d0,  0.00000d0, -0.0002d0,  0.00000d0/)
    xpl(51, 1:9) = (/ 0d0,  0d0,  0d0, -2d0,  1d0,  -0.0005d0,  0.00000d0,  0.0003d0,  0.00000d0/)
    xpl(52, 1:9) = (/ 0d0,  0d0,  2d0,  2d0,  1d0,  -0.0005d0,  0.00000d0,  0.0003d0,  0.00000d0/)
    xpl(53, 1:9) = (/ 1d0,  0d0,  2d0, -2d0,  1d0,   0.0005d0,  0.00000d0, -0.0003d0,  0.00000d0/)
    xpl(54, 1:9) = (/ 0d0,  0d0,  0d0,  1d0,  0d0,  -0.0004d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(55, 1:9) = (/ 0d0,  1d0,  0d0, -2d0,  0d0,  -0.0004d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(56, 1:9) = (/ 1d0, -1d0,  0d0,  0d0,  0d0,   0.0004d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(57, 1:9) = (/ 1d0,  0d0, -2d0,  0d0,  0d0,   0.0004d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(58, 1:9) = (/ 2d0,  0d0,  2d0,  0d0,  1d0,  -0.0004d0,  0.00000d0,  0.0002d0,  0.00000d0/)
    xpl(59, 1:9) = (/ 1d0,  0d0,  2d0,  0d0,  0d0,   0.0003d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(60, 1:9) = (/ 1d0,  1d0,  0d0,  0d0,  0d0,  -0.0003d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(61, 1:9) = (/ 1d0, -1d0,  2d0,  0d0,  2d0,  -0.0003d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(62, 1:9) = (/-2d0,  0d0,  0d0,  0d0,  1d0,  -0.0002d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(63, 1:9) = (/-1d0,  0d0,  2d0, -2d0,  1d0,  -0.0002d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(64, 1:9) = (/ 2d0,  0d0,  0d0,  0d0,  1d0,   0.0002d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(65, 1:9) = (/-1d0, -1d0,  2d0,  2d0,  2d0,  -0.0002d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(66, 1:9) = (/ 0d0, -1d0,  2d0,  2d0,  2d0,  -0.0002d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(67, 1:9) = (/ 1d0,  0d0,  0d0,  0d0,  2d0,  -0.0002d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(68, 1:9) = (/ 1d0,  1d0,  2d0,  0d0,  2d0,   0.0002d0,  0.00000d0,  0.0000d0,  0.00000d0/)
    xpl(69, 1:9) = (/ 3d0,  0d0,  2d0,  0d0,  2d0,  -0.0002d0,  0.00000d0,  0.0000d0,  0.00000d0/)

    ! 赤経・赤緯から方向余弦を計算する
    ra = ra0 / RAD
    dc = dc0 / RAD
    l = cos(dc) * cos(ra)
    m = cos(dc) * sin(ra)
    n = sin(dc)

    ! Nutation 1950
    ! 天体の位置計算 増補版 p.58 章動の計算
    ! この計算式で使われる値は、すべて度の単位になっている。
    el  = 296.104608d0 + 13.0649924465d0 * dt + 6.890d-12 * dt**2 + 2.95d-20 * dt**3
    elp = 358.475833d0 +  0.9856002669d0 * dt - 0.112d-12 * dt**2 - 6.80d-20 * dt**3
    f   =  11.250889d0 + 13.2293504490d0 * dt - 2.407d-12 * dt**2 - 0.70d-20 * dt**3
    d   = 350.737486d0 + 12.1907491914d0 * dt - 1.076d-12 * dt**2 + 3.90d-20 * dt**3
    om  = 259.183275d0 -  0.0529539222d0 * dt + 1.557d-12 * dt**2 + 4.60d-20 * dt**3

    ! 360°の整数倍を差し引く
    el  = Mla(el)
    elp = Mla(elp)
    f   = Mla(f)
    d   = Mla(d) 
    om  = Mla(om)

    ! Standards of Fundamental Astronomy
    ! http:!www.iausofa.org/2019_0722_C/CompleteList.html
    ! from nut80.c
    dp = 0.0d0
    de = 0.0d0
    do i = 1, 69
        arg = el * xpl(i, 1) + elp * xpl(i, 2) + f * xpl(i, 3) + d * xpl(i, 4) + om * xpl(i, 5)

        s = (xpl(i, 6) + (xpl(i, 7) * t))
        c = (xpl(i, 8) + (xpl(i, 9) * t))

        if (s /= 0.0d0) then
             dp = dp + s * sin(arg / RAD)
        end if
        if (c /= 0.0d0) then
            de = de + c * cos(arg / RAD)
        end if
    end do
    ! end of nut80.c

    dp = (dp / 3600.0d0) / RAD
    de = (de / 3600.0d0) / RAD

    ! 平均黄道傾斜角の計算(1950)
    e = (84428.2584d0 - 46.8450d0 * t - 0.005904d0 * t**2 + 0.0018108d0  * t**3) / 3600.0d0
    e = e / RAD

    ! 方向余弦の計算
    ma(1, 1:3) = (/1.0d0, 0.0d0,        0.0d0      /)
    ma(2, 1:3) = (/0.0d0, cos(e + de), -sin(e + de)/)
    ma(3, 1:3) = (/0.0d0, sin(e + de),  cos(e + de)/)

    mb(1, 1:3) = (/cos(dp), -sin(dp), 0.0d0/)
    mb(2, 1:3) = (/sin(dp),  cos(dp), 0.0d0/)
    mb(3, 1:3) = (/0.0d0,    0.0d0,   1.0d0/)

    mc(1, 1:3) = (/1.0d0,  0.0d0,  0.0d0 /)
    mc(2, 1:3) = (/0.0d0,  cos(e), sin(e)/)
    mc(3, 1:3) = (/0.0d0, -sin(e), cos(e)/)

    md(1, 1) = l
    md(2, 1) = m
    md(3, 1) = n

    me = matmul(matmul(ma, mb), matmul(mc, md))
    l3 = me(1, 1)
    m3 = me(2, 1)
    n3 = me(3, 1)

    star_position(1) = l3
    star_position(2) = m3
    star_position(3) = n3

    return
    contains
    double precision function Mla(x)
    ! x からnの整数倍を差し引いた値を返す

        implicit none
        
        double precision, intent(in) :: x
        double precision :: val

        val = 360.0d0 * (x / 360.0d0 - int(x / 360.d0))
        if (val < 0) then
            val = val + 360.0d0
        end if

        Mla = val

        return
    end function Mla
end subroutine Nutation1950

subroutine SunEclipticLongitude(t, sun_ecliptic)
! 太陽の黄経計算
! 海上保安庁水路部(現　海洋情報部)の略算式による計算
!
! 引　数: ユリウス世紀数
!        t: 1ユリウス世紀数(JD - 2451545.0) / 36525.0)
! 戻り値: 太陽の黄経を度の単位で返す。
!        ecl(1): 太陽の黄経
!        ecl(2): 地心距離
!
! 計算精度について:
! 係数の桁数の関係で黄経小数点以下3桁、地心距離は小数点以下4桁までにとどめる。

    implicit none

    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: RAD = 180.0d0 / PI

    double precision, intent(in) :: t
    double precision, intent(out) :: sun_ecliptic(2)

    double precision :: xpl(18, 3), rd(8, 3)
    double precision :: s, c, r, ecl
    integer :: i

    ! 黄経
    xpl( 1, 1:3) = (/ 1.9147d0, 35999.05d0, 267.52d0/)
    xpl( 2, 1:3) = (/ 0.0200d0, 71998.1d0,  265.10d0/)
    xpl( 3, 1:3) = (/ 0.0020d0, 32964.0d0,  158.00d0/)
    xpl( 4, 1:3) = (/ 0.0018d0,    19.0d0,  159.00d0/)
    xpl( 5, 1:3) = (/ 0.0018d0,445267.0d0,  208.00d0/)
    xpl( 6, 1:3) = (/ 0.0015d0, 45038.0d0,  254.00d0/)
    xpl( 7, 1:3) = (/ 0.0013d0, 22519.0d0,  352.00d0/)
    xpl( 8, 1:3) = (/ 0.0007d0, 65929.0d0,   45.00d0/)
    xpl( 9, 1:3) = (/ 0.0007d0,  3035.0d0,  110.00d0/)
    xpl(10, 1:3) = (/ 0.0007d0,  9038.0d0,   64.00d0/)
    xpl(11, 1:3) = (/ 0.0006d0, 33718.0d0,  316.00d0/)
    xpl(12, 1:3) = (/ 0.0005d0,   155.0d0,  118.00d0/)
    xpl(13, 1:3) = (/ 0.0005d0,  2281.0d0,  221.00d0/)
    xpl(14, 1:3) = (/ 0.0004d0, 29930.0d0,   48.00d0/)
    xpl(15, 1:3) = (/ 0.0004d0, 31557.0d0,  161.00d0/)
    xpl(16, 1:3) = (/-0.0048d0, 35999.00d0, 267.52d0/)
    xpl(17, 1:3) = (/ 0.0048d0,  1934.0d0,  145.00d0/)
    xpl(18, 1:3) = (/-0.0004d0, 72002.0d0,  111.00d0/)

    ! 地心距離
    rd(1, 1:3) = (/ 0.016706d0,  35999.05d0, 177.53d0/)
    rd(2, 1:3) = (/ 0.000139d0,  71998.00d0, 175.00d0/)
    rd(3, 1:3) = (/ 0.000031d0, 445267.00d0, 298.00d0/)
    rd(4, 1:3) = (/ 0.000016d0,  32964.00d0,  68.00d0/)
    rd(5, 1:3) = (/ 0.000016d0,  45038.00d0, 164.00d0/)
    rd(6, 1:3) = (/ 0.000005d0,  32519.00d0, 233.00d0/)
    rd(7, 1:3) = (/ 0.000005d0,  33718.00d0, 226.00d0/)
    rd(8, 1:3) = (/-0.000042d0,  35999.00d0, 178.00d0/)

    s = 36000.7695d0 * t + 280.4602d0
    do i = 1, 15
        s = s + xpl(i, 1) * cos((xpl(i, 2) * t + xpl(i, 3)) / RAD)
    end do

    c = 0.0d0
    do i = 16, 18
        c = c + xpl(i, 1) * t * cos((xpl(i, 2) * t + xpl(i, 3)) / RAD)
    end do

    r = 1.00014d0
    do i = 1, 7
        r = r + rd(i, 1) * cos((rd(i, 2) * t + rd(i, 3)) / RAD)
    end do
    r = r + rd(8, 1) * t * cos((xpl(8, 2) * t + xpl(8, 3)) / RAD)

    ecl = s + c + 0.00569d0
    sun_ecliptic(1) = Mla(ecl)
    sun_ecliptic(2) = r

    return
    contains
    double precision function Mla(x)
    ! x からnの整数倍を差し引いた値を返す

        implicit none
        
        double precision, intent(in) :: x
        double precision :: val

        val = 360.0d0 * (x / 360.0d0 - int(x / 360.d0))
        if (val < 0) then
            val = val + 360.0d0
        end if

        Mla = val

        return
    end function Mla
end subroutine SunEclipticLongitude

subroutine EquatorialToEcliptic(t, ra0, dc0, era, edc)
! 赤道座標から黄道座標への変換
! 引　数: 時刻引数および赤道座標
!        t : 1ユリウス世紀数(JD - 2451545.0) / 36525.0)
!        ora: 赤経
!        odc: 赤緯
! 戻り値: 黄経・黄緯を角度で返す
!        ecliptic(1): 黄経
!        ecliptic(2): 黄緯

    implicit none

    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: RAD = 180.0d0 / PI

    double precision ,intent(in):: t, ra0, dc0
    double precision, intent(out) :: era, edc

    double precision :: ra, dc
    double precision :: ep
    double precision :: l, m, n, u, v, w
    double precision :: ma(3, 3), mb(3, 1), mc(3, 1)

    ! 平均黄道傾斜角の計算
    call MeanObliquityEcliptic(t, ep)
    ep = ep / RAD

    ! 赤経・赤緯から方向余弦を計算する
    ra = ra0 / RAD
    dc = dc0 / RAD
    l = cos(dc) * cos(ra)
    m = cos(dc) * sin(ra)
    n = sin(dc)

    ! 黄経・黄緯の計算
    ma(1, 1:3) = (/1.0d0,    0.0d0,   0.0d0/)
    ma(2, 1:3) = (/0.0d0,  cos(ep), sin(ep)/)
    ma(3, 1:3) = (/0.0d0, -sin(ep), cos(ep)/)
    mb(1, 1) = l
    mb(2, 1) = m
    mb(3, 1) = n
    mc = matmul(ma, mb)
    u = mc(1, 1)
    v = mc(2, 1)
    w = mc(3, 1)

    call Quadrant(v, u, era)
    era = era * RAD
    edc = asin(sin(w)) * RAD

    return
end subroutine EquatorialToEcliptic

subroutine AnnualParallax(t, ra0, dc0, pai0, star_position)
! 年周視差の計算
! 天体の位置計算 増補版 P.66 - P.72
! 引　数:  ユリウス世紀
!          t:  2000年1月0日正午からの経過日数をユリウス世紀数で表したもの
!          ra0: 恒星の赤経
!          dc0: 恒星の赤緯
!          pai: 恒星の年周視差
! 戻り値:  年周視差によって変化した赤経および赤緯の方向余弦を返す
!          star_position(1): l4
!          star_position(2): m4
!          star_position(3): n4

    implicit none

    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: RAD = 180.0d0 / PI

    double precision, intent(in) :: t
    double precision, intent(in) :: ra0, dc0, pai0
    double precision, intent(out) :: star_position(3)

    double precision :: ra, dc, l, m, n
    double precision :: l4, m4, n4
    double precision :: ls, ms, ns
    double precision :: mecl, secl
    double precision :: pai
    double precision :: sun_ecliptic(2)


    ! 平均黄道傾斜角
    call MeanObliquityEcliptic(t, mecl)
    mecl = mecl / RAD

    ! 太陽の黄経の方向余弦
    call SunEclipticLongitude(t, sun_ecliptic)
    secl = sun_ecliptic(1) / RAD

    ls = cos(secl)
    ms = sin(secl) * cos(mecl)
    ns = sin(secl) * sin(mecl)

    ! 赤経・赤緯から方向余弦を計算する
    ra = ra0 / RAD
    dc = dc0 / RAD
    l = cos(dc) * cos(ra)
    m = cos(dc) * sin(ra)
    n = sin(dc)

    ! 年周視差をラジアン値に換算
    pai = (pai0 / 3600.0d0) / RAD

    ! 年周視差を考慮した恒星位置の計算
    l4 = l + pai * ((1 - l**2) * ls - l * m * ms - l * n * ns)
    m4 = m + pai * (-l * m * ls + (1 - m**2) * ms - m * n * ns)
    n4 = n + pai * (-l * n * ls - m * n * ms + (1 - n**2)* ns)

    star_position(1) = l4
    star_position(2) = m4
    star_position(3) = n4

    return
end subroutine AnnualParallax

subroutine AnnualAberration(t, ra0, dc0, star_position)
! 年周光行差の計算
! 天体の位置計算 増補版 P.74 - P.82
! 引　数:  ユリウス世紀
!          t:  2000年1月0日正午からの経過日数をユリウス世紀数で表したもの
!          ra0: 恒星の赤経
!          dc0: 恒星の赤緯
! 戻り値:  年周光行差によって変化した赤経および赤緯の方向余弦を返す
!          star_position(1): l5
!          star_position(2): m5
!          star_position(3): n5

    implicit none

    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: RAD = 180.0d0 / PI
    double precision, parameter :: KP = 20.49552d0

    double precision, intent(in) :: t
    double precision, intent(in) :: ra0, dc0
    double precision, intent(out) :: star_position(3)

    double precision :: ra, dc, l, m, n
    double precision :: l5, m5, n5
    double precision :: ls, ms, ns
    double precision :: mecl, secl
    double precision :: k
    double precision :: sun_ecliptic(2)


    ! 平均黄道傾斜角
    call MeanObliquityEcliptic(t, mecl)
    mecl = mecl / RAD

    ! 太陽の黄経の方向余弦
    call SunEclipticLongitude(t, sun_ecliptic)
    secl = sun_ecliptic(1) / RAD

    ls = sin(secl)
    ms = cos(secl) * cos(mecl)
    ns = cos(secl) * sin(mecl)

    ! 赤経・赤緯から方向余弦を計算する
    ra = ra0 / RAD
    dc = dc0 / RAD
    l = cos(dc) * cos(ra)
    m = cos(dc) * sin(ra)
    n = sin(dc)

    ! 光行差定数をラジアン値に換算
    k = (KP / 3600.0d0) / RAD

    ! 年周視差を考慮した恒星位置の計算
    l5 = l + k * ((1 - l**2) * ls + l * m * ms + l * n * ns)
    m5 = m + k * (-l * m * ls - (1 - m**2) * ms + m * n * ns)
    n5 = n + k * (-l * n * ls + m * n * ms - (1 - n**2)* ns)

    star_position(1) = l5
    star_position(2) = m5
    star_position(3) = n5

    return
end subroutine AnnualAberration
