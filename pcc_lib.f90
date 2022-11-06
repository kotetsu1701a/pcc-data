module pcclib
    implicit none
contains
    function Frc(x) result(val)
    ! 小数部を返す

        double precision, intent(in) :: x
        double precision :: val

        val = x - int(x);
    end Function Frc

    function Mla(x) result(val)
    ! 360°の整数倍を返す
    
        double precision, parameter :: PI = 3.141592653589793238462643d0
        double precision, parameter :: PI2 = 6.283185307179586476925287d0

        double precision, intent(in) :: x
        double precision :: val

        val = PI2 * (x / 360d0 - int(x / 360d0));
    end function Mla

    function Mla2(x) result(val)
    ! 360°の整数倍を返す

        double precision, intent(in) :: x
        double precision :: val

        val = 360d0 * frc(x / 360d0)
        if (val < 0) then
            val = val + 360d0;
        end if
    end function Mla2

    function Sgn(x) result(flg)
    ! 正負の符号を返す

        double precision, intent(in) :: x
        integer ::  flg

        flg = 1
        if (x < 0) then
            flg = -1
        end if
    end function Sgn

    function hms2deg(rh, rm, rs) result(deg)
    ! 時分秒を角度に換算して返す

        double precision, intent(in) :: rh, rm, rs
        double precision :: deg

        deg = 15.0d0 * (abs(rh) + rm / 60d0 +rs / 3600d0)
    end function hms2deg

    function dms2deg(dh, dm, ds) result(deg)
    ! 度分秒を角度に換算して返す

        double precision, intent(in) :: dh, dm, ds
        double precision :: deg

        deg = Sgn(dh) * (abs(dh) + dm / 60d0 + ds / 3600d0)
    end function dms2deg

    function deg2hms(deg) result(ra)
    ! 角度から時分秒を返す

        double precision, intent(in) :: deg
        double precision :: degree
        double precision :: rh, rm, rs, ra(3)

        degree = abs(deg) / 15.0d0
        rh = int(degree)
        rs = 60.0d0 * (degree - int(degree))
        rm = int(rs)
        rs = 60.0d0 * (rs-rm)

        ra(1) = rh
        ra(2) = rm
        ra(3) = rs
    end function deg2hms

    function deg2dms(degree) result(dc)
    ! 角度から度分秒を返す

        double precision, intent(in) :: degree
        double precision :: dh, dm, ds, dc(3)
        integer :: sg

        sg = Sgn(degree)
        dh = int(abs(degree))
        ds = 60.0d0 * (abs(degree) - int(abs(degree)))
        dm = int(ds)
        ds = 60.0d0 * (ds-dm)

        dc(1) = sg * dh
        dc(2) = dm
        dc(3) = ds
    end function deg2dms

    function deg2rad(x) result(radian)
    ! 角度をラジアンに換算する

        double precision, parameter :: PI = 3.141592653589793238462643d0
        double precision, parameter :: RAD = 180d0 / PI
    
        double precision, intent(in) :: x
        double precision :: radian

        radian = x / RAD
    end function deg2rad

    function rad2deg(x) result(degree)
    ! ラジアンを角度に換算する

        double precision, parameter :: PI = 3.141592653589793238462643d0
        double precision, parameter :: RAD = 180d0 / PI
    
        double precision, intent(in) :: x
        double precision :: degree

        degree = x * RAD
    end function rad2deg

    function Quadrant(ss, cc) result(tt)
    ! 象限の判断　「マイコン宇宙講座」79頁　リスト3-13より

        double precision, parameter :: PI = 3.141592653589793238462643d0
        double precision, parameter :: PI2 = 6.283185307179586476925287d0

        double precision, intent(in) :: ss, cc
        double precision :: tt

        tt = atan(ss / cc)

        if (cc < 0) then
            tt = tt + PI
        else if (ss < 0) then
                tt = tt + PI2
        end if
    end function Quadrant  
    
    function ModifiedJulianDay(year, month, day, hour, minute, second) result(mjd)
    ! 修正ユリウス日
    ! JSTから世界時の修正ユリウス日を返す

        double precision, intent(in) :: year, month, day, hour, minute, second

        double precision :: jd, mjd

        jd = JulianDay(year, month, day)
        jd = jd + (hour / 24d0 + minute / 1440d0 + second / 86400d0)
        jd = jd - 0.375d0
        mjd = jd - 2400000.5d0
    end function ModifiedJulianDay

    function JulianDay(year, month, day) result(julian)
    ! ユリウス日(0 UTC)

        double precision, intent(in) :: year, month, day

        double precision :: julian
        double precision :: yy, mm, dd, branch

        yy = year
        mm = month
        dd = day

        branch = yy + (mm - 1d0) / 12d0 + dd / 365.25d0

        if (int(mm) < 3d0) then
            mm = mm + 12d0
            yy = yy - 1d0
        end if

        if (branch >= 1582.78d0) then
            julian = int(yy * 365.25d0) + int(yy / 400d0) - int(yy / 100d0) + int(30.59d0 * (mm - 2d0)) + dd + 1721088.5d0
        else if (branch >= 0d0) then
            julian = int(yy * 365.25d0) + int(30.59d0 * (mm - 2d0)) + dd + 1721086.5d0
        else if (year < 0d0) then
            julian = Sgn(year) * int(abs(yy) * 365.25d0) + int(30.59d0 * (mm - 2d0)) + dd + 1721085.5d0
        end if
    end function JulianDay

    function Julian2date(julian) result(dt)
    ! 「マイコン宇宙講座」サブルーチン JDATE　リスト2-2

        integer, dimension(1:12) :: T = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)

        double precision, intent(in) :: julian
        double precision :: dt(3), year, month, day
        double precision :: jj, jd, r1, r2
        integer :: m

        jj = julian - 2400000.5d0
        year = int(2.7379093d-03 * (julian - 2400000.5d0) + 1858.877d0)
        month = 1.0d0
        day = 0.0d0

        jd = JulianDay(year, month, day)

        r2 = jj - (jd - 2400000.5d0)
        if (mod(year, 4d0) == 0d0 .and. mod(year, 100d0) /= 0d0 .or. mod(year, 400d0) == 0d0) then
            T(2) = 29
        end if

        r1 = 0d0
        m = 1
        do while (m < 13)
            if (int(r2) - r1 - T(m) <= 0) then
                exit
            end if
            r1 = r1 + T(m)
            m = m + 1
        end do

        month = m
        day = r2 - r1
        T(2) = 28
    
        if (month == 13d0) then
            year = year + 1d0
            month = month - 12d0
        end if

        dt(1) = year
        dt(2) = month
        dt(3) = day
    end function Julian2date

    function Julian2date_2(julian) result(dtime)
    ! ユリウス日から年月日を求める
    !「天文計算入門」63頁 式(16.6)
        
        double precision, intent(in) :: julian
        double precision :: year, month, day
        double precision :: dtime(3)
        double precision :: a, b, c, e, f, g, h

        a = int(julian + 68569.5d0)
        b = int(a / 36524.25d0)
        c = a - int(36524.25d0 * b + 0.75d0)
        e = int((c + 1) / 365.25025d0)
        f = c - int(365.25d0 * e) + 31d0
        g = int(f / 30.59d0)
        day = f - int(30.59d0 * g) + (julian + 0.5d0) - int(julian + 0.5d0)
        h = int(g / 11d0)
        month = int(g - 12d0 * h + 2d0)
        year = int(100d0 * (b - 49d0) + e + h)
        
        ! 12月32日になったときの処理
        if (int(month) == 12d0) then
            if (int(day) > 31d0) then
                year = year + 1d0
                month = (month + 1d0) - 12d0
                day = 1d0
            end if
        end if

        dtime(1) = year
        dtime(2) = month
        dtime(3) = day
    end function Julian2date_2

    function BesselianYear(julian) result(t)
        ! 1950のベッセル年初からの経過日数を36524.21で測ったものを返す。
        ! 「マイコン宇宙講座」31頁　式（2.3.1）より
    
        double precision, parameter :: JDBEPOCH = 2433282.42346d0
    
        double precision, intent(in) :: julian
        double precision :: t
    
        t = julian - JDBEPOCH
        t = t * (0.00002737909288d0 + 1.25013d-17 * t)
    end function BesselianYear

    function localSDT(julian) result(lsdt)
    ! グリニッジ地方恒星時
    
        double precision, parameter :: PI = 3.141592653589793238462643d0
        double precision, parameter :: PI2 = 6.283185307179586476925287d0
    
        double precision, intent(in) :: julian
        double precision :: lsdt
    
        lsdt = PI2 * Frc(0.7769194d0 + 1.002737909265d0 * (julian - 2415020.0d0))
    end function LocalSDT
    
    function MeanSDT50(t) result(msdt)
    ! グリニッジ平均恒星時(1950)
    ! t = (JD - 2415020.0) / 36525.0
    
        double precision, intent(in) :: t
        double precision :: msdt
    
        msdt = Mla(99.690983333d0 + 36000.768925d0 * t + 0.0003870833d0 * t**2)
    end function MeanSDT50
    
    function MeanSDT80(t) result(msdt)
    ! グリニッジ平均恒星時(1984)
    ! t = (JD - 2451545.0) / 36525.0
    
            double precision, intent(in) :: t
            double precision :: msdt
    
            msdt = Mla(100.460618375d0 + 36000.770053608d0 * t + 0.000387933d0 * t**2 - 0.00000002583d0 * t**3)
    end function MeanSDT80
    
    function MeanSDT(julian, t) result(gmst)
    ! グリニッジ平均恒星時
    ! t = (JD - 2451545.0) / 36525.0
    
        double precision, parameter :: PI = 3.141592653589793238462643d0
        double precision, parameter :: PI2 = 6.283185307179586476925287d0
        double precision, parameter :: RAD = 180d0 / PI
    
        double precision, intent(in) :: julian, t
        double precision :: gmst, era
    
        era = 0.7790572732640d0 + 1.00273781191135448d0 * (julian - 2451545.0d0)
        era = PI2 * (era - int(era))
        
        gmst = 0.014506d0 + 4612.156534d0 * t + 1.3915817d0 * t**2 - 0.00000044d0 * t**3 &
             - 0.000029956d0 * t**4 - 0.0000000368d0 * t**5
        gmst = gmst / 3600d0
    
        gmst = ((era * RAD) + gmst) / 15d0
    end function MeanSDT
    
    function iauMOE50(t) result(moe)
    ! 平均黄道傾斜角(1950)
    ! t = (JD - 2415020.0) / 36525.0
    
        double precision, intent(in) :: t
        double precision :: moe
    
        moe = 23.45229444d0 - 0.0130125d0 * t - 0.0000016389d0 * t**2 + 0.00000050278d0 * t**3
    end function iauMOE50
    
    function iauMOE80(t) result(moe)
    ! 平均黄道傾斜角(1984)
    ! t = (JD - 2451545.0) / 36525.0
    
        double precision, intent(in) :: t
        double precision :: moe
            
        moe = 84381.448d0 - 46.8150d0 * t - 0.00059d0 * t**2 + 0.001813d0 * t**3
        moe = moe / 3600d0
    end function iauMOE80
            
    function iauMOE(t) result(moe)
    ! 平均黄道傾斜角
    ! t = (JD - 2451545.0) / 36525.0
    
        double precision, intent(in) :: t
        double precision :: moe
    
        moe = 84381.406d0 - 46.836769d0 * t - 0.0001831d0 * t**2 + 0.0020034d0 * t**3 - 0.000000576d0 * t**4 - 0.0000000434d0 * t**5
        moe = moe / 3600d0
    end function iauMOE

    function iauNut80(dt, t) result(eps)
    ! 章動の計算
    ! 「天体の位置計算 増補版」229-232頁

        double precision, intent(in) :: dt, t

        double precision :: xpl(106, 9)
        double precision :: arg, s, c, dt2, dt3
        double precision :: dp             ! 黄経の章動
        double precision :: de             ! 黄道傾斜角の章動
        double precision :: el             ! 月の平均近点角
        double precision :: elp            ! 太陽の平均近点角
        double precision :: f              ! 昇交点から測った月の平均黄経
        double precision :: d              ! 太陽と月の平均離角
        double precision :: om             ! 月の平均昇交点黄経
        double precision :: eps(2)
        integer :: i

        xpl(  1, 1:9) = (/ 0d0,  0d0,  0d0,  0d0,  1d0, -17.1996d0, -0.01742d0,  9.2025d0,  0.00089d0/)
        xpl(  2, 1:9) = (/ 0d0,  0d0,  0d0,  0d0,  2d0,   0.2062d0, -0.00002d0, -0.0895d0,  0.00005d0/)
        xpl(  3, 1:9) = (/-2d0,  0d0,  2d0,  0d0,  1d0,   0.0046d0,  0.00000d0, -0.0024d0,  0.00000d0/)
        xpl(  4, 1:9) = (/ 2d0,  0d0, -2d0,  0d0,  0d0,   0.0011d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl(  5, 1:9) = (/-2d0,  0d0,  2d0,  0d0,  2d0,  -0.0003d0,  0.00000d0,  0.0001d0,  0.00000d0/)
        xpl(  6, 1:9) = (/ 1d0, -1d0,  0d0, -1d0,  0d0,  -0.0003d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl(  7, 1:9) = (/ 0d0, -2d0,  2d0, -2d0,  1d0,  -0.0002d0,  0.00000d0,  0.0001d0,  0.00000d0/)
        xpl(  8, 1:9) = (/ 2d0,  0d0, -2d0,  0d0,  1d0,   0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl(  9, 1:9) = (/ 0d0,  0d0,  2d0, -2d0,  2d0,  -1.3187d0, -0.00016d0,  0.5736d0, -0.00031d0/)
        xpl( 10, 1:9) = (/ 0d0,  1d0,  0d0,  0d0,  0d0,   0.1426d0, -0.00034d0,  0.0054d0, -0.00001d0/)
        xpl( 11, 1:9) = (/ 0d0,  1d0,  2d0, -2d0,  2d0,  -0.0517d0,  0.00012d0,  0.0224d0, -0.00006d0/)
        xpl( 12, 1:9) = (/ 0d0, -1d0,  2d0, -2d0,  2d0,   0.0217d0, -0.00005d0, -0.0095d0,  0.00003d0/)
        xpl( 13, 1:9) = (/ 0d0,  0d0,  2d0, -2d0,  1d0,   0.0129d0,  0.00001d0, -0.0070d0,  0.00000d0/)
        xpl( 14, 1:9) = (/ 2d0,  0d0,  0d0, -2d0,  0d0,   0.0048d0,  0.00000d0,  0.0001d0,  0.00000d0/)
        xpl( 15, 1:9) = (/ 0d0,  0d0,  2d0, -2d0,  0d0,  -0.0022d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 16, 1:9) = (/ 0d0,  2d0,  0d0,  0d0,  0d0,   0.0017d0, -0.00001d0,  0.0000d0,  0.00000d0/)
        xpl( 17, 1:9) = (/ 0d0,  1d0,  0d0,  0d0,  1d0,  -0.0015d0,  0.00000d0,  0.0009d0,  0.00000d0/)
        xpl( 18, 1:9) = (/ 0d0,  2d0,  2d0, -2d0,  2d0,  -0.0016d0,  0.00001d0,  0.0007d0,  0.00000d0/)
        xpl( 19, 1:9) = (/ 0d0, -1d0,  0d0,  0d0,  1d0,  -0.0012d0,  0.00000d0,  0.0006d0,  0.00000d0/)
        xpl( 20, 1:9) = (/-2d0,  0d0,  0d0,  2d0,  1d0,  -0.0006d0,  0.00000d0,  0.0003d0,  0.00000d0/)
        xpl( 21, 1:9) = (/ 0d0, -1d0,  2d0, -2d0,  1d0,  -0.0005d0,  0.00000d0,  0.0003d0,  0.00000d0/)
        xpl( 22, 1:9) = (/ 2d0,  0d0,  0d0, -2d0,  1d0,   0.0004d0,  0.00000d0, -0.0002d0,  0.00000d0/)
        xpl( 23, 1:9) = (/ 0d0,  1d0,  2d0, -2d0,  1d0,   0.0004d0,  0.00000d0, -0.0002d0,  0.00000d0/)
        xpl( 24, 1:9) = (/ 1d0,  0d0,  0d0, -1d0,  0d0,  -0.0004d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 25, 1:9) = (/ 2d0,  1d0,  0d0, -2d0,  0d0,   0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 26, 1:9) = (/ 0d0,  0d0, -2d0,  2d0,  1d0,   0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 27, 1:9) = (/ 0d0,  1d0, -2d0,  2d0,  0d0,  -0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 28, 1:9) = (/ 0d0,  1d0,  0d0,  0d0,  2d0,   0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 29, 1:9) = (/-1d0,  0d0,  0d0,  1d0,  1d0,   0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 30, 1:9) = (/ 0d0,  1d0,  2d0, -2d0,  0d0,  -0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 31, 1:9) = (/ 0d0,  0d0,  2d0,  0d0,  2d0,  -0.2274d0, -0.00002d0,  0.0977d0, -0.00005d0/)
        xpl( 32, 1:9) = (/ 1d0,  0d0,  0d0,  0d0,  0d0,   0.0712d0,  0.00001d0, -0.0007d0,  0.00000d0/)
        xpl( 33, 1:9) = (/ 0d0,  0d0,  2d0,  0d0,  1d0,  -0.0386d0, -0.00004d0,  0.0200d0,  0.00000d0/)
        xpl( 34, 1:9) = (/ 1d0,  0d0,  2d0,  0d0,  2d0,  -0.0301d0,  0.00000d0,  0.0129d0, -0.00001d0/)
        xpl( 35, 1:9) = (/ 1d0,  0d0,  0d0, -2d0,  0d0,  -0.0158d0,  0.00000d0, -0.0001d0,  0.00000d0/)
        xpl( 36, 1:9) = (/-1d0,  0d0,  2d0,  0d0,  2d0,   0.0123d0,  0.00000d0, -0.0053d0,  0.00000d0/)
        xpl( 37, 1:9) = (/ 0d0,  0d0,  0d0,  2d0,  0d0,   0.0063d0,  0.00000d0, -0.0002d0,  0.00000d0/)
        xpl( 38, 1:9) = (/ 1d0,  0d0,  0d0,  0d0,  1d0,   0.0063d0,  0.00001d0, -0.0033d0,  0.00000d0/)
        xpl( 39, 1:9) = (/-1d0,  0d0,  0d0,  0d0,  1d0,  -0.0058d0, -0.00001d0,  0.0032d0,  0.00000d0/)
        xpl( 40, 1:9) = (/-1d0,  0d0,  2d0,  2d0,  2d0,  -0.0059d0,  0.00000d0,  0.0026d0,  0.00000d0/)
        xpl( 41, 1:9) = (/ 1d0,  0d0,  2d0,  0d0,  1d0,  -0.0051d0,  0.00000d0,  0.0027d0,  0.00000d0/)
        xpl( 42, 1:9) = (/ 0d0,  0d0,  2d0,  2d0,  2d0,  -0.0038d0,  0.00000d0,  0.0016d0,  0.00000d0/)
        xpl( 43, 1:9) = (/ 2d0,  0d0,  0d0,  0d0,  0d0,   0.0029d0,  0.00000d0, -0.0001d0,  0.00000d0/)
        xpl( 44, 1:9) = (/ 1d0,  0d0,  2d0, -2d0,  2d0,   0.0029d0,  0.00000d0, -0.0012d0,  0.00000d0/)
        xpl( 45, 1:9) = (/ 2d0,  0d0,  2d0,  0d0,  2d0,  -0.0031d0,  0.00000d0,  0.0013d0,  0.00000d0/)
        xpl( 46, 1:9) = (/ 0d0,  0d0,  2d0,  0d0,  0d0,   0.0026d0,  0.00000d0, -0.0001d0,  0.00000d0/)
        xpl( 47, 1:9) = (/-1d0,  0d0,  2d0,  0d0,  1d0,   0.0021d0,  0.00000d0, -0.0010d0,  0.00000d0/)
        xpl( 48, 1:9) = (/-1d0,  0d0,  0d0,  2d0,  1d0,   0.0016d0,  0.00000d0, -0.0008d0,  0.00000d0/)
        xpl( 49, 1:9) = (/ 1d0,  0d0,  0d0, -2d0,  1d0,  -0.0013d0,  0.00000d0,  0.0007d0,  0.00000d0/)
        xpl( 50, 1:9) = (/-1d0,  0d0,  2d0,  2d0,  1d0,  -0.0010d0,  0.00000d0,  0.0005d0,  0.00000d0/)
        xpl( 51, 1:9) = (/ 1d0,  1d0,  0d0, -2d0,  0d0,  -0.0007d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 52, 1:9) = (/ 0d0,  1d0,  2d0,  0d0,  2d0,   0.0007d0,  0.00000d0, -0.0003d0,  0.00000d0/)
        xpl( 53, 1:9) = (/ 0d0, -1d0,  2d0,  0d0,  2d0,  -0.0007d0,  0.00000d0,  0.0003d0,  0.00000d0/)
        xpl( 54, 1:9) = (/ 1d0,  0d0,  2d0,  2d0,  2d0,  -0.0008d0,  0.00000d0,  0.0003d0,  0.00000d0/)
        xpl( 55, 1:9) = (/ 1d0,  0d0,  0d0,  2d0,  0d0,   0.0006d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 56, 1:9) = (/ 2d0,  0d0,  2d0, -2d0,  2d0,   0.0006d0,  0.00000d0, -0.0003d0,  0.00000d0/)
        xpl( 57, 1:9) = (/ 0d0,  0d0,  0d0,  2d0,  1d0,  -0.0006d0,  0.00000d0,  0.0003d0,  0.00000d0/)
        xpl( 58, 1:9) = (/ 0d0,  0d0,  2d0,  2d0,  1d0,  -0.0007d0,  0.00000d0,  0.0003d0,  0.00000d0/)
        xpl( 59, 1:9) = (/ 1d0,  0d0,  2d0, -2d0,  1d0,   0.0006d0,  0.00000d0, -0.0003d0,  0.00000d0/)
        xpl( 60, 1:9) = (/ 0d0,  0d0,  0d0, -2d0,  1d0,  -0.0005d0,  0.00000d0,  0.0003d0,  0.00000d0/)
        xpl( 61, 1:9) = (/ 1d0, -1d0,  0d0,  0d0,  0d0,   0.0005d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 62, 1:9) = (/ 2d0,  0d0,  2d0,  0d0,  1d0,  -0.0005d0,  0.00000d0,  0.0003d0,  0.00000d0/)
        xpl( 63, 1:9) = (/ 0d0,  1d0,  0d0, -2d0,  0d0,  -0.0004d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 64, 1:9) = (/ 1d0,  0d0, -2d0,  0d0,  0d0,   0.0004d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 65, 1:9) = (/ 0d0,  0d0,  0d0,  1d0,  0d0,  -0.0004d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 66, 1:9) = (/ 1d0,  1d0,  0d0,  0d0,  0d0,  -0.0003d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 67, 1:9) = (/ 1d0,  0d0,  2d0,  0d0,  0d0,   0.0003d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 68, 1:9) = (/ 1d0, -1d0,  2d0,  0d0,  2d0,  -0.0003d0,  0.00000d0,  0.0001d0,  0.00000d0/)
        xpl( 69, 1:9) = (/-1d0, -1d0,  2d0,  2d0,  2d0,  -0.0003d0,  0.00000d0,  0.0001d0,  0.00000d0/)
        xpl( 70, 1:9) = (/-2d0,  0d0,  0d0,  0d0,  1d0,  -0.0002d0,  0.00000d0,  0.0001d0,  0.00000d0/)
        xpl( 71, 1:9) = (/ 3d0,  0d0,  2d0,  0d0,  2d0,  -0.0003d0,  0.00000d0,  0.0001d0,  0.00000d0/)
        xpl( 72, 1:9) = (/ 0d0, -1d0,  2d0,  2d0,  2d0,  -0.0003d0,  0.00000d0,  0.0001d0,  0.00000d0/)
        xpl( 73, 1:9) = (/ 1d0,  1d0,  2d0,  0d0,  2d0,   0.0002d0,  0.00000d0, -0.0001d0,  0.00000d0/)
        xpl( 74, 1:9) = (/-1d0,  0d0,  2d0, -2d0,  1d0,  -0.0002d0,  0.00000d0,  0.0001d0,  0.00000d0/)
        xpl( 75, 1:9) = (/ 2d0,  0d0,  0d0,  0d0,  1d0,   0.0002d0,  0.00000d0, -0.0001d0,  0.00000d0/)
        xpl( 76, 1:9) = (/ 1d0,  0d0,  0d0,  0d0,  2d0,  -0.0002d0,  0.00000d0,  0.0001d0,  0.00000d0/)
        xpl( 77, 1:9) = (/ 3d0,  0d0,  0d0,  0d0,  0d0,   0.0002d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 78, 1:9) = (/ 0d0,  0d0,  2d0,  1d0,  2d0,   0.0002d0,  0.00000d0, -0.0001d0,  0.00000d0/)
        xpl( 79, 1:9) = (/-1d0,  0d0,  0d0,  0d0,  2d0,   0.0001d0,  0.00000d0, -0.0001d0,  0.00000d0/)
        xpl( 80, 1:9) = (/ 1d0,  0d0,  0d0, -4d0,  0d0,  -0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 81, 1:9) = (/-2d0,  0d0,  2d0,  2d0,  2d0,   0.0001d0,  0.00000d0, -0.0001d0,  0.00000d0/)
        xpl( 82, 1:9) = (/-1d0,  0d0,  2d0,  4d0,  2d0,  -0.0002d0,  0.00000d0,  0.0001d0,  0.00000d0/)
        xpl( 83, 1:9) = (/ 2d0,  0d0,  0d0, -4d0,  0d0,  -0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 84, 1:9) = (/ 1d0,  1d0,  2d0, -2d0,  2d0,   0.0001d0,  0.00000d0, -0.0001d0,  0.00000d0/)
        xpl( 85, 1:9) = (/ 1d0,  0d0,  2d0,  2d0,  1d0,  -0.0001d0,  0.00000d0,  0.0001d0,  0.00000d0/)
        xpl( 86, 1:9) = (/-2d0,  0d0,  2d0,  4d0,  2d0,  -0.0001d0,  0.00000d0,  0.0001d0,  0.00000d0/)
        xpl( 87, 1:9) = (/-1d0,  0d0,  4d0,  0d0,  2d0,   0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 88, 1:9) = (/ 1d0, -1d0,  0d0, -2d0,  0d0,   0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 89, 1:9) = (/ 2d0,  0d0,  2d0, -2d0,  1d0,   0.0001d0,  0.00000d0, -0.0001d0,  0.00000d0/)
        xpl( 90, 1:9) = (/ 2d0,  0d0,  2d0,  2d0,  2d0,  -0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 91, 1:9) = (/ 1d0,  0d0,  0d0,  2d0,  1d0,  -0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 92, 1:9) = (/ 0d0,  0d0,  4d0, -2d0,  2d0,   0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 93, 1:9) = (/ 3d0,  0d0,  2d0, -2d0,  2d0,   0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 94, 1:9) = (/ 1d0,  0d0,  2d0, -2d0,  0d0,  -0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 95, 1:9) = (/ 0d0,  1d0,  2d0,  0d0,  1d0,   0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 96, 1:9) = (/-1d0, -1d0,  0d0,  2d0,  1d0,   0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 97, 1:9) = (/ 0d0,  0d0, -2d0,  0d0,  1d0,  -0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 98, 1:9) = (/ 0d0,  0d0,  2d0, -1d0,  2d0,  -0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl( 99, 1:9) = (/ 0d0,  1d0,  0d0,  2d0,  0d0,  -0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl(100, 1:9) = (/ 1d0,  0d0, -2d0, -2d0,  0d0,  -0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl(101, 1:9) = (/ 0d0, -1d0,  2d0,  0d0,  1d0,  -0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl(102, 1:9) = (/ 1d0,  1d0,  0d0, -2d0,  1d0,  -0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl(103, 1:9) = (/ 1d0,  0d0, -2d0,  2d0,  0d0,  -0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl(104, 1:9) = (/ 2d0,  0d0,  0d0,  2d0,  0d0,   0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl(105, 1:9) = (/ 0d0,  0d0,  2d0,  4d0,  2d0,  -0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)
        xpl(106, 1:9) = (/ 0d0,  1d0,  0d0,  1d0,  0d0,   0.0001d0,  0.00000d0,  0.0000d0,  0.00000d0/)

        dt2 = dt * dt
        dt3 = dt2 * dt
        el  = 0d0
        elp = 0d0
        f   = 0d0
        d   = 0d0
        om  = 0d0
        
        ! 360°の整数倍を差し引く
        el  = Mla2(el)
        elp = Mla2(elp)
        f   = Mla2(f)
        d   = Mla2(d) 
        om  = Mla2(om)

        ! Standards of Fundamental Astronomy
        ! http:!www.iausofa.org/2019_0722_C/CompleteList.html
        ! from nut80.c
        dp = 0d0
        de = 0d0
        do i = 1, 69
            arg = el * xpl(i, 1) + elp * xpl(i, 2) + f * xpl(i, 3) + d * xpl(i, 4) + om * xpl(i, 5)
            s = (xpl(i, 6) + (xpl(i, 7) * t))
            c = (xpl(i, 8) + (xpl(i, 9) * t))
            if (s /= 0d0) then
                 dp = dp + s * sin(deg2rad(arg))
            end if
            if (c /= 0d0) then
                de = de + c * cos(deg2rad(arg))
            end if
        end do
        ! end of nut80.c

        eps(1) = dp
        eps(2) = de

    end function iauNut80

    function iauNut50(dt, t) result(eps)
    ! 章動の計算
    ! 「天体の位置計算 増補版」63-65頁

        double precision, intent(in) :: dt, t

        double precision :: xpl(69, 9)
        double precision :: arg, s, c, dt2, dt3
        double precision :: dp             ! 黄経の章動
        double precision :: de             ! 黄道傾斜角の章動
        double precision :: el             ! 月の平均近点角
        double precision :: elp            ! 太陽の平均近点角
        double precision :: f              ! 昇交点から測った月の平均黄経
        double precision :: d              ! 太陽と月の平均離角
        double precision :: om             ! 月の平均昇交点黄経
        double precision :: eps(2)
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

        dt2 = dt * dt
        dt3 = dt2 * dt
        el  = 296.104608d0 + 13.0649924465d0 * dt + 6.890d-12 * dt2 + 2.95d-20 * dt3
        elp = 358.475833d0 +  0.9856002669d0 * dt - 0.112d-12 * dt2 - 6.80d-20 * dt3
        f   =  11.250889d0 + 13.2293504490d0 * dt - 2.407d-12 * dt2 - 0.70d-20 * dt3
        d   = 350.737486d0 + 12.1907491914d0 * dt - 1.076d-12 * dt2 + 3.90d-20 * dt3
        om  = 259.183275d0 -  0.0529539222d0 * dt + 1.557d-12 * dt2 + 4.60d-20 * dt3
        
        ! 360°の整数倍を差し引く
        el  = Mla2(el)
        elp = Mla2(elp)
        f   = Mla2(f)
        d   = Mla2(d) 
        om  = Mla2(om)

        ! Standards of Fundamental Astronomy
        ! http:!www.iausofa.org/2019_0722_C/CompleteList.html
        ! from nut80.c
        dp = 0d0
        de = 0d0
        do i = 1, 69
            arg = el * xpl(i, 1) + elp * xpl(i, 2) + f * xpl(i, 3) + d * xpl(i, 4) + om * xpl(i, 5)
            s = (xpl(i, 6) + (xpl(i, 7) * t))
            c = (xpl(i, 8) + (xpl(i, 9) * t))
            if (s /= 0d0) then
                 dp = dp + s * sin(deg2rad(arg))
            end if
            if (c /= 0d0) then
                de = de + c * cos(deg2rad(arg))
            end if
        end do
        ! end of nut80.c

        eps(1) = dp
        eps(2) = de
    end function iauNut50    

    function AltitudeAzimuth(lsdt, la, ra, dc) result(hc)
    ! 赤道座標から地平座標への変換
    ! 「天体の位置計算 増補版」28-31頁

        double precision, intent(in) :: lsdt, la, ra, dc
        double precision :: hg, al, hi, ss, cc, hc(2)

        ! 時角の計算
        hg = lsdt - ra

        ! 方位角の計算
        ss = cos(dc) * sin(hg)
        cc = -cos(la) * sin(dc) + sin(la) * cos(dc) * cos(hg)
        al = Quadrant(ss, cc)

        ! 高度の計算
        hi = asin(sin(la) * sin(dc) + cos(la) * cos(dc) * cos(hg))

        hc(1) = al
        hc(2) = hi
    end function AltitudeAzimuth

    function ProperMotion(t, ra, dc, ra_proper, dc_proper, ap, rv) result(star_position)
    ! 固有運動補正による恒星位置のずれ
    ! 「天体の位置計算 増補版」41-49頁

        double precision, intent(in) :: t
        double precision, intent(in) :: ra, dc, ra_proper, dc_proper, ap, rv

        double precision :: star_position(3)
        double precision :: l1, m1, n1
        double precision :: mu, phi, sigma, ss, cc
        double precision :: ma(3, 3), mb(3, 3), mc(3, 3), md(3, 1), me(3, 1)

        ! 全固有運動速度の計算
        mu = sqrt((ra_proper * cos(dc))**2 + dc_proper**2)

        ! 固有運動の向きを計算
        ss = ra_proper * cos(dc)
        cc = dc_proper
        phi = Quadrant(ss, cc)

        ! 1年間における恒星の移動の角距離を計算
        sigma = mu * t * (1d0 - (ap / 1.496d8) * rv * (t * (365.2422d0 * 86400.0d0)))
        sigma = deg2rad(sigma)

        ! 方向余弦の計算
        ma(1, 1:3) = (/cos(ra), -sin(ra), 0d0/)
        ma(2, 1:3) = (/sin(ra),  cos(ra), 0d0/)
        ma(3, 1:3) = (/0d0,      0d0,     1d0/)

        mb(1, 1:3) = (/cos(dc),  0d0, -sin(dc)/)
        mb(2, 1:3) = (/0d0,      1d0,  0d0/)
        mb(3, 1:3) = (/sin(dc),  0d0,  cos(dc)/)

        mc(1, 1:3) = (/1d0,  0d0,      0d0/)
        mc(2, 1:3) = (/0d0,  cos(phi), sin(phi)/)
        mc(3, 1:3) = (/0d0, -sin(phi), cos(phi)/)

        md(1, 1) = cos(sigma)
        md(2, 1) = 0d0
        md(3, 1) = sin(sigma)

        me = matmul(matmul(ma, mb), matmul(mc, md))
        l1 = me(1, 1)
        m1 = me(2, 1)
        n1 = me(3, 1)

        star_position(1) = l1
        star_position(2) = m1
        star_position(3) = n1
    end function ProperMotion

    function Precession(t, ra, dc) result(star_position)
    ! 歳差による赤経・赤緯の変化
    ! 「天体の位置計算 増補版」49-55頁

        double precision, intent(in) :: t
        double precision, intent(in) :: ra, dc

        double precision :: star_position(3)
        double precision :: x1, x2, x3
        double precision :: l, m, n, l2, m2, n2
        double precision :: ma(3, 3), mb(3, 3), mc(3, 3), md(3, 1), me(3, 1)

        ! 赤経・赤緯から方向余弦を計算する
        l = cos(dc) * cos(ra)
        m = cos(dc) * sin(ra)
        n = sin(dc)

        ! 歳差定数の計算(EPOCH=1950)
        x1 = 2304.9480d0 * t + 0.302d0 * t**2 + 0.018d0 * t**3
        x2 = 2304.9480d0 * t + 1.093d0 * t**2 + 0.019d0 * t**3
        x3 = 2004.2555d0 * t - 0.426d0 * t**2 - 0.042d0 * t**3

        x1 = deg2rad(x1 / 3600d0)
        x2 = deg2rad(x2 / 3600d0)
        x3 = deg2rad(x3 / 3600d0)

        ! 方向余弦の計算
        ma(1, 1:3) = (/-sin(x2), -cos(x2), 0d0/)
        ma(2, 1:3) = (/ cos(x2), -sin(x2), 0d0/)
        ma(3, 1:3) = (/0d0,       0d0,     1d0/)

        mb(1, 1:3) = (/1d0,   0d0,      0d0/)
        mb(2, 1:3) = (/0d0,   cos(x3),  sin(x3)/)
        mb(3, 1:3) = (/0d0,  -sin(x3),  cos(x3)/)

        mc(1, 1:3) = (/ sin(x1),  cos(x1),  0d0/)
        mc(2, 1:3) = (/-cos(x1),  sin(x1),  0d0/)
        mc(3, 1:3) = (/0d0,       0d0,      1d0/)

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
    end function Precession

    function Nutation(dt, t, ra, dc) result(star_position)
    ! 章動による赤経・赤緯の変化
    ! 「天体の位置計算 増補版」 58-66頁

        double precision, intent(in) :: dt
        double precision, intent(in) :: t
        double precision, intent(in) :: ra, dc

        double precision :: ecl, dp, de
        double precision:: star_position(3), eps(2)
        double precision :: l, m, n, l3, m3, n3
        double precision :: ma(3, 3), mb(3, 3), mc(3, 3), md(3, 1), me(3, 1)

        ! 章動の計算
        eps = iauNut50(dt, t)

        dp = deg2rad(eps(1) / 3600d0)
        de = deg2rad(eps(2) / 3600d0)

        ! 平均黄道傾斜角の計算(1950)
        ecl = deg2rad(iauMOE50(t))

        ! 赤経・赤緯から方向余弦を計算する
        l = cos(dc) * cos(ra)
        m = cos(dc) * sin(ra)
        n = sin(dc)

        ! 方向余弦の計算
        ma(1, 1:3) = (/1d0, 0d0,            0d0/)
        ma(2, 1:3) = (/0d0, cos(ecl + de), -sin(ecl + de)/)
        ma(3, 1:3) = (/0d0, sin(ecl + de),  cos(ecl + de)/)

        mb(1, 1:3) = (/cos(dp), -sin(dp), 0d0/)
        mb(2, 1:3) = (/sin(dp),  cos(dp), 0d0/)
        mb(3, 1:3) = (/0d0,      0d0,     1d0/)

        mc(1, 1:3) = (/1d0,  0d0,      0d0/)
        mc(2, 1:3) = (/0d0,  cos(ecl), sin(ecl)/)
        mc(3, 1:3) = (/0d0, -sin(ecl), cos(ecl)/)

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
    end function Nutation

    function SunEclipticLongitude(t) result(sun_ecliptic)
    ! 太陽の黄経計算
    ! 海上保安庁水路部(現　海洋情報部)の略算式による計算

        double precision, intent(in) :: t
        double precision :: sun_ecliptic(2)

        double precision :: xpl(18, 3), rd(8, 3)
        double precision :: s, c, r, sun_ecl
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

        ! 黄経の計算
        s = 36000.7695d0 * t + 280.4602d0
        do i = 1, 15
            s = s + xpl(i, 1) * cos(deg2rad(xpl(i, 2) * t + xpl(i, 3)))
        end do

        c = 0.0d0
        do i = 16, 18
            c = c + xpl(i, 1) * t * cos(deg2rad(xpl(i, 2) * t + xpl(i, 3)))
        end do
        sun_ecl = s + c + 0.00569d0

        ! 動径の計算
        r = 1.00014d0
        do i = 1, 7
            r = r + rd(i, 1) * cos(deg2rad(rd(i, 2) * t + rd(i, 3)))
        end do
        r = r + rd(8, 1) * t * cos(deg2rad(xpl(8, 2) * t + xpl(8, 3)))

        sun_ecliptic(1) = Mla2(sun_ecl)
        sun_ecliptic(2) = r
    end function SunEclipticLongitude

    function EquatorialToEcliptic(t, ra, dc) result(star_position)
    ! 赤道座標から黄道座標への変換
    ! 「天体の位置計算 増補版」140-141頁

        double precision ,intent(in):: t, ra, dc
        double precision :: star_position(2)

        double precision :: era, edc
        double precision :: e, ss, cc, tt
        double precision :: l, m, n, u, v, w
        double precision :: ma(3, 3), mb(3, 1), me(3, 1)

        ! 平均黄道傾斜角の計算
        e = deg2rad(iauMOE50(t))

        ! 赤経・赤緯から方向余弦を計算する
        l = cos(dc) * cos(ra)
        m = cos(dc) * sin(ra)
        n = sin(dc)

        ! 黄経・黄緯の計算
        ma(1, 1:3) = (/1d0,  0d0,    0d0/)
        ma(2, 1:3) = (/0d0,  cos(e), sin(e)/)
        ma(3, 1:3) = (/0d0, -sin(e), cos(e)/)
        mb(1, 1) = l
        mb(2, 1) = m
        mb(3, 1) = n

        me = matmul(ma, mb)
        u = me(1, 1)
        v = me(2, 1)
        w = me(3, 1)

        ss = v
        cc = u
        tt = Quadrant(ss, cc)
        era = tt
        edc = asin(sin(w))

        star_position(1) = era
        star_position(2) = edc
    end function EquatorialToEcliptic

    function AnnualParallax(t0, t1, ra, dc, ap) result(star_position)
    ! 視差による恒星位置のずれ
    ! 「天体の位置計算 増補版」66-72頁

        double precision, intent(in) :: t0, t1, ra, dc, ap
        double precision :: star_position(3)

        double precision :: l, m, n, l4, m4, n4
        double precision :: ls, ms, ns
        double precision :: ecl, sun_ecl
        double precision :: sun_ecliptic(2), ma(3, 1), mb(3, 3), mc(3, 1), me(3, 1)

        ! 平均黄道傾斜角
        ecl = deg2rad(iauMOE50(t1))

        ! 太陽の黄経の方向余弦
        sun_ecliptic =  SunEclipticLongitude(t0)
        sun_ecl = deg2rad(sun_ecliptic(1))

        ls = cos(sun_ecl)
        ms = sin(sun_ecl) * cos(ecl)
        ns = sin(sun_ecl) * sin(ecl)

        ! 赤経・赤緯から方向余弦を計算する
        l = cos(dc) * cos(ra)
        m = cos(dc) * sin(ra)
        n = sin(dc)
    
        ! 年周視差を考慮した恒星位置の計算
        ma(1, 1) = l
        ma(2, 1) = m
        ma(3, 1) = n

        mb(1, 1:3) = (/1d0-l**2, -l*m,     -l*n/)
        mb(2, 1:3) = (/-m*l,     1d0-m**2, -m*n/)
        mb(3, 1:3) = (/-n*l,     -n*m,     1d0-n**2/)

        mc(1, 1) = ls
        mc(2, 1) = ms
        mc(3, 1) = ns

        me = ma + ap * (matmul(mb, mc))
        l4 = me(1, 1)
        m4 = me(2, 1)
        n4 = me(3, 1)

        star_position(1) = l4
        star_position(2) = m4
        star_position(3) = n4
    end function AnnualParallax

    function AnnualAberration(t0, t1, ra, dc) result(star_position)
    ! 光行差による恒星位置のずれ
    ! 「天体の位置計算 増補版」 74-82頁

        double precision, parameter :: KP = 20.49552d0

        double precision, intent(in) :: t0, t1, ra, dc

        double precision :: star_position(3)
        double precision :: l, m, n, l5, m5, n5
        double precision :: ls, ms, ns
        double precision :: ecl, sun_ecl
        double precision :: k
        double precision :: sun_ecliptic(2), ma(3, 1), mb(3, 3), mc(3, 1), me(3, 1)


        ! 平均黄道傾斜角
        ecl = deg2rad(iauMOE50(t1))

        ! 太陽の黄経の方向余弦
        sun_ecliptic = SunEclipticLongitude(t0)
        sun_ecl = deg2rad(sun_ecliptic(1))

        ls = sin(sun_ecl)
        ms = -cos(sun_ecl) * cos(ecl)
        ns = -cos(sun_ecl) * sin(ecl)

        ! 赤経・赤緯から方向余弦を計算する
        l = cos(dc) * cos(ra)
        m = cos(dc) * sin(ra)
        n = sin(dc)

        ! 光行差定数をラジアン値に換算
        k = deg2rad(KP / 3600.0d0)

        ! 年周視差を考慮した恒星位置の計算
        ma(1, 1) = l
        ma(2, 1) = m
        ma(3, 1) = n

        mb(1, 1:3) = (/1d0-l**2, -l*m,     -l*n/)
        mb(2, 1:3) = (/-m*l,     1d0-m**2, -m*n/)
        mb(3, 1:3) = (/-n*l,     -n*m,     1d0-n**2/)

        mc(1, 1) = ls
        mc(2, 1) = ms
        mc(3, 1) = ns

        me = ma + k * (matmul(mb, mc))
        l5 = me(1, 1)
        m5 = me(2, 1)
        n5 = me(3, 1)

        star_position(1) = l5
        star_position(2) = m5
        star_position(3) = n5
    end function AnnualAberration

    function Atmosphericlight(lsdt, la, ra, dc, hpa, tmp) result(hc)
        double precision, parameter :: K = 1.55141d-6

        double precision, intent(in) :: lsdt, la, ra, dc, hpa, tmp

        double precision :: t, tt, ss, cc, z, cp, cr, r, hc(2)
        double precision :: x1, x2, x3, l, m, n
        double precision :: ma(3, 1), mb(3, 1), mc(3, 1), md(3, 3), me(3, 1)

        ! 日周光行差の計算
        t = lsdt - ra

        x1 = cos(dc) * cos(t)
        x2 = -cos(dc) * sin(t)
        x3 = sin(dc)

        ma(1, 1) = x1
        ma(2, 1) = x2
        ma(3, 1) = x3

        mb(1, 1) = cos(t) * sin(t) * cos(dc)**2
        mb(2, 1) = cos(t)**2 + sin(t)**2 * sin(dc)**2
        mb(3, 1) = sin(t) * cos(dc) * sin(dc)

        mc = ma + K * cos(la) * mb

        md(1, 1:3) = (/sin(la), 0d0, -cos(la)/)
        md(2, 1:3) = (/0d0, 1d0, 0d0/)
        md(3, 1:3) = (/cos(la), 0d0, sin(la)/)

        me = matmul(md, mc)
        l = me(1, 1)
        m = me(2, 1)
        n = me(3, 1)

        ss = m
        cc = l
        tt = Quadrant(-ss, cc)
        hc(1) = tt
        hc(2) = asin(n)

        ! 大気差補正
        z = 90d0 - rad2deg(asin(n))
        z = tan(deg2rad(z))
        cr = 273.15d0 / (273.15d0 + tmp)
        cp = hpa / 1013.25d0
        r = (60.0615d0 * z - 0.0884d0 * z**3) * cr * cp
        r = deg2rad(r / 3600d0)
        hc(1) = tt
        hc(2) = hc(2) + r
    end function Atmosphericlight

    ! ここに新しい関数（サブルーチン）を記述する
    ! end mosule pcclibより後ろに記述しないこと！

end module pcclib
