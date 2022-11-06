program pcc_lsdt_2
    use pcclib

    implicit none

    ! 地点
    character(21), parameter :: OBS_NAME = 'Tokyo Observatory PZT'          ! 地点名
    double precision, parameter :: OBS_LG(1:3) = (/9d0, 18d0, 9.936d0/)     ! 経度
    double precision, parameter :: OBS_LA(1:3) = (/35d0, 40d0, 20.707d0/)   ! 緯度
    double precision, parameter :: GAST(1:3) = (/17d0, 51d0, 24.267d0/)
    
    ! 検証用地点
    ! character(21), parameter    :: OBS_NAME = "Tokyo Ngasawa"               ! 地点名
    ! double precision, parameter :: OBS_LG(1:3) = (/9d0, 18d0, 7.573d0/)     ! 経度
    ! double precision, parameter :: OBS_LA(1:3) = (/35d0, 47d0, 20.0d0/)     ! 緯度
    ! double precision, parameter :: GAST(1:3) = (/17d0, 11d0, 58.714d0/)

    double precision :: dy, dt, julian
    double precision :: year, month, day, hour, minute, second
    double precision :: lsdt, lg
    double precision :: sdt_hour, sdt_minute, sdt_second
    double precision :: sdt(3), dtime(3)
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

    julian = JulianDay(year, month, day)
    julian = julian + (hour / 24d0 + minute / 1440d0 + second / 86400d0)
    julian = julian - 0.375

    dtime = Julian2date(julian)
    year = dtime(1)
    month = dtime(2)
    day = dtime(3)
    
    ! 地方恒星時の計算
    lg = hms2deg(OBS_LG(1), OBS_LG(2), OBS_LG(3))
    lsdt = rad2deg(localSDT(julian)) + lg
    sdt = deg2hms(lsdt)

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
    dfmt = '(" ", i4, " ", i2, " ", f8.5, "       ", f14.5, "       ", i2, "  ", i2, "  ", f6.3)'
    write(*, *)
    write(*, *) 'Date/                    Julian Date       Local Sidereal Time'
    write(*, *) '--------------------------------------------------------------'
    write(*, *) '                                              h   m   s'
    write(*, dfmt) int(year), int(month), day, julian, int(sdt_hour), int(sdt_minute), sdt_second
    write(*, *)

    stop
end program pcc_lsdt_2
