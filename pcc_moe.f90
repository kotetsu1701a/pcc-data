program moe
    use pcclib

    implicit none

    double precision :: julian, t
    double precision :: dy, dt
    double precision :: year, month, day
    double precision :: hour, minute, second
    double precision :: ecl
    double precision :: me(3), dtime(3)
    character(255) :: dfmt

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

    ! ユリウス日の計算
    julian = JulianDay(year, month, day)
    julian = julian + (hour / 24d0 + minute / 1440d0 + second / 86400d0)
    julian = julian - 0.375

    ! ユリウス日から年月日を求める
    dtime = Julian2date(julian)
    year = dtime(1)
    month = dtime(2)
    day = dtime(3)

    ! 時刻引数の計算
    ! t = (julian - 2415020d0) / 36525d0
    t = (julian - 2451545d0) / 36525d0

    !平均黄道傾斜角の計算
    ! ecl = iauMOE50(t)
    ecl = iauMOE(t)

    me = deg2dms(ecl)

    ! 出力
    write(*, *)
    dfmt = '(" ", i4, " ", i2, " ", f8.5, "       ", f14.5, "               ", f10.7)'
    write(*, *)
    write(*, *) 'Date / UTC               Julian Date           Mean obliquity of ecliptic'
    write(*, *) '-------------------------------------------------------------------------'
    write(*, *) '                                                      。'
    write(*, dfmt) int(year), int(month), day, julian, ecl
    write(*, *)

    stop
end program moe
