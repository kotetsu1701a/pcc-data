program jdate
    use pcclib

    implicit none

    double precision :: julian
    double precision :: dy, dt
    double precision :: year, month, day, day_temp
    double precision :: hour, minute, second
    double precision :: dtime(3)
    character(128) :: dfmt

    write(*,*)
    write(*, fmt='(a)', advance='no') 'DATE AND TIME(JST) ? '
    read(*,*) dy, dt

    year = int(dy / 10000.0d0)
    month = mod(int(dy / 100.0d0), 100)
    day = mod(dy, 100.0d0)
    hour = int(dt / 10000.0d0)
    minute = mod(int(dt / 100.0d0), 100)
    second = mod(dt, 100.0d0)

    ! 入力した年月日を表示
    write(*, *)
    dfmt = '(" INPUT DATE = ", i4, "年 ", i2, "月 ", i2, "日 ", i2, "時 ", i2, "分 ", i2, "秒")'
    write(*, dfmt) int(year), int(month), int(day), int(hour), int(minute), int(second)

    ! ユリウス日の計算
    julian = JulianDay(year, month, day)
    julian = julian + (hour / 24d0 + minute / 1440d0 + second / 86400d0)
    julian = julian - 0.375d0

    ! ユリウス日から年月日を求める
    dtime = Julian2date_2(julian)
    year = dtime(1)
    month = dtime(2)
    day = dtime(3)

    ! 日の小数点から時刻を求める
    day_temp = 24.0d0 * (day - int(day))
    hour = int(day_temp)
    second = 60d0 * (day_temp - hour)
    minute = int(second)
    second = int(60d0 * (second - minute))

    ! 出力
    write(*, *)
    dfmt = '("RETURN DATE = ", i4, "年 ", i2, "月 ", i2, "日 ", i2, "時 ", i2, "分 ", i2, "秒")'
    write(*, dfmt) int(year), int(month), int(day), int(hour), int(minute), int(second)
    write(*, *)

    stop
end program jdate