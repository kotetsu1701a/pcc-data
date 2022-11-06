program pcc_degrad
    use pcclib

    implicit none

    ! 恒星データ
    character(16), parameter :: SNM = 'α CMa  Sirius  '              ! 恒星名
    double precision, parameter :: SRA(1:3) = (/6d0, 42d0, 56.714d0/)   ! 赤経
    double precision, parameter :: SDC(1:3) = (/-16d0, 38d0, 46.36d0/)  ! 赤緯

    !検証用恒星データ
    ! character(16), parameter :: SNM = '61 Cyg          ';           ! 恒星名
    ! double precision, parameter :: SRA(1:3) = (/21d0, 4d0, 39.935d0/) ! 赤経
    ! double precision, parameter :: SDC(1:3) = (/38d0, 29d0, 59.10d0/) ! 赤緯

    character(255) :: sfmt
    double precision :: ra_deg, dc_deg,ra_rad, dc_rad
    
    ! 時分秒もしくは度分秒を角度に
    ! 角度を時分秒もしくは度分秒に換算
    ra_deg = hms2deg(SRA(1), SRA(2), SRA(3))
    dc_deg = dms2deg(SDC(1), SDC(2), SDC(3))

    ! 角度をラジアンに換算
    ra_rad = deg2rad(ra_deg)
    dc_rad = deg2rad(dc_deg)

    ! 出力
    write(*, *)
        sfmt = trim('(a16, "     ", f10.6, "    ", f10.6, "     ", f11.8, "    ", f11.8)')
        write(*, *) '                             角 度                       ラジアン'
        write(*, *) '星名                   α             δ               α            δ'
        write(*, *) '---------------------------------------------------------------------------'
        write(*, *) '                      。            。     '

    write(*, sfmt) SNM, ra_deg, dc_deg, ra_rad, dc_rad
    write(*, *)

    stop
end program pcc_degrad
