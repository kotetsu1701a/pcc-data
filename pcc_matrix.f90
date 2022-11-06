program matrix
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

    double precision :: ra_rad, dc_rad
    double precision :: l, m, n
    character(255) :: dfmt

    ra_rad = deg2rad(hms2deg(SRA(1), SRA(2), SRA(3)))
    dc_rad = deg2rad(dms2deg(SDC(1), SDC(2), SDC(3)))

    ! 方向余弦
    l = cos(dc_rad) * cos(ra_rad)
    m = cos(dc_rad) * sin(ra_rad)
    n = sin(dc_rad)

    ! 出力
    dfmt = '(a16, "      ", f11.8, "     ",  f11.8, "     ", f11.8, "    ", f11.8)'
    write(*, *)
    write(*, *) '星名                      L               M               N            二乗和'
    write(*, *) '-------------------------------------------------------------------------------'
    write(*, dfmt) SNM, l, m, n, l**2 + m**2 + n**2
    write(*, *)

    stop
end program matrix
