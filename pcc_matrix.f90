program matrix
! 赤道座標から地平座標の方向余弦

    implicit none

    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: RAD = 180.0d0 / PI
    double precision, parameter :: LA =   35.78888889d0 / RAD
    double precision, parameter :: SDT = 223.03270833d0 / RAD
    double precision, parameter :: RA = 316.1663958d0 / RAD
    double precision, parameter :: DC = 38.49975d0 / RAD

    double precision :: a(3, 3), b(3, 3), c(3, 1), d(3,1)
    double precision :: L, M, N

    L = cos(DC) * cos(RA)
    M = cos(DC) * sin(RA)
    N = sin(DC)

    a(1, 1:3) = (/sin(LA),     0.0d0, -cos(LA)/)
    a(2, 1:3) = (/ 0.0d0,      1.0d0,    0.0d0/)
    a(3, 1:3) = (/cos(LA),     0.0d0,  sin(LA)/)

    b(1, 1:3) = (/ cos(SDT), sin(SDT), 0.0d0/)
    b(2, 1:3) = (/-sin(SDT), cos(SDT), 0.0d0/)
    b(3, 1:3) = (/ 0.0d0,      0.0d0,  1.0d0/)

    c(1, 1) = L
    c(2, 1) = M
    c(3, 1) = N

    ! 一度に計算するにはmatmulの中にmatmulを記述する
    d = matmul(matmul(a, b), c)

    write(*, *)    
    write(*, '("l = ", f15.12)')d(1, 1)
    write(*, '("m = ", f15.12)')d(2, 1)
    write(*, '("n = ", f15.12)')d(3, 1)
    write(*, *)

    stop
end program matrix
