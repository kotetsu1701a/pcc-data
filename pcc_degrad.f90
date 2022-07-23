! 赤経・赤緯を角度およびラジアンに換算する
program pcc_degrad
    implicit none

    double precision :: second
    double precision :: degree, radian
    character(255) :: message = ""
    integer :: hour, minute, option
    logical :: error = .false.
    
    write(*,*)
    write(*, fmt='(a)', advance='no') 'HMS or DMS ? '
    read(*,*) hour, minute, second
    write(*, fmt='(a)', advance='no') 'Option(0:hms 1:dms) ? '
    read(*,*) option

    ! 入力チェック
    call ErrorCheck(hour, minute, second, option, error, message)

    ! 入力ミスがなければ処理続行
    if (.not.(error)) then
        select case(option)
        case(0)
            call hms2deg(hour, minute, second, degree)
        case(1)
            call dms2deg(hour, minute, second, degree)
        end select

        call deg2rad(degree, radian)

        write(*, *)
        write(*, '("Degree = ", f12.8)') degree
        write(*, '("Radian = ", f12.8)') radian
        write(*, *)
    else
        ! 入力ミスがあればメッセージを表示
        write(*,*)
        write(*, *) message  
    end if

    stop

end program pcc_degrad
