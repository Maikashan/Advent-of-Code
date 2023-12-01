module m
    use iso_fortran_env
    implicit none
    character(len=:), allocatable :: input

contains 

subroutine part1()
    integer :: first, last, io, j, sum
    character  :: s * 256
    logical eof
    print *,'input file = ', input

    open(UNIT=1, file=input, status='old')

    eof=.false.
    do while(.not. eof)
        first = -1
        last = -1
        read(1,'(a)', iostat=io) s
        eof = io == iostat_end
        if (eof) exit
        !print *, 's = ',s
        do j = 1, len(s)
            if (s(j:j) >=  '0' .and. s(j:j) <= '9') then
                if (first == -1)  then
                    read(s(j:j),*) first
                endif
                read(s(j:j),*) last
            endif
        enddo
        sum = sum + first * 10 + last
    enddo
    print*, 'res = ',sum
endsubroutine

endmodule

program main
    use m
    
    print *,''
    print *,'Begin AOC main Day 1'
    print *,''

    input='input.txt'
    call part1()

    print *,''
    print *,'Ending AOC main Day 1'
    print *,''
endprogram