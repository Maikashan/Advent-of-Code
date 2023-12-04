module m
    use iso_fortran_env
    implicit none
    character(len=:), allocatable :: input

contains 


subroutine getNum(str, int_res, i)
    character(:), allocatable, intent(in) :: str
    integer, intent(inout)                :: i
    integer, intent(out)                  :: int_res
    integer                               :: nb
    int_res = 0
    do i = i, len(str)
        if (str(i:i) >=  '0' .and. str(i:i) <= '9') then
            read(str(i:i),*) nb
            int_res = int_res * 10 + nb
        else
            exit
        endif
    enddo
endsubroutine

subroutine nextNb(str, i)
    character(:), allocatable, intent(in) :: str
    integer, intent(inout)                :: i

    do while(i <= len(str))
        if ((str(i:i) >= '0' .and. str(i:i) <= '9') .or. str(i:i) == '|') exit
        i = i + 1
    enddo
endsubroutine

subroutine part1()
        integer, parameter :: bufflen=1024
        integer :: io,  isize, res, i, nb, sizWin, sum
        character(len=:),allocatable  :: s
        character(len=bufflen) :: buffer
        integer, dimension(10) :: winNb
        logical                            ::  eof

        print *,'input file = ', input

        open(UNIT=1, file=input, status='old')

        eof=.false.
        do while(.not. eof)
            !Lexing part
            s = ''
            read(1,'(a)',advance='no', iostat=io, size=isize) buffer
            if(isize.gt.0)then 
                s = s//buffer(:isize)
            endif
            eof = io == iostat_end
            if (eof) exit

            !Parsing part
            i = 1
            nb = 0
            sizWin = 0
            winNb(:) = -1
            call nextNb(s,i)
            call getNum(s, nb, i)
            call nextNb(s,i)
            !Get all the winning numbers
            do while (s(i:i) /= '|')
                call getNum(s, nb, i)
                sizWin = sizWin + 1
                winNb(sizWin) = nb
                call nextNb(s,i)
            enddo

            sum = 0
            i = i + 1
            call nextNb(s,i)
            do while (i <= len(s))
                call getNum(s, nb, i)
                if ( ANY(winNb==nb)) then
                    if (sum == 0) then 
                        sum = 1
                    else
                        sum = ISHFT(sum,1)
                    endif
                endif
                call nextNb(s,i)
            enddo
            res = res + sum
        enddo
        print*, 'res = ', res
        close(1)
endsubroutine

subroutine part2()
endsubroutine

endmodule

program main
    use m

    print *,''
    print *,'Begin AOC main Day 1'
    print *,''

    input='input.txt'
    call part1()
    call part2()

    print *,''
    print *,'Ending AOC main Day 1'
    print *,''
endprogram
