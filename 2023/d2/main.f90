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

subroutine getCol(str, color, i)
    character(:), allocatable, intent(in) :: str
    integer, intent(inout)                :: i
    character(5), intent(out)             :: color 
    character                             :: c

    c = ''
    do i = i, len(str)
        c = str(i:i)
        if (c /= ',' .and. c /= ';') then
            color = trim(color)//c
        else
            exit
        endif
    enddo
endsubroutine

function checkC(color, currNb) result(over)
    character(5), intent(in)              :: color
    integer, intent(in)                   :: currNb
    logical                               :: over

    over = .true.
    if (color == 'red' .and. currNb > 12) then
        over = .false.
    else if (color == 'blue' .and. currNb > 14) then
        over = .false.
    else if (color == 'green' .and. currNb > 13) then
        over = .false.
    endif
endfunction

subroutine part1()
    integer, parameter :: bufflen=1024
    integer :: io,  isize, res, currId, i, currnb
    character(len=:),allocatable  :: s
    character(5)                  :: color
    character(len=bufflen) :: buffer
    character              :: c
    logical eof
    print *,'input file = ', input

    open(UNIT=1, file=input, status='old')
    s = ''
    color = ''
    eof=.false.
    res = 0
readl:    do while(.not. eof)

        !Lexing part
        s=''
        read(1,'(a)',advance='no', iostat=io, size=isize) buffer
        if(isize.gt.0)then 
            s=s//buffer(:isize)
        endif
        eof = io == iostat_end
        if (eof) exit
        !print*, 's = ', s 

        !Parsing part
        !Get current Id
        currId = 0
        i = 6
        call getNum(s, currId, i)

        do while (i < len(s))
            i = i+2

        !Get amount of cubes
            currnb = 0
            call getNum(s, currnb, i)
            !print*, 'nb cube  : ', currnb

        !Get color of the cube
            i = i+1
            color = ''
            call getCol(s, color, i)
            !print*, ' for color : ', color
            if (.not. checkC(color, currnb)) then
                cycle readl
            endif
        enddo
        res = res + currId
    end do readl
    print*, 'res = ', res
    close(1)
endsubroutine


subroutine part2()
    integer, parameter :: bufflen=1024
    integer :: io,  isize, maxR, maxG, maxB, res, i, currnb
    character(len=:),allocatable  :: s
    character(5)                  :: color
    character(len=bufflen) :: buffer
    character              :: c
    logical eof
    print *,'input file = ', input

    open(UNIT=1, file=input, status='old')
    s = ''
    color = ''
    eof=.false.
    res = 0
readl:    do while(.not. eof)

        !Lexing part
        s=''
        read(1,'(a)',advance='no', iostat=io, size=isize) buffer
        if(isize.gt.0)then 
            s=s//buffer(:isize)
        endif
        eof = io == iostat_end
        if (eof) exit
        !print*, 's = ', s 

        !Parsing part
        currnb = 0
        i = 6
        call getNum(s,currnb,i) 
        maxR = 0
        maxG = 0
        maxB = 0

        do while (i < len(s))
            i = i+2

        !Get amount of cubes
            currnb = 0
            call getNum(s, currnb, i)

        !Get color of the cube
            i = i+1
            color = ''
            call getCol(s, color, i)
            if (color == 'green' .and. currnb > maxG) then 
                maxG = currnb
            else if (color == 'blue' .and. currnb > maxB) then
                maxB = currnb
            else if (color == 'red' .and. currnb > maxR) then
                maxR = currnb
            endif
        enddo
        res = res + maxG * maxB * maxR
    end do readl
    print*, 'res = ', res
    close(1)
endsubroutine

endmodule

program main
    use m
    
    print *,''
    print *,'Begin AOC main Day 1'
    print *,''

    input='input.txt'
    !call part1()
    call part2()

    print *,''
    print *,'Ending AOC main Day 1'
    print *,''
endprogram
