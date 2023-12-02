module m
    use iso_fortran_env
    implicit none
    character(len=:), allocatable :: input

contains 

subroutine part1()
    integer :: first, last, io, j, sum
    character :: s * 70
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
    close(1)
endsubroutine

function intFromStr(str) result(int_res)
    character(:), allocatable, intent(in) :: str
    integer                  :: int_res
    if(.not. index(str, 'one') == 0) then
        int_res = 1
    else if(.not. index(str, 'two') == 0) then
        int_res = 2
    else if(.not. index(str, 'three') == 0) then
        int_res = 3
    else if(.not. index(str, 'four')==0) then
        int_res = 4
    else if(.not. index(str, 'five')==0) then
        int_res = 5
    else if(.not. index(str, 'six') == 0) then
        int_res = 6
    else if(.not. index(str, 'seven') == 0) then
        int_res = 7
    else if(.not. index(str, 'eight') == 0) then
        int_res = 8
    else if(.not. index(str, 'nine') == 0) then
        int_res = 9
    else
        int_res = -1
    endif
endfunction

subroutine part2()
    integer, parameter :: bufflen=1024
    integer :: first, last, io, j, sum, test, isize
    character(len=:),allocatable  :: s, beginStr, endStr
    character(len=bufflen) :: buffer
    character :: cf, cl
    logical eof
    print *,'input file = ', input

    open(UNIT=1, file=input, status='old')

    s = ''
    sum = 0
    eof=.false.

    do while(.not. eof)

        first = -1
        last = -1
        s=''

        read(1,'(a)',advance='no', iostat=io, size=isize) buffer

        if(isize.gt.0)then 
            s=s//buffer(:isize)
        endif

        allocate(character(len(s)) :: beginStr)

        beginStr=''
        endStr=''

        eof = io == iostat_end
        if (eof) exit

        j = 1
        do while(last == -1 .or. first == -1 .and. len(s) >= j)
            if (first == -1) then
                read(s(j:j),*)cf
                if (cf >=  '0' .and. cf <= '9') then
                    read(cf,*)first 
                else 
                    beginStr = beginStr//cf
                    first = intFromStr(beginStr)
                endif
            endif
            if (last == -1) then
                read(s(len(s) + 1 - j:len(s) + 1 -j),*)cl
                if (cl >=  '0' .and. cl <= '9') then
                    read(cl,*)last
                else
                    endStr = cl//endStr
                    last = intFromStr(endStr)
                endif
            endif
            j= j + 1
        enddo
        deallocate(beginStr)
        sum = sum + first * 10 + last
    enddo
    print*,'res = ',sum
    close(1)
endsubroutine

endmodule

program main
    use m
    
    print *,''
    print *,'Begin AOC main Day 1'
    print *,''

    input='test.txt'
    !call part1()
    call part2()

    print *,''
    print *,'Ending AOC main Day 1'
    print *,''
endprogram
