module m
    use iso_fortran_env
    use container
    implicit none
    character(len=:), allocatable :: input
    character(len=*),parameter    :: free='(*(g0,1x))'
contains

    subroutine getSub(s, i, sub)
        character(len=:),allocatable,intent(in)       :: s
        integer(kind=LargeInt),intent(inout)          :: i
        character(len=:),allocatable,intent(inout)    :: sub
        sub = ''
        do while (i <= len(s))
            if (s(i:i) == ',') exit
            sub = sub //s(i:i)
            i = i + 1
        enddo
        i = i + 1
    endsubroutine

    function hash(sub) result(res)
        character(len=:), allocatable, intent(in)     :: sub
        integer(kind=LargeInt)                        :: res, i
        res = 0
        do i = 1, len(sub)
            res = res + IACHAR(sub(i:i))
            res = res * 17
            res = mod(res,256)
        enddo
    endfunction


    subroutine part1()
        integer, parameter :: bufflen=32768
        integer                                       :: u
        logical                                       :: eof
        integer                                       :: io,  isize
        integer(kind=LargeInt)                        :: i, res
        character(len=:),allocatable                  :: s, sub
        character(len=bufflen)                        :: buffer
        u = 1
        open(UNIT=u, file=input, status='old')
        eof = .false.
        s = ''
        read(u,'(a)',advance='no', iostat=io, size=isize) buffer
        if(isize.gt.0)then 
            s = s//buffer(:isize)
        endif
        eof = iostat_end == io
        i = 1
        do while (i <=  len(s))
            call getSub(s,i,sub)
            res = res + hash(sub)
        enddo
        close(u)
        print*, res
    endsubroutine

    subroutine getLbl(s,i,sub)
        character(len=:),allocatable,intent(in)       :: s
        integer(kind=LargeInt),intent(inout)          :: i
        character(len=:),allocatable,intent(inout)    :: sub
        sub = ''
        do while (i <= len(s))
            if (s(i:i) == '='.or.s(i:i) == '-') exit
            sub = sub //s(i:i)
            i = i + 1
        enddo
    endsubroutine

    subroutine getNum(s,i,num)
        character(len=:),allocatable,intent(in)       :: s
        integer(kind=LargeInt),intent(inout)          :: i
        integer(kind=LargeInt),intent(out)            :: num
        integer(kind=LargeInt)                        :: tmp
        num = 0
        do while (i <= len(s))
            if (s(i:i) == ',') exit
            read(s(i:i),*)tmp
            num = num * 10 + tmp
            i = i + 1
        enddo
    endsubroutine

    subroutine part2()
        integer, parameter :: bufflen=32768
        integer                                       :: u
        logical                                       :: eof
        integer                                       :: io,  isize
        integer(kind=LargeInt)                        :: i, res, num, pos, j
        character(len=:),allocatable                  :: s, sub
        character(len=bufflen)                        :: buffer
        character                                     :: sep
        type(contain),dimension(256)                  :: map
        type(keyVal)                                  :: el
        u = 1
        open(UNIT=u, file=input, status='old')
        eof = .false.
        s = ''
        res = 0
        j = 25
        do i = 1, 256
            call inCont(map(i),j)
        enddo
        read(u,'(a)',advance='no', iostat=io, size=isize) buffer
        if(isize.gt.0)then 
            s = s//buffer(:isize)
        endif
        eof = iostat_end == io
        i = 1
        do while (i <=  len(s))
            call getLbl(s,i,sub)
            el%key = sub
            sep = s(i:i)
            i = i + 1
            pos = hash(sub)
            if (sep == '-') then
                call popCnt(map(pos + 1),el)
                i = i + 1
            else
                call getNum(s, i, num)
                el%val = num
                i = i + 1
                if (.not.rplcCt(map(pos + 1), el)) then
                    call adCont(map(pos + 1),el)
                endif
            endif
        enddo
        do i = 1, 256
            do j = 1, map(i)%curSiz
                el = atCont(map(i),j)
                res = res + i * j * el%val
            enddo
            call rmVect(map(i))
        enddo
        close(u)
        print*, 'res = ', res
    endsubroutine

endmodule

program main
    use m

    print *,''
    print *,'Begin AOC main Day 15'
    print *,''

    input='input.txt'
    print*, 'Part 1'
    call part1()
    print*, 'Part 2'
    call part2()

    print *,''
    print *,'Ending AOC main Day 15'
    print *,''
endprogram
