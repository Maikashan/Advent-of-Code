module m
    use iso_fortran_env
    use vectChar
    implicit none
    character(len=:), allocatable :: input
    integer, parameter            :: height = 1000
contains 

    subroutine getNum(str, int_res, i)
        character(:), allocatable, intent(in)      :: str
        integer(kind=LargeInt_Vect), intent(inout) :: i
        integer(kind=LargeInt_Vect), intent(out)   :: int_res
        integer                                    :: nb
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
    character(:), allocatable, intent(in)      :: str
    integer(kind=LargeInt_Vect),intent(inout)  :: i

    do while(i <= len(str))
        if (str(i:i) >= '0' .and. str(i:i) <= '9') exit
        i = i + 1
    enddo
endsubroutine

subroutine gtSprg(spring, s, j, i)
    type(vectCh),dimension(height),intent(inout)  :: spring
    integer(kind=LargeInt_Vect), intent(inout)    :: i, j
    integer(kind=LargeInt_Vect)                   :: siz
    character(len=:),allocatable, intent(in)      :: s
    siz = 25
    call inVcCh(spring(j), siz)
    do while (s(i:i) /= ' ')
        call adVcCh(spring(j),s(i:i))
        i = i + 1
    enddo
endsubroutine

subroutine gtMap(map, s, j, i)
    type(vector),dimension(height),intent(inout)  :: map
    integer(kind=LargeInt_Vect), intent(inout)    :: i, j
    integer(kind=LargeInt_Vect)                   :: siz, num
    character(len=:),allocatable, intent(in)      :: s
    siz = 25
    call initVc(map(j), siz)
    do while (i <= len(s))
        call getNum(s, num, i)
        call addVec(map(j), num)
        call nextNb(s,i)
    enddo
endsubroutine

subroutine parse(spring, map)
    integer, parameter :: bufflen=1024
    type(vectCh),dimension(height),intent(inout)  :: spring
    type(vector),dimension(height),intent(inout)  :: map 
    integer                                       :: io,  isize, unit
    integer(kind=LargeInt_Vect)                   :: i, j
    character(len=:),allocatable                  :: s
    character(len=bufflen)                        :: buffer
    logical                                       :: eof

    print *,'input file = ', input

    unit = 1
    open(UNIT=unit, file=input, status='old')

    eof = .false.
    j = 0
    do while(.not.eof)
        s = ''
        read(unit,'(a)',advance='no', iostat=io, size=isize) buffer
        if(isize.gt.0)then 
            s = s//buffer(:isize)
        endif
        eof = iostat_end == io
        if (eof) exit
        i = 1
        j = j + 1
        call gtSprg(spring, s, j, i)
        i = i + 1
        call gtMap(map, s, j, i)
    enddo
    close(unit)
endsubroutine

function valid(spring, map) result(res)
    type(vectCh),intent(inout)                             :: spring
    type(vector),intent(inout)                             :: map
    logical                                                :: res
    integer(kind=LargeInt_Vect)                            :: i, j, curr
    character                                              :: tmp
    res = .false.
    j = 1
    curr = 0
    do i = 1, spring%curSiz
        tmp = atVcCh(spring,i)
        if (atVcCh(spring, i) == '#') then
            curr = curr + 1
        else
            if (curr /= 0) then
                if (map%curSiz < j) exit
                if (atVect(map, j) /= curr) exit
                j = j + 1
            endif
            curr = 0
        endif
    enddo
    if (i > spring%curSiz) then
        if (j>map%curSiz.and.curr == 0) then
            res = .true.
        else if (j == map%curSiz.and.atVect(map,j) == curr) then
            res = .true.
        endif
    endif

endfunction

RECURSIVE function comput(spring, map, begin) result(res)
    integer(kind=LargeInt_Vect), intent(in)                :: begin
    type(vectCh),intent(inout)                             :: spring
    type(vectCh)                                           :: s
    type(vector),intent(inout)                             :: map
    integer(kind=LargeInt_Vect)                            :: i, j, res
    s = spring
    do i = begin, spring%curSiz
        if (atVcCh(spring, i) == '?') then
            s%vect(i:i) = '.'
            res = comput(s, map, i + 1)
            s%vect(i:i) = '#'
            res = res + comput(s, map, i + 1)
            exit
        endif
    enddo
    if (i > spring%curSiz) then
        if (valid(spring, map)) then
            res = 1
        else
            res = 0
        endif
    endif
endfunction

subroutine part1()
    type(vectCh),dimension(height)                :: spring
    type(vector),dimension(height)                :: map
    integer(kind=LargeInt_Vect),dimension(height) :: resArr
    integer(kind=LargeInt_Vect)                   :: res, h, tmp
    
    call parse(spring, map)

    tmp = 1
    do h = 1, height
        resArr(h) = comput(spring(h), map(h), tmp)
    enddo

    res = SUM(resArr)

    print*, 'res = ', res

endsubroutine

subroutine part2()
endsubroutine

endmodule

program main
    use m

    print *,''
    print *,'Begin AOC main Day 11'
    print *,''

    input='input.txt'
    print*, 'Part 1'
    call part1()
    print*, 'Part 2'
    call part2()

    print *,''
    print *,'Ending AOC main Day 11'
    print *,''
endprogram