module m
    use iso_fortran_env
    use vect
    implicit none
    character(len=:), allocatable :: input
    integer, parameter            :: width = 140
    integer, parameter            :: height = 140
contains 

subroutine stVoid(el, void)
        type(vector), intent(inout)              :: el
        logical,dimension(:),intent(inout)       :: void
        integer(kind=LargeInt_Vect)              :: i
        do i = 1, el%curSiz
            void(atVect(el, i)) = .false.
        enddo
endsubroutine

subroutine parse(u,elX,elY)
    integer, parameter :: bufflen=1024
    integer, intent(in)                           :: u
    type(vector), intent(inout)                   :: elX, elY
    integer                                       :: io,  isize
    integer(kind=LargeInt_Vect)                   :: i, j
    character(len=:),allocatable                  :: s
    character(len=bufflen)                        :: buffer
    logical                                       :: eof
    eof = .false.
    j = 0
    do while(.not.eof)
        s = ''
        read(u,'(a)',advance='no', iostat=io, size=isize) buffer
        if(isize.gt.0)then 
            s = s//buffer(:isize)
        endif
        eof = iostat_end == io
        if (eof) exit
        j = j + 1
        do i = 1, len(s)
            if (s(i:i) == '#') then
                call addVec(elX, i)
                call addVec(elY, j)
            endif
        enddo
    enddo
endsubroutine

function gtDist(el, void, x, y, fact) result(res)
    type(vector),intent(inout)                  :: el
    logical,dimension(:),intent(in)             :: void
    integer(kind=LargeInt_Vect),intent(in)      :: x, y, fact
    integer(kind=LargeInt_Vect)                 :: res, begin, end, tmp
    begin = atVect(el, x)
    end = atVect(el,y)
    if (begin > end) then
        tmp = begin
        begin = end
        end = tmp
    endif
    !print*,'begin = ', begin, ' end = ', end, 'count = ', count(MASK=void(begin:end))
    tmp = count(MASK=void(begin:end))
    res = count(MASK=.not.void(begin:end - 1)) + tmp * fact
    !print*,'val = ', res
endfunction

subroutine part(step)
    integer, intent(in)                      :: step
    integer                                     :: unit
    type(vector)                                :: elX, elY
    logical,dimension(width)                    :: voidX
    logical, dimension(height)                  :: voidY
    integer(kind=LargeInt_Vect)                 :: res, siz, i, j, val, tmp

    print *,'input file = ', input

    if (step == 2) then
        tmp = 1000000
    else 
        tmp = 2
    endif

    unit = 1
    open(UNIT=unit, file=input, status='old')

    siz = 25
    call initVc(elX, siz)
    call initVc(elY, siz)

    !Parsing the file
    call parse(unit,elX, elY)

    !Set the void
    voidX = .true.
    voidY = .true.
    call stVoid(elX, voidX)
    call stVoid(elY, voidY)

    !Computing
    res = 0
    do i = 1, elX%curSiz
        do j = i + 1, elX%curSiz
            !print*, 'i = ', i, ' j= ', j
            val = gtDist(elX, voidX, i, j, tmp) + gtDist(elY, voidY, i, j,tmp)
            !print*, 'tot val = ', val
            res = res + val
        enddo
    enddo

    print*, 'res = ', res
    close(unit)
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
    call part(1)
    print*, 'Part 2'
    call part(2)

    print *,''
    print *,'Ending AOC main Day 11'
    print *,''
endprogram
