module m
    use vect
    use iso_fortran_env
    implicit none
    character(len=:), allocatable :: input

contains 

subroutine getNum(str, int_res, i)
    character(:), allocatable, intent(in)    :: str
    integer, intent(inout)                   :: i
    integer(kind=LargeInt_Vect), intent(out) :: int_res
    integer                                  :: nb, neg
    int_res = 0
    neg = 1
    do i = i, len(str)
        if (str(i:i) =='-') then
            neg = -1
        else if (str(i:i) >=  '0' .and. str(i:i) <= '9') then
            read(str(i:i),*) nb
            int_res = int_res * 10 + nb
        else
            exit
        endif
    enddo
    int_res = int_res * neg

endsubroutine

subroutine nextNb(str, i)
    character(:), allocatable, intent(in) :: str
    integer, intent(inout)                :: i

    do while(i <= len(str))
        if ((str(i:i) >= '0' .and. str(i:i) <= '9').or.str(i:i) == '-') exit
        i = i + 1
    enddo
endsubroutine

function vecNxt(curLst, prvLst, rmbNum, p) result(res)
    type(vector), intent(inout)       :: curLst, prvLst, rmbNum
    integer(kind=LargeInt_Vect)       :: nb, res, i, newVal, j
    logical                           :: zeros
    integer, intent(in)               :: p
    zeros = .false.
    do while(.not.zeros)
        call initVc(curLst, prvLst%curSiz - 1)
        zeros = .true.
        do i = 1, prvLst%curSiz - 1
            newVal = atVect(prvLst, i + 1) - atVect(prvLst, i) 
            if (newVal /= 0) zeros = .false.
            call addVec(curLst, newVal)
        enddo
        call rmVect(prvLst)
        prvLst = curLst
        if (p == 1) then
            j = curLst%curSiz
        else
            j = 1
        endif
        call addVec(rmbNum, atVect(curLst, j))
        call rmVect(curLst)
    enddo
    res = 0
    do i = rmbNum%curSiz, 1, -1
        if (p == 1) then
            res = res + atVect(rmbNum, i)
        else
            res = atVect(rmbNum, i) - res
        endif
    enddo

endfunction

subroutine parse(u, prvLst, eof)
    integer, parameter                :: bufflen=1024
    character(len=bufflen)            :: buffer
    character(len=:),allocatable      :: s
    integer                           :: io, isize, i, u
    logical,intent(inout)             :: eof
    integer(kind=LargeInt_Vect)       :: nb
    type(vector), intent(inout)       :: prvLst
    ! Parse a line
    s=''
    read(u,'(a)',advance='no', iostat=io, size=isize) buffer
    if (isize.gt.0)then
        s=s//buffer(:isize)
    endif
    eof = io == iostat_end
    if (.not.eof) then
        i = 1
        do while (i <= len(s))
            call getNum(s, nb, i)
            call addVec(prvLst, nb)
            call nextNB(s,i)
        enddo
    endif
endsubroutine

subroutine part(p)
    integer, intent(in)               :: p
    integer(kind=LargeInt_Vect)       :: nb, res, siz
    type(vector)                      :: curLst, prvLst, rmbNum
    logical                           :: eof

    open(UNIT=1, file=input, status='old')
    eof = .false.
    res = 0
    siz = 25
    do while (.not. eof)
        call initVc(prvLst, siz)
        call parse(1, prvLst,eof)
        if (eof) exit
        call initVc(rmbNum, siz)
        call addVec(rmbNum, atVect(prvLst, prvLst%curSiz))

        res = res + vecNxt(curLst, prvLst, rmbNum, p)
        call rmVect(prvLst)
        call rmVect(rmbNum)
    enddo

    close(1)
    print*,'res = ',res
endsubroutine

endmodule

program main
    use m

    print *,''
    print *,'Begin AOC main Day 9'
    print *,''

    input='example1.txt'
    print*, 'Part 1'
    call part(1)
    print*, 'Part 2'
    call part(2)

    print *,''
    print *,'Ending AOC main Day 9'
    print *,''
endprogram
