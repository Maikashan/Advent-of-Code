module m
    use iso_fortran_env
    use vectChar
    implicit none
    character(len=:), allocatable :: input
    integer, parameter            :: height = 32
    character(len=*),parameter :: free='(*(g0,1x))'
contains

    subroutine parseL(u, eof, volcs, h)
        integer, parameter :: bufflen=1024
        integer, intent(in)                           :: u
        type(vectCh), dimension(:), intent(inout)     :: volcs
        integer(kind=LargeInt_VectChar),intent(out)   :: h
        logical, intent(inout)                        :: eof
        integer                                       :: io,  isize
        integer(kind=LargeInt_VectChar)               :: i, siz
        character(len=:),allocatable                  :: s
        character(len=bufflen)                        :: buffer
        siz = 25
        h = 0
        isize = 1
        do while (.not.eof.and.isize/=0)
            s = ''
            read(u,'(a)',advance='no', iostat=io, size=isize) buffer
            if(isize.gt.0)then 
                s = s//buffer(:isize)
            endif
            eof = iostat_end == io
            if (eof.or.isize==0) exit
            h = h + 1
            ! parsing
            call inVcCh(volcs(h), siz)
            do i = 1, len(s)
                call adVcCh(volcs(h), s(i:i))
            enddo
        enddo
    endsubroutine

    function vert(volcs, h, p) result(val)
        type(vectCh),dimension(:),intent(inout)       :: volcs
        integer(kind=LargeInt_VectChar),intent(in)    :: h, p
        integer(kind=LargeInt_VectChar)               :: val
        integer(kind=LargeInt_VectChar)               :: i, j, k, test, l
        val = 0
        do l = 1, volcs(1)%curSiz - 1
            i = l
            j = volcs(1)%curSiz
            test = 0
            do while (i <= j)
                do k = 1, h
                    if (j == i) then
                        test = 2
                        exit 
                    endif
                    if (atVcCh(volcs(k),i) /= atVcCh(volcs(k),j).or.i==j) then
                        test =  test + 1
                    endif
                enddo
                i = i + 1
                j = j - 1
                if (test > p - 1) exit
            enddo
            if (test == p - 1) then
                exit
            endif
        enddo
        if (test == p - 1) then 
            val = j
        else 
        do l = volcs(1)%curSiz, 2, -1
            i = 1
            j = l
            test = 0
            do while (i <= j)
                do k = 1, h
                    if (j == i) then
                        test = 2
                        exit 
                    endif
                    if (atVcCh(volcs(k),i) /= atVcCh(volcs(k),j).or.i==j) then
                        test =  test + 1
                    endif
                enddo
                i = i + 1
                j = j - 1
                if (test > p - 1) exit
            enddo
            if (test == p - 1) then
                exit
            endif
        enddo
        if (test == p - 1) then 
            val = j
        endif
    endif
    endfunction


    function horiz(volcs, h, p) result(val)
        type(vectCh),dimension(:),intent(inout)       :: volcs
        integer(kind=LargeInt_VectChar),intent(in)    :: h, p
        integer(kind=LargeInt_VectChar)               :: val
        integer(kind=LargeInt_VectChar)               :: i, j, k, test, l
        val = 0
        do l = 1, h - 1
            i = l
            j = h
            test = 0
            do while (i <= j)
                do k = 1, volcs(1)%curSiz
                    if (j == i) then
                        test = 2
                        exit 
                    endif
                    if (atVcCh(volcs(i),k) /= atVcCh(volcs(j),k)) then
                        test =  test + 1
                    endif
                enddo
                i = i + 1
                j = j - 1
                if (test > p - 1) exit
            enddo
            if (test == p - 1) then
                exit
            endif
        enddo
        if (test == p - 1) then
            val = j
        else
            do l = h, 2, -1
                i = 1
                j = l
                test = 0
                do while (i <= j)
                    do k = 1, volcs(1)%curSiz
                        if (j == i) then
                            test = 2
                            exit 
                        endif
                        if (atVcCh(volcs(i),k) /= atVcCh(volcs(j),k)) then
                            test =  test + 1
                        endif
                    enddo
                    i = i + 1
                    j = j - 1
                    if (test > p - 1) exit
                enddo
                if (test == p - 1) then
                    exit
                endif
            enddo
            if (test == p - 1) then
            val = j
            endif
        endif
    endfunction

    function comput(volcs, h, p) result(res)
        type(vectCh), dimension(:),intent(inout)      :: volcs
        integer(kind=LargeInt_VectChar),intent(in)    :: h, p
        integer(kind=LargeInt_VectChar)               :: res
        res = vert(volcs,h, p)
        if (res == 0) then
            res = horiz(volcs, h, p) * 100
        endif
    endfunction

    subroutine part(p)
        integer(kind=LargeInt_VectChar),intent(in)    :: p
        integer                                       :: u
        logical                                       :: eof
        type(vectCh), dimension(height)               :: volcs
        integer(kind=LargeInt_VectChar)               :: res, h, i,j
        res = 0
        u = 1
        open(UNIT=u, file=input, status='old')
        eof = .false.
        j = 0
        do while(.not.eof)
            j = j + 1
            call parseL(u, eof, volcs, h)
            res = res + comput(volcs,h, p)
            do i = 1, h
                call rmVcCh(volcs(i))
            enddo
        enddo
        close(u)
        print*, res
    endsubroutine

endmodule

program main
    use m
    integer(kind=LargeInt_VectChar)               :: p

    print *,''
    print *,'Begin AOC main Day 13'
    print *,''

    input='input.txt'
    print*, 'Part 1'
    p = 1
    call part(p)
    print*, 'Part 2'
    p = 2
    call part(p)

    print *,''
    print *,'Ending AOC main Day 13'
    print *,''
endprogram
