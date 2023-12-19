module m
    use iso_fortran_env
    use vectInt
    use vectGrid
    implicit none
    character(len=:), allocatable :: input
    character(len=*),parameter    :: free='(*(g0,1x))'
    integer,parameter             :: width=100
contains

    subroutine rollN(s, curTop, nbRoll, h)
        character(len=:),allocatable, intent(in)      :: s
        integer,dimension(:),intent(inout)            :: curTop
        type(vector), intent(inout)                   :: nbRoll
        integer(kind=LargeInt_Vect), intent(in)       :: h
        integer(kind=LargeInt_Vect)                   :: i
        do i = 1, len(s)
            if (s(i:i) == '#') then
                curTop(i) = h + 1
            else if (s(i:i) == 'O') then
                nbRoll%vect(curTop(i)) = nbRoll%vect(curTop(i)) + 1
                curTop(i) = curTop(i) + 1
            endif
        enddo
    endsubroutine

    function comput(nbRoll) result(res)
        type(vector), intent(inout)                   :: nbRoll
        integer(kind=LargeInt_Vect)                   :: res
        integer(kind=LargeInt_Vect)                   :: i
        res = 0
        do i = 1, nbRoll%curSiz
            res = res + atVect(nbRoll, nbRoll%curSiz - i + 1) * i
        enddo
    endfunction

    subroutine part1()
        integer, parameter :: bufflen=1024
        integer                                       :: u
        logical                                       :: eof
        integer                                       :: io,  isize
        integer(kind=LargeInt_Vect)                   :: i, tmp, res, h
        character(len=:),allocatable                  :: s
        character(len=bufflen)                        :: buffer
        type(vector)                                  :: nbRoll
        integer,dimension(width)                      :: curTop
        tmp = 25
        u = 1
        open(UNIT=u, file=input, status='old')
        eof = .false.
        call initVc(nbRoll, tmp)
        curTop = 1
        tmp = 0
        h = 1
        do while(.not.eof)
            s = ''
            read(u,'(a)',advance='no', iostat=io, size=isize) buffer
            if(isize.gt.0)then 
                s = s//buffer(:isize)
            endif
            eof = iostat_end == i
            if (eof.or.isize==0) exit
            call addVec(nbRoll,tmp)
            call rollN(s, curTop, nbRoll, h)
            h = h + 1
        enddo
        res = comput(nbRoll)
        close(u)
        print*, res
    endsubroutine

    subroutine parse(grid, siz)
        character,dimension(:,:),allocatable, intent(inout)       :: grid
        integer(kind=LargeInt_Vect), intent(out)      :: siz
        integer, parameter                            :: bufflen=1024
        logical                                       :: eof
        integer                                       :: io,  isize
        integer(kind=LargeInt_Vect)                   :: i
        character(len=:),allocatable                  :: s
        character(len=bufflen)                        :: buffer
        open(UNIT=1, file=input, status='old')
        eof = .false.
        siz = 1
        do while(.not.eof)
            s = ''
            read(1,'(a)',advance='no', iostat=io, size=isize) buffer
            if(isize.gt.0)then 
                s = s//buffer(:isize)
            endif
            eof = iostat_end == io
            if (eof.or.isize==0) exit
            do i = 1, len(s)
                grid(siz, i) = s(i:i)
            enddo
            siz = siz + 1
        enddo
        siz = siz - 1
        close(1)
    endsubroutine

    subroutine rlRowO(grid, r,siz)
        character,dimension(:,:),allocatable, intent(inout)       :: grid
        integer(kind=LargeInt_Vect), intent(in)       :: r, siz
        integer(kind=LargeInt_Vect)                   :: curTop, i
        curTop = 1
        do i = 1, siz
            if (grid(r, i) == '#') then
                curTop = i + 1
            else if (grid(r,i) == 'O') then
                grid(r,i) = '.'
                grid(r,curTop) = 'O'
                curTop = curTop + 1
            endif
        enddo
    endsubroutine 

    subroutine rlRowE(grid, r, siz)
        character,dimension(:,:), allocatable,intent(inout)       :: grid
        integer(kind=LargeInt_Vect), intent(in)       :: r, siz
        integer(kind=LargeInt_Vect)                   :: curTop, i
        curTop = siz
        do i =  siz, 1, -1
            if (grid(r, i) == '#') then
                curTop = i - 1
            else if (grid(r,i) == 'O') then
                grid(r,i) = '.'
                grid(r,curTop) = 'O'
                curTop = curTop - 1
            endif
        enddo
    endsubroutine

    subroutine rlColN(grid, c,siz)
        character,dimension(:,:), allocatable,intent(inout)       :: grid
        integer(kind=LargeInt_Vect), intent(in)       :: c, siz
        integer(kind=LargeInt_Vect)                   :: curTop, i
        curTop = 1
        do i = 1, siz
            if (grid(i, c) == '#') then
                curTop = i + 1
            else if (grid(i,c) == 'O') then
                grid(i,c) = '.'
                grid(curTop,c) = 'O'
                curTop = curTop + 1
            endif
        enddo
    endsubroutine

    subroutine rlColS(grid, c, siz)
        character,dimension(:,:), allocatable, intent(inout)       :: grid
        integer(kind=LargeInt_Vect), intent(in)       :: c, siz
        integer(kind=LargeInt_Vect)                   :: curTop, i
        curTop = siz
        do i =  siz, 1, -1
            if (grid(i,c) == '#') then
                curTop = i - 1
            else if (grid(i,c) == 'O') then
                grid(i,c) = '.'
                grid(curTop,c) = 'O'
                curTop = curTop - 1
            endif
        enddo
    endsubroutine

    subroutine rolGrd(grid, dir, siz)
        character,dimension(:,:),allocatable, intent(inout)       :: grid
        character,intent(in)                          :: dir
        integer(kind=LargeInt_Vect),intent(in)        :: siz
        integer(kind=LargeInt_Vect)                   :: i
        integer                                       :: mxThrd
        mxThrd = 8
        if (dir == 'N') then
            !$OMP PARALLEL NUM_THREADS(mxThrd)
            !$OMP DO
            do i = 1, siz
                call rlColN(grid, i, siz)
            enddo
            !$OMP END DO
            !$OMP END PARALLEL
        else if (dir == 'O') then
            !$OMP PARALLEL NUM_THREADS(mxThrd)
            !$OMP DO
            do i = 1, siz
                call rlRowO(grid, i, siz)
            enddo
            !$OMP END DO
            !$OMP END PARALLEL
        else if (dir == 'S') then
            !$OMP PARALLEL NUM_THREADS(mxThrd)
            !$OMP DO
            do i = 1, siz
                call rlColS(grid, i, siz)
            enddo
            !$OMP END DO
            !$OMP END PARALLEL
        else
            !$OMP  PARALLEL NUM_THREADS(mxThrd)
            !$OMP  DO
            do i = 1, siz
                call rlRowE(grid, i, siz)
            enddo
            !$OMP END DO
            !$OMP END PARALLEL
        endif
    endsubroutine

    function nSupp(grid, siz) result(res)
        character,dimension(:,:),allocatable,intent(inout)       :: grid
        integer(kind=LargeInt_Vect),intent(in)        :: siz
        integer(kind=LargeInt_Vect)                   :: res, i, j
        res = 0
        do i = 1, siz
            do j = 1, siz
                if (grid(i,j) == 'O') then
                    res = res + (siz - i + 1)
                endif
            enddo
        enddo
    endfunction



    subroutine part2()
        character,dimension(:,:), allocatable         :: grid
        type(vectGd)                                  :: rmb
        integer(kind=LargeInt_Vect)                   :: res, siz, i, max, tmp
        integer(kind=LargeInt_VectGrid)               :: check
        allocate(grid(width,width))
        call parse(grid, siz)
        max = 1000000000
        call inVcGd(rmb,siz)
        do i = 1, max
            call rolGrd(grid, 'N', siz)
            call rolGrd(grid, 'O', siz)
            call rolGrd(grid, 'S', siz)
            call rolGrd(grid, 'E', siz)
            check = adVcGd(rmb,grid)
            if (check /= 0) exit
        enddo
        tmp = mod(max - check, i - check)
        if (tmp == 0) then
            tmp = i - 1
        else
            tmp = check + tmp
        endif
        grid = atVcGd(rmb,tmp)
        res = nSupp(grid, siz)
        print*, 'res = ', res
    endsubroutine

endmodule

program main
    use m

    print *,''
    print *,'Begin AOC main Day 14'
    print *,''

    input='input.txt'
    print*, 'Part 1'
    call part1()
    print*, 'Part 2'
    call part2()

    print *,''
    print *,'Ending AOC main Day 14'
    print *,''
endprogram
