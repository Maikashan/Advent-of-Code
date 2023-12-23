module m
    use iso_fortran_env
    use queueL
    implicit none
    character(len=:), allocatable :: input
    character(len=*),parameter    :: free='(*(g0,1x))'
    integer,parameter             :: width=110
    integer, parameter :: LargeInt = selected_int_kind(32)
contains

    subroutine parse(grid)
        character,dimension(width,width),intent(inout):: grid
        integer, parameter :: bufflen=32768
        integer                                       :: u
        logical                                       :: eof
        integer                                       :: io,  isize
        character(len=:),allocatable                  :: s
        character(len=bufflen)                        :: buffer
        integer(kind=LargeInt)                        :: w, i
        u = 1
        open(UNIT=u, file=input, status='old')
        eof = .false.
        w = 0
        do while (.not.eof)
            s = ''
            w = w + 1
            read(u,'(a)',advance='no', iostat=io, size=isize) buffer
            if(isize.gt.0)then 
                s = s//buffer(:isize)
            endif
            eof = iostat_end == io
            if (eof) exit
            do i = 1, len(s)
                grid(i, w) = s(i:i)
            enddo
        enddo
        close(u)
    endsubroutine

    function isIn(p) result(res)
        type(pos), intent(in)                             :: p
        logical                                           :: res
        res = .true.
        if (p%x < 1.or.p%x > width.or.p%y<1.or.p%y>width) res=.false.
    endfunction

    subroutine clearP(p)
        type(pos), intent(out)                            :: p
        p%x = 0
        p%y = 0
        p%from = ""
    endsubroutine

    subroutine getDir(c, newpos, p1, p2)
        character,intent(in)                              :: c
        type(pos),intent(in)                              :: newpos
        type(pos),intent(out)                             :: p1, p2
        call clearP(p2)
        if (newpos%from == 'N') then
            if (c == '.') then
                p1%x = newpos%x;
                p1%y = newpos%y + 1;
                p1%from = "N"
            else if (c == '/') then
                p1%x = newpos%x - 1
                p1%y = newpos%y
                p1%from = 'E'
            else if (c == '\') then
                p1%x = newpos%x + 1
                p1%y = newpos%y
                p1%from = 'O'
            else if (c == '|') then
                p1%x = newpos%x;
                p1%y = newpos%y + 1;
                p1%from = "N"
            !splitter -
            else 
                p1%x = newpos%x - 1
                p1%y = newpos%y
                p1%from = 'E'
                p2%x = newpos%x + 1
                p2%y = newpos%y
                p2%from = 'O'
            endif
        else if (newpos%from == 'S') then
            if (c == '.') then
                p1%x = newpos%x;
                p1%y = newpos%y - 1;
                p1%from = "S"
            else if (c == '/') then
                p1%x = newpos%x + 1
                p1%y = newpos%y
                p1%from = 'O'
            else if (c == '\') then
                p1%x = newpos%x - 1
                p1%y = newpos%y
                p1%from = 'E'
            else if (c == '|') then
                p1%x = newpos%x;
                p1%y = newpos%y - 1;
                p1%from = "S"
            !splitter -
            else 
                p1%x = newpos%x - 1
                p1%y = newpos%y
                p1%from = 'E'
                p2%x = newpos%x + 1
                p2%y = newpos%y
                p2%from = 'O'
            endif
        else if (newpos%from == 'E') then
            if (c == '.') then
                p1%x = newpos%x - 1
                p1%y = newpos%y
                p1%from = 'E'
            else if (c == '/') then
                p1%x = newpos%x;
                p1%y = newpos%y + 1;
                p1%from = "N"
            else if (c == '\') then
                p1%x = newpos%x;
                p1%y = newpos%y - 1;
                p1%from = "S"
            else if (c == '|') then
                p1%x = newpos%x;
                p1%y = newpos%y + 1;
                p1%from = "N"
                p2%x = newpos%x;
                p2%y = newpos%y - 1;
                p2%from = "S"
            !splitter -
            else 
                p1%x = newpos%x - 1
                p1%y = newpos%y
                p1%from = 'E'
            endif
        else
            if (c == '.') then
                p1%x = newpos%x + 1
                p1%y = newpos%y
                p1%from = 'O'
            else if (c == '/') then
                p1%x = newpos%x;
                p1%y = newpos%y - 1;
                p1%from = "S"
            else if (c == '\') then
                p1%x = newpos%x;
                p1%y = newpos%y + 1;
                p1%from = "N"
            else if (c == '|') then
                p1%x = newpos%x;
                p1%y = newpos%y + 1;
                p1%from = "N"
                p2%x = newpos%x;
                p2%y = newpos%y - 1;
                p2%from = "S"
            !splitter -
            else 
                p1%x = newpos%x + 1
                p1%y = newpos%y
                p1%from = 'O'
            endif
        endif
        if (.not.isIn(p1)) then
            call clearP(p1)
        endif
        if (.not.isIn(p2)) then
            call clearP(p2)
        endif
    endsubroutine

    function nbTile(grid,start) result(res)
        character,dimension(width,width),intent(in)   :: grid
        type(pos),intent(in)                          :: start
        logical,dimension(width,width)                :: seen
        integer(kind=LargeInt)                        :: res
        integer(kind=LargeInt)                        :: siz
        type(pos)                                     :: p1, p2, curr
        type(queue)                                   :: q
        type(vector)                                  :: set
        siz = 32
        res = 0
        seen = .false.
        call initQu(q,siz)
        call initVc(set,siz)
        curr = start
        call pushQu(q, curr)
path:   do while (q%q%curSiz /= 0)
            curr = peekQu(q)
            call popQu(q)
            if (.not.addVec(set,curr))then
                cycle path
            endif
            if (.not.seen(curr%x,curr%y)) then
                res = res + 1
                seen(curr%x,curr%y) = .true.
            endif
            call getDir(grid(curr%x,curr%y),curr,p1,p2)
            if (p1%x /= 0) then
                call pushQu(q, p1)
            endif
            if (p2%x /= 0) then
                call pushQu(q,p2)
            endif
        enddo path
        call rmQu(q)
        call rmVect(set)
    endfunction

    subroutine part1()
        character,dimension(width,width)              :: grid
        integer(kind=LargeInt_Vect)                   :: res
        type(pos)                                     :: start
        res = -1
        call parse(grid)
        start%x = 4
        start%y = 1
        start%from = "N"
        res = nbTile(grid,start)
        print*,'res = ', res
    endsubroutine

    subroutine part2()
        character,dimension(width,width)              :: grid
        integer(kind=LargeInt_Vect)                   :: res, tmp, i
        type(pos)                                     :: start
        res = -1
        call parse(grid)
        !$OMP PARALLEL NUM_THREADS(8)
        !$OMP DO
        do i = 1, width
            start%x = i
            start%y = 1
            start%from = "N"
            tmp = nbTile(grid,start)
            res = max(res,tmp)
        enddo
        !$OMP END DO
        !$OMP END PARALLEL
        !$OMP PARALLEL NUM_THREADS(8)
        !$OMP DO
        do i = 1, width
            start%x = i
            start%y = width 
            start%from = "S"
            tmp = nbTile(grid,start)
            res = max(res,tmp)
        enddo
        !$OMP END DO
        !$OMP END PARALLEL
        !$OMP PARALLEL NUM_THREADS(8)
        !$OMP DO
        do i = 1, width
            start%x = 1
            start%y = i
            start%from = "O"
            tmp = nbTile(grid,start)
            res = max(res,tmp)
        enddo
        !$OMP END DO
        !$OMP END PARALLEL
        !$OMP PARALLEL NUM_THREADS(8)
        !$OMP DO
        do i = 1, width
            start%x = width
            start%y = i
            start%from = "E"
            tmp = nbTile(grid,start)
            res = max(res,tmp)
        enddo
        !$OMP END DO
        !$OMP END PARALLEL
        print*,'res = ', res
    endsubroutine

endmodule

program main
    use m

    print *,''
    print *,'Begin AOC main Day 16'
    print *,''

    input='input.txt'
    print*, 'Part 1'
    call part1()
    print*, 'Part 2'
    call part2()

    print *,''
    print *,'Ending AOC main Day 16'
    print *,''
endprogram
