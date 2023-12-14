module v1
    use iso_fortran_env
    use vectChar
    implicit none
    character(len=:), allocatable :: input
    integer, parameter            :: height = 6
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


    function gtBegn(spring, i) result(res)
        type(vectCh),intent(inout)                             :: spring
        integer(kind=LargeInt_Vect), intent(in)                :: i
        integer(kind=LargeInt_Vect)                            :: res
        res = i
        do while (res > 0)
            if (atVcCh(spring, res) /= '#') exit
            res = res - 1
        enddo
        if (res == 0) res = 1
    endfunction

    function curval(spring, map, max, begin) result(res)
        type(vectCh),intent(inout)                             :: spring
        type(vector),intent(inout)                             :: map
        integer(kind=LargeInt_Vect), intent(in)                :: begin
        type(vector)                                           :: tmpMap
        integer(kind=LargeInt_Vect), intent(in)                :: max
        logical                                                :: res
        integer(kind=LargeInt_Vect)                            :: i, j, curr, val
        character                                              :: tmp
        if (begin <= spring%curSiz) then
            val = gtBegn(spring, begin - 1)
        else
            val = begin
        endif
        res = .false.
        j = 1
        curr = 0
        do i = val, max
            tmp = atVcCh(spring, i)
            if (tmp == '#') then
                curr = curr + 1
            else
                if (curr /= 0) then
                    if(map%curSiz < j) exit
                    if (atVect(map,j) /= curr) exit
                    j = j + 1
                endif
                curr = 0
            endif
        enddo
        if (i > max) then
            if (curr /= 0 .and. (map%curSiz < j.or.map%curSiz==0)) then
                res = .false.
            else
                res = .true.
            endif
            if (begin > spring%curSiz.or.i > spring%curSiz) then
                if (atVect(map,j) == curr.and. j == map%curSiz) then
                    j = j  +1
                endif
            endif
        endif
        call initVc(tmpMap, map%curSiz)
        do i = j, map%curSiz
            call addVec(tmpMap, atVect(map, i))
        enddo
        call rmVect(map)
        map = tmpmap
    endfunction

    RECURSIVE function comput(spring, map, begin) result(res)
        integer(kind=LargeInt_Vect), intent(in)                :: begin
        type(vectCh),intent(inout)                             :: spring
        type(vectCh)                                           :: s
        type(vector),intent(inout)                             :: map
        type(vector)                                           :: m
        integer(kind=LargeInt_Vect)                            :: i, j, res
        s = spring
        m = map
        res = 0
        if (begin <= spring%curSiz) then
            do i = begin, spring%curSiz
                if (atVcCh(spring, i) == '?') then
                    s%vect(i:i) = '.'
                    if (curval(s, m, i, begin)) then
                        res = comput(s, m, i + 1)
                    endif
                    m = map
                    s%vect(i:i) = '#'
                    if (curval(s, m, i, begin)) then
                        res = res + comput(s, m, i + 1)
                    endif
                    exit
                endif
            enddo
        else
            i = begin
        endif
        if (i > spring%curSiz) then
            if (curval(spring, map, spring%curSiz, begin)) then
                if (map%curSiz == 0) then
                    res = 1
                endif
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

    subroutine repars(spring, map)
        type(vectCh),dimension(height),intent(inout)  :: spring
        type(vector),dimension(height),intent(inout)  :: map
        type(vectCh)                                  :: oldS
        type(vector)                                  :: oldMap
        integer(kind=LargeInt_Vect)                   :: i, j, k
        do i = 1, height
            oldS = spring(i)
            oldMap = map(i)
            do j = 1, 4
                call adVcCh(spring(i),'?')
                do k = 1, oldS%curSiz
                    call adVcCh(spring(i),atVcCh(oldS,k))
                enddo
                do k = 1, oldMap%curSiz
                    call addVec(map(i), atVect(oldMap, k))
                enddo
            enddo
        enddo
    endsubroutine

    subroutine part2()
        type(vectCh),dimension(height)                :: spring
        type(vector),dimension(height)                :: map
        integer(kind=LargeInt_Vect),dimension(height) :: resArr
        integer(kind=LargeInt_Vect)                   :: res, h, tmp
        call parse(spring, map)
        call repars(spring, map)
        tmp = 1
        do h = 1, height
            print*,'h = ', h
            resArr(h) = comput(spring(h), map(h), tmp)
        enddo
        res = SUM(resArr)
        print*, 'res = ', res
    endsubroutine

endmodule

module v2
    use iso_fortran_env
    use vectLogi
    use vectChar
    implicit none
    character(len=:), allocatable :: input
    integer, parameter            :: height = 6
contains

    subroutine getNum(str, int_res, i)
        character(:), allocatable, intent(in)      :: str
        integer(kind=LargeInt_VectLogi), intent(inout) :: i
        integer(kind=LargeInt_VectLogi), intent(out)   :: int_res
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
        integer(kind=LargeInt_VectLogi),intent(inout)  :: i

        do while(i <= len(str))
            if (str(i:i) >= '0' .and. str(i:i) <= '9') exit
            i = i + 1
        enddo
    endsubroutine

    subroutine gtSprg(spring, s, i, p)
        integer,intent(in)                             :: p
        type(vectCh),intent(inout)                     :: spring
        integer(kind=LargeInt_VectLogi), intent(inout) :: i
        integer(kind=LargeInt_VectLogi)                :: siz, k, j
        character(len=:),allocatable, intent(in)       :: s
        siz = 25
        call inVcCh(spring, siz)
        call adVcCh(spring, '.')
        do while (s(i:i) /= ' ')
            call adVcCh(spring,s(i:i))
            i = i + 1
        enddo
        if (p == 2) then
            do j = 1, 4
                call adVcCh(spring, '?')
                do k = 1, i - 1
                    call adVcCh(spring, s(k:k))
                enddo
            enddo
        endif
        call adVcCh(spring, '.')
    endsubroutine

    subroutine gtMap(map, s, i, p)
        integer,intent(in)                             :: p
        type(vectLo),intent(inout)                     :: map
        integer(kind=LargeInt_VectLogi), intent(inout) :: i
        integer(kind=LargeInt_VectLogi)                :: siz, num, j, k, l, m
        character(len=:),allocatable, intent(in)       :: s
        siz = 25
        call inVcLo(map, siz)
        call adVcLo(map, .false.)
        k = i
        l = 1
        if (p == 2) l = 5
        do m = 1, l
            i = k
            do while (i <= len(s))
                call getNum(s, num, i)
                do j = 1, num
                    call adVcLo(map, .true.)
                enddo
                call adVcLo(map, .false.)
                call nextNb(s,i)
            enddo
        enddo
        
    endsubroutine

    subroutine parseL(spring, map, u, eof, p)
        integer, parameter :: bufflen=1024
        integer, intent(in)                           :: u, p
        type(vectCh),intent(inout)                    :: spring
        type(vectLo),intent(inout)                    :: map 
        integer                                       :: io,  isize
        integer(kind=LargeInt_VectLogi)               :: i
        character(len=:),allocatable                  :: s
        character(len=bufflen)                        :: buffer
        logical, intent(inout)                        :: eof
        s = ''
        read(u,'(a)',advance='no', iostat=io, size=isize) buffer
        if(isize.gt.0)then 
            s = s//buffer(:isize)
        endif
        eof = iostat_end == io
        if (.not.eof) then
            i = 1
            call gtSprg(spring, s, i, p)
            i = i + 1
            call gtMap(map, s, i, p)
        endif
    endsubroutine

    function compute(spring, map) result(res)
        type(vectCh),intent(inout)                                 :: spring
        type(vectLo),intent(inout)                                 :: map 
        integer(kind=LargeInt_VectLogi)                            :: res, i, j
        integer(kind=LargeInt_VectLogi),dimension(:,:),allocatable :: vals
        logical                                                    :: fine, broke
        allocate(vals(spring%curSiz + 2, map%curSiz + 2))
        vals = 0
        vals(spring%curSiz + 1,map%curSiz + 1) = 1
        do i = spring%curSiz, 1, -1
            do j = map%curSiz, 1, -1
                broke = .false.
                fine = .false.
                if (atVcCh(spring, i) == '#') then
                    broke = .true.
                else if (atVcCh(spring, i) == '.') then
                    fine = .true.
                else
                    fine = .true.
                    broke = .true.
                endif
                res = 0
                if (broke.and.atVcLo(map,j)) then
                    res = res + vals(i + 1,j + 1)
                else if (fine.and.(.not.atVcLo(map,j))) then
                    res = res + vals(i + 1, j + 1) + vals(i + 1, j)
                endif
                vals(i,j) = res
            enddo
        enddo
        res = vals(1,1)
        call rmVcCh(spring)
        call rmVcLo(map)
    endfunction

    subroutine part(p)
        integer, intent(in)                           :: p
        type(vectCh)                                  :: spring
        type(vectLo)                                  :: map 
        integer                                       :: u
        logical                                       :: eof
        integer(kind=LargeInt_VectLogi)               :: res
        res = 0
        u = 1
        open(UNIT=u, file=input, status='old')
        eof = .false.
        call parseL(spring, map, u, eof, p)
        do while(.not.eof)
            res = res + compute(spring, map)
            call parseL(spring, map, u, eof, p)
        enddo
        close(u)
        print*, 'res = ', res
    endsubroutine

endmodule

program main
    use v2

    print *,''
    print *,'Begin AOC main Day 12'
    print *,''

    input='input.txt'
    print*, 'Part 1'
    call part(1)
    print*, 'Part 2'
    call part(2)

    print *,''
    print *,'Ending AOC main Day 12'
    print *,''
endprogram
