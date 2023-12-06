module m
    use iso_fortran_env
    implicit none
    character(len=:), allocatable :: input

contains 


subroutine getNum(str, int_res, i)
    character(:), allocatable, intent(in) :: str
    integer, intent(inout)                :: i
    integer, parameter :: LargeInt_K = selected_int_kind(18)
    integer (kind=LargeInt_K), intent(out):: int_res
    integer (kind=LargeInt_K)             :: nb
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
    character(:), allocatable, intent(in) :: str
    integer, intent(inout)                :: i

    do while(i <= len(str))
        if (str(i:i) >= '0' .and. str(i:i) <= '9') exit
        i = i + 1
    enddo
endsubroutine

subroutine gtSeed(u, seeds, nbSeed)
    integer, parameter :: bufflen=1024
    integer, intent(in) :: u
    integer, intent(out) :: nbSeed
    integer :: io,  isize, i
    character(len=:),allocatable  :: s
    character(len=bufflen) :: buffer
    logical                            ::  eof
    integer, parameter :: LargeInt_K = selected_int_kind(18)
    integer (kind=LargeInt_K) :: nb
    integer (kind=LargeInt_K), dimension(20), intent(out) :: seeds

    s = ''
    read(u,'(a)',advance='no', iostat=io, size=isize) buffer
    if(isize.gt.0)then 
        s = s//buffer(:isize)
    endif
    eof = io == iostat_end
    if (.not. eof) then
    i = 1
    nbSeed = 1
    !Get the seeds
    call nextNb(s,i)
    do while (i < len(s))
        call getNum(s, nb, i)
        seeds(nbSeed) = nb
        call nextNb(s,i)
        nbSeed = nbSeed + 1
    enddo
    nbSeed = nbSeed - 1
    endif
endsubroutine    


subroutine gtSran(u, seedst, seesrc, seeran, nbSeed)
    integer, parameter :: bufflen=1024
    integer, intent(in) :: u
    integer, intent(out) :: nbSeed
    integer :: io,  isize, i
    character(len=:),allocatable  :: s
    character(len=bufflen) :: buffer
    logical                            ::  eof
    integer, parameter :: LargeInt_K = selected_int_kind(18)
    integer (kind=LargeInt_K) :: nb
    integer (kind=LargeInt_K), dimension(:),allocatable,  intent(inout) :: seedst, seesrc, seeran

    s = ''
    read(u,'(a)',advance='no', iostat=io, size=isize) buffer
    if(isize.gt.0)then 
        s = s//buffer(:isize)
    endif
    eof = io == iostat_end
    if (.not. eof) then
    i = 1
    nbSeed = 1
    !Get the seeds
    call nextNb(s,i)
    do while (i < len(s))
        call getNum(s, nb, i)
        seedst(nbSeed) = nb
        seesrc(nbSeed) = nb
        call nextNb(s,i)
        call getNum(s, nb, i)
        seeran(nbSeed) = nb
        call nextNb(s,i)
        nbSeed = nbSeed + 1
    enddo
    nbSeed = nbSeed - 1
    endif
endsubroutine    

subroutine part1()
        integer, parameter :: bufflen=1024
        integer :: io,  isize, i, nbSeed, j
        character(len=:),allocatable  :: s
        character(len=bufflen) :: buffer
        logical                            ::  eof
        integer, parameter :: LargeInt_K = selected_int_kind(18)
        integer (kind=LargeInt_K) :: src, dst, range, res
        integer (kind=LargeInt_K), dimension(20) :: seeds
        logical, dimension(20)                   :: trslte

        print *,'input file = ', input

        open(UNIT=1, file=input, status='old')

        eof=.false.

        call gtSeed(1, seeds, nbSeed)
        read(1,'(a)',advance='no', iostat=io, size=isize) buffer
        eof = io == iostat_end
        do while(.not. eof)
            !Lexing part
            trslte(:) = .false.
            s = ''
            !Read the blank line and the text part
            read(1,'(a)',advance='no', iostat=io, size=isize) buffer
            eof = io == iostat_end
            if (eof) exit


            do while (.not. eof)
                s = ''
                read(1,'(a)',advance='no', iostat=io, size=isize) buffer
                if(isize.gt.0)then 
                    s = s//buffer(:isize)
                endif
                eof = io == iostat_end
                if (eof) exit
                if (s(1:1) < '0' .or. s(1:1) > '9') exit
                i = 1
                
                !Get the destination
                call getNum(s, dst, i)
                !Get the source
                call nextNb(s,i)
                call getNum(s,src,i)
                !Get the range
                call nextNb(s,i)
                call getNum(s,range,i)

                
                do j = 1, nbSeed
                    if (.not. trslte(j)) then
                        if (seeds(j) >= src .and. seeds(j) - range <= src) then
                            
                            seeds(j) = dst + (seeds(j) - src)
                            trslte(j) = .true.
                        endif
                    endif
                enddo
            enddo
        enddo
        res = MINVAL(seeds(1:nbSeed))
        print*, 'res = ', res
        close(1)
endsubroutine


subroutine part2()
    integer, parameter :: bufflen=1024
    integer :: io,  isize, preSiz, lenL, i, j, k, cursiz
    character(len=:),allocatable  :: s
    character(len=bufflen) :: buffer
    logical                            ::  eof, srcBig, ranBig, equal, tooBig, tooSml
    integer, parameter :: LargeInt_K = selected_int_kind(16)
    integer (kind=LargeInt_K) :: src, dst, range, res, tmpran
    integer (kind=LargeInt_K), allocatable, dimension(:) :: curdst, cursrc, curran,predst, presrc, preran
    logical, allocatable, dimension(:) :: toKeep
    !buffer to build the map
    integer (kind=LargeInt_K), dimension(47) :: lindst, linsrc, linran
    
    print *,'input file = ', input
    

    open(UNIT=1, file=input, status='old')

    eof=.false.

    allocate(predst(20))
    allocate(presrc(20))
    allocate(preran(20))
    predst = -1
    presrc = -1
    preran = -1
    call gtSran(1, predst, presrc, preran, preSiz)
    read(1,'(a)',advance='no', iostat=io, size=isize) buffer
    eof = io == iostat_end
    do while(.not. eof)
        !Lexing part
        s = ''
        !Read the blank line and the text part
        read(1,'(a)',advance='no', iostat=io, size=isize) buffer
        eof = io == iostat_end
        if (eof) exit

        lenL = 0
        lindst = -1
        linsrc = -1
        linran = -1
        do while (.not. eof)
            s = ''
            read(1,'(a)',advance='no', iostat=io, size=isize) buffer
            if(isize.gt.0)then 
                s = s//buffer(:isize)
            else
                eof = io == iostat_end
                exit
            endif
            eof = io == iostat_end
            if (eof) exit
            if (s(1:1) < '0' .or. s(1:1) > '9') exit
            i = 1
            lenL = lenL + 1
            
            !Get the destination
            !print*, 's = ', s
            call getNum(s, dst, i)
            call nextNb(s,i)
            call getNum(s,src,i)
            call nextNb(s,i)
            call getNum(s,range,i)
            !print*, 'dst = ', dst, ' src = ', src, ' range = ', range, ' len L = ',lenL
            lindst(lenL) = dst
            linsrc(lenL) = src
            linran(lenL) = range
        enddo
        allocate(curdst((2 * lenL + preSiz) * 2))
        allocate(cursrc((2 * lenL + preSiz) * 2))
        allocate(curran((2 * lenL + preSiz) * 2))
        allocate(toKeep((2 * lenL + preSiz) * 2))
        curdst = -1
        cursrc = -1
        curran = -1
        
        toKeep = .true.
        cursiz = 0
        do j = 1, lenL
            ! get through the current map
            ! check if each seed conflict with the range
            do k = 1, preSiz
                srcBig = linsrc(j)>predst(k)
                ranBig = linsrc(j)+linran(j) >= predst(k) + preran(k)
                equal = linsrc(j) == predst(k) .and. linran(j) == preran(k)
                tooBig = linsrc(j) >= predst(k) + preran(k)
                tooSml = linsrc(j) + linran(j) < predst(k)
            ! No need to keep unnecessary ranges
                if (tooBig.or.tooSml.or.(.not.toKeep(k))) then
                    cycle
            ! If the range is the same
                else if (equal) then
                    toKeep(k) = .false.
                    cursiz = cursiz + 1
                    curdst(cursiz) = lindst(j)
                    cursrc(cursiz) = linsrc(j)
                    curran(cursiz) = linran(j)
            ! first case : the range is in the middle of the seed
                else if (srcBig.and.(.not.ranBig)) then
                    ! update current so we have the 'left part' of the split
                cursiz = cursiz + 1
                    tmpran = preran(k)
                    preran(k) = linsrc(j) - predst(k)
                    ! add the right part of the split to the right
                    preSiz = preSiz + 1
                    predst(preSiz) = linsrc(j) + linran(j)
                    presrc(preSiz) = presrc(k) + (predst(preSiz) - predst(k))
                    preran(preSiz) = tmpran - (predst(preSiz) - predst(k))
                    curdst(cursiz) = lindst(j)
                    cursrc(cursiz) = linsrc(j)
                    curran(cursiz) = linran(j)
            ! second case : the range is to the 'right' of the seed
                else if(srcBig.and.ranBig) then
                cursiz = cursiz + 1
                    tmpran = preran(k)
                    preran(k) = linsrc(j) - predst(k)
                    curdst(cursiz) = lindst(j)
                    cursrc(cursiz) = linsrc(j)
                    curran(cursiz) =  tmpran - preran(k)
            ! third case : the range is to the 'left' of the seed
                else if ((.not. srcBig) .and. (.not.ranBig)) then
                cursiz = cursiz + 1
                    curdst(cursiz) = lindst(j) + (predst(k) - linsrc(j))
                    cursrc(cursiz) = predst(k)
                    curran(cursiz) = linran(j) - (predst(k) - linsrc(j))
                    preran(k) = preran(k) - curran(cursiz)
                    predst(k) = predst(k) + curran(cursiz)
            ! fourth case : the seed is to the middle of the range
                else if ((.not.srcBig) .and. ranBig) then
                cursiz = cursiz + 1
                    curdst(cursiz) = lindst(j) + (predst(k) -linsrc(j))
                    cursrc(cursiz) = predst(k)
                    curran(cursiz) = preran(k)
                    toKeep(k) = .false.
                endif
            enddo
        enddo
        do j = 1, preSiz
            if (.not.toKeep(j)) cycle
            cursiz = cursiz + 1
            curdst(cursiz) = predst(j)
            cursrc(cursiz) = presrc(j)
            curran(cursiz) = preran(j)
        enddo
        deallocate(predst)
        deallocate(presrc)
        deallocate(preran)
        predst = curdst
        presrc = cursrc
        preran = curran
        preSiz = cursiz
        deallocate(curdst)
        deallocate(cursrc)
        deallocate(curran)
        deallocate(toKeep)
    enddo
    res = minval(predst,mask= predst /= -1)
    print*, 'res = ', res
endsubroutine
endmodule

program main
    use m

    print *,''
    print *,'Begin AOC main Day 5'
    print *,''

    input='input.txt'
    print*,'Part 1'
    call part1()
    print*,'Part 2'
    call part2()

    print *,''
    print *,'Ending AOC main Day 5'
    print *,''
endprogram
