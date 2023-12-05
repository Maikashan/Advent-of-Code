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


            ! MUST MAKE A WHILE TO READ WHILE THERE ARE NB
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
        integer :: io,  isize, i, nbSeed, j, lenL, nbMap, k, l, begin, m, n
        character(len=:),allocatable  :: s
        character(len=bufflen) :: buffer
        logical                            ::  eof, cond1, cond2
        integer, parameter :: LargeInt_K = selected_int_kind(18)
        integer (kind=LargeInt_K) :: src, dst, range, res , tmp
        !7 is the amount of maps, 144 (the max nb of els in a map + 1) * 3
        !dynamic allocation should take place here, but well, let's make it work before doing that
        integer(kind=LargeInt_K), dimension(7,48) :: mapdst, mapsrc, mapr, curdst, cursrc, curra
        integer,dimension(2)                      :: resMin
        integer, dimension(48)                    :: prevL, currL
        integer (kind=LargeInt_K), dimension(20)  :: seeds

        print *,'input file = ', input

        open(UNIT=1, file=input, status='old')

        eof=.false.

        mapsrc = -1
        mapdst = -1
        mapr = -1


        call gtSeed(1, seeds, nbSeed)
        read(1,'(a)',advance='no', iostat=io, size=isize) buffer
        eof = io == iostat_end
        nbMap = 0
        do while(.not. eof)
            !Lexing part
            s = ''
            !Read the blank line and the text part
            read(1,'(a)',advance='no', iostat=io, size=isize) buffer
            eof = io == iostat_end
            if (eof) exit


            nbMap = nbMap + 1
            lenL = 0
            do while (.not. eof)
                lenL = lenL + 1
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
                print*, 's = ', s
                call getNum(s, dst, i)
                call nextNb(s,i)
                call getNum(s,src,i)
                call nextNb(s,i)
                call getNum(s,range,i)
                print*, 'dst = ', dst, ' src = ', src, ' range = ', range, ' len L = ',lenL
                do n = 1,lenL 
                    if(mapdst(n, nbMap) == -1) then
                        mapdst(n, nbMap) = dst
                        mapsrc(n, nbMap) = src
                        mapr(n,nbMap) = range
                        exit
                    else if (mapdst(n,nbMap) > dst) then
                        tmp = mapdst(n, nbMap)
                        mapdst(n,nbMap) = dst
                        dst = tmp
                        tmp = mapsrc(n,nbMap)
                        mapsrc(n,lenL) = src
                        src = tmp
                        tmp = mapr(n,nbMap)
                        mapr(n, nbMap) =range 
                        range = tmp
                    endif 
                enddo
            enddo
        enddo

        !We then go back with the smaller dest etc..
        do i = 1, 7
            j = 1
            do while (mapdst(j,i) /= -1)
                print*,mapdst(j,i),' '
                j = j + 1
            enddo
            print*,''
        enddo
        res = -1
        do while (res == -1)
          curdst = mapdst
          cursrc = mapsrc
          curra = mapr
          resMin = MINLOC(curdst(:,7:7),MASK = curdst(:,7:7) .GT. -1)
          print*,'res Min = ', resMin
          call sleep(5)
          begin = resMin(1)
          j = 6
          prevL= -1
          prevL(1) = begin
          print* , 'i = ', begin, 'val is = ', curdst(begin,7)
            do while (j > 0)
            i = 1
            currL = -1
            l = 0
            do while (prevL(i) /= -1)
              k = 1 
              do while (curdst(k,j) /= -1)
                cond1 = ((curdst(k,j)- curra(k,j) <  cursrc(prevL(i),j + 1)).and.(curdst(k,j) > cursrc(prevL(i),j + 1)))
                cond2 = ((cursrc(prevL(i),j + 1) - curra(prevL(i),j + 1) <  curdst(k,j)).and.(curdst(k,j) < cursrc(prevL(i),j + 1)))
                if (cond1 .or. cond2)then
                  curra(k,j) = MIN(curra(k,j),curdst(k,j) - cursrc(prevL(i), j + 1) + curra(prevL(i),j+1))
                  cursrc(k,j) = cursrc(k,j) + MIN(0,cursrc(prevL(i), j + 1) - curdst(k,j))
                  l = l + 1
                  currL(l) = k
                endif
                k = k + 1
              enddo
              i = i +1 
            enddo
            prevL = currL
            j = j - 1
            enddo
            m = 1
            do while (prevL(m) /= -1)
                do n = 1,  nbSeed, 2
                    if (seeds(n) - curra(prevL(m), 1) < cursrc(prevL(m),1).and.seeds(n + 1) > cursrc(prevL(m),1) ) then
                        if (res == - 1) then
                            res = seeds(min(seeds(n), cursrc(prevL(m),1)))
                        else
                            res = min(seeds(n), cursrc(prevL(m),1))
                        endif
                    endif
                enddo
                m = m + 1
            enddo
            curdst(begin,7) = -1
        enddo
        print*, 'res = ', res
        close(1)
endsubroutine

endmodule

program main
    use m

    print *,''
    print *,'Begin AOC main Day 1'
    print *,''

    input='example1.txt'
    call part1()
    call part2()

    print *,''
    print *,'Ending AOC main Day 1'
    print *,''
endprogram
