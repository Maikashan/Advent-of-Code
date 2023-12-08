module m
    use iso_fortran_env
    implicit none
    character(len=:), allocatable :: input
    integer,parameter             :: nbNode = 756
    integer, parameter :: LargeInt_K = selected_int_kind(32)
    type node
        character(3) :: name
        character(3) :: L
        character(3) :: R
    endtype

contains 

subroutine getWrd(str, strRes, i)
    character(:), allocatable, intent(in) :: str
    character(3), intent(out)             :: strRes 
    integer, intent(inout)                :: i
    character                             :: c
    strRes = ''
    do i = i, len(str)
        if (str(i:i) >=  'A' .and. str(i:i) <= 'Z') then
            read(str(i:i),*) c
            strRes = trim(strRes) // c
        else if  (str(i:i) >= '0'.and. str(i:i) <= '9') then
            read(str(i:i),*) c
            strRes = trim(strRes) // c
        else
            exit
        endif
    enddo
endsubroutine


subroutine nextWd(str, i)
    character(:), allocatable, intent(in) :: str
    integer, intent(inout)                :: i

    do while(i <= len(str))
        if (str(i:i) >= 'A' .and. str(i:i) <= 'Z') exit
        if (str(i:i) >= '0'.and. str(i:i) <= '9') exit
        i = i + 1
    enddo
endsubroutine

! The research could be done in depth, will be optimized if needed
function findI(mapper, word) result(index)
    type(node), dimension(:),intent(in) :: mapper
    character(3), intent(in)            :: word
    integer                             :: index
    do index = 1, size(mapper)
        if (mapper(index)%name == word) exit
    enddo
endfunction

subroutine parse(u, path, map, begin, end)
        integer, intent(in) :: u
        integer,dimension(:), allocatable, intent(out):: begin, end
        character(len=:),allocatable, intent(out) :: path
        integer,dimension(nbNode, 2), intent(out) :: map
        integer, dimension(nbNode)                :: tmpbeg, tmpend
        integer, parameter :: bufflen=1024
        character(len=bufflen) :: buffer
        character(len=:),allocatable  :: s
        integer :: io,  isize, i, nbWord, starti, endi
        character(3)                              :: word
        logical                                   :: eof
        type(node),dimension(nbNode)              :: mapper
        map = 0
        s = ''
        read(u,'(a)',advance='no', iostat=io, size=isize) buffer
        if(isize.gt.0)then 
            s = s//buffer(:isize)
        endif
        path = s
        read(u,'(a)',advance='no', iostat=io, size=isize) buffer
        eof = io == iostat_end
        nbWord = 0
        ! Fill the 'mapper' : what is needed to fill the graph
        do while (.not. eof)
            s=''
            read(u,'(a)',advance='no', iostat=io, size=isize) buffer
            if (isize.gt.0)then
                s=s//buffer(:isize)
            endif
            eof = io == iostat_end
            if (eof) exit
            i = 1
            word =''
            call getWrd(s,word,i)
            nbWord = nbWord + 1
            mapper(nbWord)%name= word 
            call nextWd(s,i)            
            call getWrd(s,word,i)
            mapper(nbWord)%L = word
            call nextWd(s,i)
            call getWrd(s,word,i)
            mapper(nbWord)%R = word
        enddo
        ! Fill the graph from the mapper
        starti = 1
        endi = 1
        do i=1, nbWord
            if (mapper(i)%name == 'AAA') then
                tmpbeg(1) = i
            else if (mapper(i)%name(3:3) == 'A') then
                starti = starti + 1
                tmpbeg(starti) = i
            else if (mapper(i)%name == 'ZZZ') then
                tmpend(1) = i
            else if (mapper(i)%name(3:3) == 'Z') then
                endi = endi + 1
                tmpend(endi) = i
            endif
            map(i,1) = findI(mapper, mapper(i)%L)
            map(i,2) = findI(mapper, mapper(i)%R)
        enddo
        begin = tmpbeg(:starti)
        end = tmpend(:endi)
endsubroutine

subroutine part1()
        integer                           :: i, j
        character(len=:),allocatable      :: path
        integer,dimension(nbNode, 2)      :: map
        integer(kind=LargeInt_K)          :: res
        integer,dimension(:), allocatable :: begin, end

        open(UNIT=1, file=input, status='old')
        call parse(1, path, map, begin, end)

        res = 0
        i = begin(1)
        j = 1
        do while(i /= end(1))
            if (j == len(path) + 1) j = 1
            if (path(j:j) == 'L') then 
                i = map(i,1)
            else
                i = map(i,2)
            endif
            res = res + 1
            j = j + 1
        enddo
        close(1)
        print*,'res = ',res
endsubroutine

function isEnd(index, end) result(ended)
    integer, intent(in)                            :: index
    integer, dimension(:), allocatable, intent(in) :: end
    logical                                        :: ended
    integer                                        :: i
    ended = .false.
    do i = 1, size(end)
        if (end(i) == index) then
            ended = .true.
            exit
        endif
    enddo
    
endfunction

function hcf(x, y) result(res)
    integer(kind=LargeInt_K),intent(in) :: x,y
    integer(kind=LargeInt_K)            :: res, smaller, i
    
    if (x > y) then
        smaller = y
    else
        smaller = x
    endif
    do i = 1, smaller
        if((mod(x,i) == 0).and.(mod(y,i) == 0)) res = i
    enddo
endfunction
        

subroutine part2()
        integer                           :: i, j, k
        character(len=:),allocatable      :: path
        integer,dimension(nbNode, 2)      :: map
        integer(kind=LargeInt_K)          :: res
        integer(kind=LargeInt_K),dimension(:),allocatable :: allRes
        integer,dimension(:), allocatable :: begin, end

        open(UNIT=1, file=input, status='old')
        call parse(1, path, map, begin, end)

        allocate(allRes(size(begin)))
        allRes = 1
        do i = 1, size(begin)
            res = 0
            j = begin(i)
            k = 1
            do while(.not.isEnd(j, end))
                if (k == len(path) + 1) k = 1
                if (path(k:k) == 'L') then 
                    j = map(j,1)
                else
                    j = map(j,2)
                endif
                res = res + 1
                k = k + 1
            enddo
            allRes(i) = res
        enddo
        res = 1
        do i = 1, size(allRes)
            res = res * allRes(i) / hcf(res,allRes(i))
        enddo
        deallocate(allRes)
        close(1)
        print*,'res = ',res
endsubroutine

endmodule

program main
    use m

    print *,''
    print *,'Begin AOC main Day 8'
    print *,''

    input='example3.txt'
    print*, 'Part 1'
    !call part1()
    print*, 'Part 2'
    call part2()

    print *,''
    print *,'Ending AOC main Day 8'
    print *,''
endprogram
