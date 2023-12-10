module m
    use queueL
    use iso_fortran_env
    implicit none
    integer, parameter                :: width = 5
    integer, parameter                :: height = 5
    character(len=:), allocatable     :: input
    type node
        character                     :: c
        integer                       :: nbChld
        integer,dimension(:)          :: child 
    endtype
        

contains 

subroutine parse(u, grid, beginX, beginY)
    integer, intent(in)                                :: u
    integer, intent(out)                               :: beginX, beginY
    type(node), dimension(width, height), intent(inout):: grid
    integer, parameter                                 :: bufflen=1024
    character(len=bufflen)                             :: buffer
    character(len=:),allocatable                       :: s
    integer                                            :: io, isize, i, j
    logical                                            :: eof
    eof = .false.
    j = 0
    do while (.not.eof)
        j = j + 1
        s=''
        read(u,'(a)',advance='no', iostat=io, size=isize) buffer
        if (isize.gt.0) then
            s=s//buffer(:isize)
        endif
        eof = io == iostat_end
        i = 0
        do while (i < len(s))
            i = i + 1
            grid(i, j)%c = s(i:i)
            if (s(i:i) == 'S') then
                beginX = i
                beginY = j
            endif
        enddo
    enddo
endsubroutine

! ind value (with C the current x,y values)
! .1.
! 4C2
! .3.
subroutine push(qX, qY, x, y)
    type(queue), intent(inout)                      :: qX, qY
    integer(kind=LargeInt_Vect)                     :: x, y
    call pushQu(qY, y)
    call pushQu(qX, x)
endsubroutine

subroutine move(qX, qY, x, y, ind)
    type(queue), intent(inout)                      :: qX, qY
    integer(kind=LargeInt_Vect), intent(in)         :: x, y
    integer(kind=LargeInt_Vect)                     :: newX, newY
    integer, intent(in)                             :: ind
    newX = x
    newY = y
    if ((ind == 1.or.ind==5).and. newY /= 1) then
        newY = newY - 1
        call push(qX,qY,newX, newY)
        newY = y
    endif
    if ((ind == 2.or.ind==5).and.newX/=width) then
        newX = newX + 1
        call push(qX,qY, newX, newY)
        newX = x
    endif
    if ((ind == 3.or.ind==6).and.newY/=height) then
        newY = newY + 1
        call push(qX, qY, newX, newY)
        newY = y
    endif
    if ((ind == 4.or.ind==6).and.newX/=1) then
        newX = newX - 1
        call push(qX,qY, newX, newY)
        newX = x
    endif
endsubroutine

subroutine slctMv(c, ind1, ind2, prvX, prvY)
    character, intent(in)                           :: c
    integer(kind=LargeInt_Vect), intent(int)        :: prvX, prvY
    integer, intent(out)                            :: ind1, ind2
    ind1 = 0
    ind2 = 0
    if (c == '|') then
        ind1 = 1
        ind2 = 3
    else if (c == '-') then
        ind1 = 4
        ind2 = 2
    else if (c == 'L') then
        ind1 = 1
        ind2 = 2
    else if (c == 'J') then
        ind1 = 1
        ind2 = 4
    else if (c == '7') then
        ind1 = 4
        ind2 = 3
    else if (c == 'F') then
        ind1 = 2
        ind2 = 3
    else if (c == 'S') then
        ind1 = 5
        ind2 = 6
    endif
endsubroutine


subroutine rabHol(grid, dist, beginX, beginY)
    character, dimension(width, height), intent(in) :: grid
    integer, intent(out)                            :: dist
    integer, intent(in)                             :: beginX, beginY
    logical, dimension(width, height)               :: path
    type(queue)                                     :: qX, qY
    integer(kind=LargeInt_Vect)                     :: x, y, flag
    integer                                         :: ind1, ind2
    path = .false.
    x = beginX
    y = beginY
    flag = 25
    call initQu(qX, flag)
    call initQu(qY, flag)
    flag = 0
    call push(qX, qY, x, y)
    call push(qX, qY, flag, flag)
    dist = 0
    x = peekQu(qX)
    y = peekQu(qY)
    call popQu(qX)
    call popQu(qY)
    do while (.true.)
        if (x == 0) then
            dist = dist + 1
            call push(qX, qY, flag, flag)
        else if (path(x,y)) then 
            exit
        else
            path(x,y) = .true.
            call slctMv(grid(x,y), ind1, ind2)
            if (ind1 /= 0) then
                call move(qX, qY, x, y, ind1)
                call move(qX, qY, x, y, ind2)
            endif
        endif
        x = peekQu(qX)
        y = peekQu(qY)
        call popQu(qX)
        call popQu(qY)
    enddo
endsubroutine

subroutine part1()
    character, dimension(width, height)                :: grid
    integer                                            :: res, beginX, beginY
    open(UNIT=1, file=input, status='old')
    grid = '.'
    call parse(1, grid, beginX, beginY)
    call rabHol(grid, res, beginX, beginY)
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
    call part1()
    print*, 'Part 2'
    !call part(2)

    print *,''
    print *,'Ending AOC main Day 9'
    print *,''
endprogram
