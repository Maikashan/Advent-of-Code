module m
    use queueL
    use iso_fortran_env
    implicit none
    integer, parameter                :: width = 141
    integer, parameter                :: height = 141
    character(len=:), allocatable     :: input
    type coord
        integer(kind=LargeInt_Vect)     :: x = 0
        integer(kind=LargeInt_Vect)     :: y = 0
    endtype
    type child
        type(coord)                     ::  child1
        type(coord)                     ::  child2
        type(coord)                     ::  child3
        type(coord)                     ::  child4
    endtype
contains 

subroutine slctMv(c, ind1, ind2)
    character, intent(in)                           :: c
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

! ind value (with C the current x,y values)
! .1.
! 4C2
! .3.
subroutine neigh(grid, x, y, ind, nbChld)
    type(child), dimension(width, height),intent(inout) :: grid
    integer(kind=LargeInt_Vect), intent(in)             :: x, y
    integer(kind=LargeInt_Vect)                         :: newX, newY
    integer, intent(in)                                 :: ind, nbChld
    newX = x
    newY = y
    if ((ind == 1.or.ind == 5).and. newY /= 1) then
        newY = newY - 1
        if (nbChld == 1.or.ind == 5) then
            grid(x,y)%child1%x = newX
            grid(x,y)%child1%y = newY
        else
            grid(x,y)%child2%x = newX
            grid(x,y)%child2%y = newY
        endif
        newY = y
    endif
    if ((ind == 2.or.ind==5).and.newX/=width) then
        newX = newX + 1
        if (nbChld == 1.and.ind/=5) then
            grid(x,y)%child1%x = newX
            grid(x,y)%child1%y = newY
        else
            grid(x,y)%child2%x = newX
            grid(x,y)%child2%y = newY
        endif
        newX = x
    endif
    if ((ind == 3.or.ind==6).and.newY/=height) then
        newY = newY + 1
        if (ind==6) then
            grid(x,y)%child3%x = newX
            grid(x,y)%child3%y = newY
        else if (nbChld == 1) then
            grid(x,y)%child1%x = newX
            grid(x,y)%child1%y = newY
        else
            grid(x,y)%child2%x = newX
            grid(x,y)%child2%y = newY
        endif
        newY = y
    endif
    if ((ind == 4.or.ind==6).and.newX/=1) then
        newX = newX - 1
        if (ind == 6) then
            grid(x,y)%child4%x = newX
            grid(x,y)%child4%y = newY
        else if (nbChld == 1) then
            grid(x,y)%child1%x = newX
            grid(x,y)%child1%y = newY
        else
            grid(x,y)%child2%x = newX
            grid(x,y)%child2%y = newY
        endif
        newX = x
    endif
endsubroutine

subroutine stChld(c, x, y, grid)
    character,intent(in)                                 :: c
    integer(kind=LargeInt_Vect), intent(in)              :: x, y
    type(child), dimension(width, height), intent(inout) :: grid
    integer                                              :: ind1, ind2
    call slctMv(c, ind1, ind2)
    call neigh(grid, x, y, ind1, 1)
    call neigh(grid, x, y, ind2, 2)
endsubroutine


subroutine parse(u, grid, beginX, beginY, gridC)
    integer, intent(in)                                :: u
    integer, intent(out)                               :: beginX, beginY
    type(child), dimension(width, height), intent(inout) :: grid
    character, dimension(width, height), intent(inout) :: gridC
    integer, parameter                                 :: bufflen=1024
    character(len=bufflen)                             :: buffer
    character(len=:),allocatable                       :: s
    integer                                            :: io, isize, i, j
    integer(kind=LargeInt_Vect)                        :: x, y
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
            x = i
            y = j
            call stChld(s(i:i), x, y, grid)
            gridC(i,j) = s(i:i)
            if (s(i:i) == 'S') then
                beginX = i
                beginY = j
            endif
        enddo
    enddo
endsubroutine

subroutine push(qX, qY, x, y)
    type(queue), intent(inout)                      :: qX, qY
    integer(kind=LargeInt_Vect)                     :: x, y
    call pushQu(qY, y)
    call pushQu(qX, x)
endsubroutine

function cncted(grid, x, y, coo) result(res)
    type(child), dimension(width, height), intent(inout) :: grid
    type(coord), intent(inout)                         :: coo
    integer(kind=LargeInt_Vect)                        :: x,y
    logical                                            :: res
    type(coord)                                        :: c1, c2
    res = .false.
    c1 = grid(coo%x, coo%y)%child1
    c2 = grid(coo%x, coo%y)%child2
    if (c1%x == x.and. y == c1%y) then
        res= .true.
    else if (c2%x == x.and.c2%y == y) then
        res= .true.
    endif
endfunction

function move(grid,qX, qY, x, y, path) result(res)
    type(child), dimension(width, height), intent(inout) :: grid
    type(queue), intent(inout)                           :: qX, qY
    integer(kind=LargeInt_Vect), intent(in)              :: x, y
    type(coord), dimension(width, height), intent(inout) :: path
    integer                                              :: res
    type(coord)                                          :: curr, c
    curr%x = x
    curr%y = y
    res = 0
    c = grid(x,y)%child1
    if (cncted(grid, x, y, c)) then
        if (path(c%x,c%y)%x /= 0) then
            res = res + 1
        else
            path(c%x, c%y) = curr
            call push(qX, qY, c%x, c%y)
        endif
    endif

    c = grid(x,y)%child2
    if (cncted(grid, x, y, c)) then
        if (path(c%x,c%y)%x /= 0) then
            res = res + 1
        else
            path(c%x, c%y) = curr
            call push(qX, qY, c%x, c%y)
        endif
    endif

    c = grid(x,y)%child3
    if (c%x /= 0) then
        if (cncted(grid, x, y , c)) then
            if (path(c%x,c%y)%x /= 0) then
                res = res + 1
            else
                path(c%x, c%y) = curr
                call push(qX, qY, c%x, c%y)
            endif
        endif
        c = grid(x,y)%child4
        if (cncted(grid, x, y, c)) then
            if (path(c%x,c%y)%x /= 0) then
                res = res + 1
            else
                path(c%x, c%y) = curr
                call push(qX, qY, c%x, c%y)
            endif
        endif
    endif

endfunction



subroutine rabHol(grid, dist, beginX, beginY)
    type(child), dimension(width, height), intent(inout) :: grid
    integer, intent(out)                            :: dist
    integer, intent(in)                             :: beginX, beginY
    type(coord), dimension(width, height)           :: path
    type(queue)                                     :: qX, qY
    integer(kind=LargeInt_Vect)                     :: x, y, flag
    integer                                         :: val
    x = beginX
    y = beginY
    path(x,y)%x = x
    path(x,y)%y = y

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
        else
            val = move(grid, qX, qY, x, y, path)
            if (val == 2) exit
        endif
        x = peekQu(qX)
        y = peekQu(qY)
        call popQu(qX)
        call popQu(qY)
    enddo
    dist = dist + 1
endsubroutine

subroutine rabNst(grid, beginX, beginY)
    type(child), dimension(width, height), intent(inout) :: grid
    integer, intent(inout)                          :: beginX, beginY
    type(coord), dimension(width, height)           :: path
    type(queue)                                     :: qX, qY
    integer(kind=LargeInt_Vect)                     :: x, y, flag
    integer                                         :: val
    x = beginX
    y = beginY
    path(x,y)%x = x
    path(x,y)%y = y

    flag = 25
    call initQu(qX, flag)
    call initQu(qY, flag)
    flag = 0
    call push(qX, qY, x, y)
    call push(qX, qY, flag, flag)
    x = peekQu(qX)
    y = peekQu(qY)
    call popQu(qX)
    call popQu(qY)
    do while (.true.)
        if (x == 0) then
            call push(qX, qY, flag, flag)
        else
            val = move(grid, qX, qY, x, y, path)
            if (val == 2) exit
        endif
        x = peekQu(qX)
        y = peekQu(qY)
        call popQu(qX)
        call popQu(qY)
    enddo
    beginX = x
    beginY = y
endsubroutine

RECURSIVE subroutine goStrt(grid, coo, prev, loopX, loopY)
    type(child), dimension(width, height), intent(inout) :: grid
    type(coord), intent(in)                              :: prev
    type(vector), intent(inout)                          :: loopX, loopY
    type(coord), intent(inout)                           :: coo
    type(coord)                                          :: c
    if (grid(coo%x, coo%y)%child3%x == 0) then
        call addVec(loopX, coo%x)
        call addVec(loopY, coo%y)
        c = grid(coo%x, coo%y)%child1
        if (c%x == prev%x .and. c%y == prev%y) then
            c = grid(coo%x, coo%y)%child2
        endif
        call goStrt(grid, c, coo, loopX, loopY)
    endif
endsubroutine

subroutine part1()
    type(child), dimension(width, height)              :: grid
    character, dimension(width, height)                :: gridC
    integer                                            :: res, beginX, beginY
    open(UNIT=1, file=input, status='old')
    call parse(1, grid, beginX, beginY, gridC)
    call rabHol(grid, res, beginX, beginY)
    close(1)
    print*,'res = ',res
endsubroutine

subroutine sortYX(loopX, loopY)
    type(vector), intent(inout)                        :: loopX, loopY
    integer(kind=LargeInt_Vect)                        :: i, j, valX, valY
    logical                                            :: check1, check2, check
    do i = 1, loopY%curSiz
        valY = atVect(loopY,i)
        valX = atVect(loopX, i)
        j = i - 1
        check1 = atVect(loopY,j) >valY
        check2 = atVect(loopY,j)==valY.and.atVect(loopX,j)>valX
        check = check1.or.check2
        do while(j > 0.and.check)
            loopX%vect(j + 1) = loopX%vect(j)
            loopY%vect(j + 1) = loopY%vect(j)
            j = j - 1
            check1 = atVect(loopY,j)>valY
            check2 = atVect(loopY,j)==valY.and.atVect(loopX,j)>valX
            check = check1.or.check2
        enddo
        loopX%vect(j + 1) = valX
        loopY%vect(j + 1) = valY
    enddo
endsubroutine

function isBdry(c, loopX, loopY, i, j) result (res)
  character, intent(in)                          :: c
  type(vector),intent(inout)                     :: loopX, loopY
  integer(kind=LargeInt_Vect),intent(in)      :: i, j
  integer(kind=LargeInt_Vect)                    :: x, y, first, last
  logical                                        :: res, check
  res = .false.
  check = .false.
  first = 0
  last = 0
  do x = 1, loopY%curSiz
      if (loopY%vect(x) == j) then
          last = x
          if (first == 0) first = x
      else if (last /= 0) then
        exit
      endif
  enddo
  do x = first, last
      if (loopX%vect(x) == i) then
          check= .true.
          exit
      endif
  enddo
    if (check) then
      if (c=='-'.or.c=='|'.or.c=='L'.or.c=='F'.or.c=='7'.or.c=='J'.or.c=='S') then
          res = .true.
      endif
  endif
endfunction

function upOrDn(c) result(val)
    character,intent(in)                           :: c
    integer                                        :: val
    if (c == 'L'.or.c=='J') then
        val = 1
    else
        val = 2
    endif
endfunction


function rtOrLf(c) result(val)
    character,intent(in)                           :: c
    integer                                        :: val
    if (c == '7'.or.c=='J') then
        val = 1
    else
        val = 2
    endif
endfunction


subroutine horChk(loopX, loopY, horX, horY, gridC)
  type(vector),intent(inout)                     :: loopX, loopY, horX, horY
  character, dimension(width, height), intent(in):: gridC
  logical                                        :: inside, bdry
  integer(kind=LargeInt_Vect)                    :: i, j
  integer                                        :: prev
  do j = minval(loopX%vect), maxval(loopX%vect)
      inside = .false.
      prev = 0
      do i = loopY%vect(1), loopY%vect(loopY%curSiz)
          bdry = isBdry(gridC(j,i),loopX,loopY,j,i)
          if (bdry) then
              if (gridC(j,i)=='-') then
                  inside = .not.inside
              else if (gridC(j,i)/='|')then
                  if (prev==0) then
                      prev = rtOrLf(gridC(j,i))
                      inside = .not.inside
                  else if (prev == rtOrLf(gridC(j,i))) then
                      inside = .not.inside
                      prev=0
                  else
                      prev=0
                  endif
              endif
          else if (inside) then
                call addVec(horX, j)
                call addVec(horY, i)
          endif
      enddo
  enddo
  call sortYX(horX, horY)
endsubroutine

subroutine verChk(loopX, loopY, verX, verY, gridC)
  type(vector),intent(inout)                     :: loopX, loopY, verX, verY
  character, dimension(width, height), intent(in):: gridC
  logical                                        :: inside, bdry
  integer(kind=LargeInt_Vect)                    :: i, j
  integer                                        :: prev
  do i = loopY%vect(1), loopY%vect(loopY%curSiz)
      inside = .false.
      prev = 0
      do j = minval(loopX%vect), maxval(loopX%vect)
      bdry = isBdry(gridC(j,i), loopX, loopY, j, i)
          if (bdry) then
              if (gridC(j,i)=='|') then
                  inside = .not.inside
              else if (gridC(j,i)/='-')then
                  if (prev==0) then
                      prev = upOrDn(gridC(j,i))
                      inside = .not.inside
                  else if (prev == upOrDn(gridC(j,i))) then
                      inside = .not.inside
                      prev=0
                  else
                      prev=0
                  endif
              endif
          else if(inside) then
              call addVec(verY, i)
              call addVec(verX, j)
          endif
      enddo
  enddo
  call sortYX(verX, verY)
endsubroutine

subroutine morphS(gridC, S, loopX, loopY, beginX, beginY)
    type(child),intent(inout)                          :: S
    character, dimension(width, height),intent(inout)  :: gridC
    type(vector),intent(inout)                         :: loopX, loopY
    type(coord)                                        :: c1, c2, c3, c4
    integer(kind=LargeInt_Vect), intent(inout)         :: beginX, beginY
    integer                                            :: val1, val2
    c1 = S%child1
    c2 = S%child2
    c3 = S%child3
    c4 = S%child4
    val1 = 0
    if (isBdry(gridC(c1%x,c1%y), loopX, loopY, c1%x, c1%y)) then
        val1 = 1
    endif
    if (isBdry(gridC(c2%x,c2%y), loopX, loopY, c2%x, c2%y)) then
        if (val1 == 0) then
            val1 = 2
        else
            val2 = 2
        endif
    endif
    if (isBdry(gridC(c3%x,c3%y), loopX, loopY, c3%x, c3%y)) then
        if (val1 ==0) then
            val1 = 3
        else
            val2 = 3
        endif
    endif
    if (isBdry(gridC(c3%x,c3%y), loopX, loopY, c3%x, c3%y)) then
        val2 = 4
    endif
    if (val1 == 1) then
        if (val2 == 2) then
            gridC(beginX, beginY) = 'L'
        else if (val2 == 3) then
            gridC(beginX, beginY) = '|'
        else
            gridC(beginX, beginY) = 'J'
        endif
    else if (val1==2) then
        if (val2 == 3) then
            gridC(beginX, beginY) = 'F'
        else
            gridC(beginX, beginY) = '-'
        endif
    else
        gridC(beginX,beginY) = '7'
    endif
endsubroutine

subroutine part2()
    type(child), dimension(width, height)              :: grid
    character, dimension(width, height)                :: gridC
    integer                                            :: res, beginX, beginY
    type(vector)                                       :: loopX, loopY
    type(vector)                                       :: verX, verY, horX, horY 
    integer(kind=LargeInt_Vect)                        :: siz, tmpX, tmpY, i, j
    type(coord)                                        :: coo
    logical                                            :: inside
    open(UNIT=1, file=input, status='old')
    siz = 25
    res = 0
    call parse(1, grid, beginX, beginY, gridC)
    call initVc(loopX, siz)
    call initVc(loopY, siz)
    tmpX = beginX
    call addVec(loopX, tmpX)
    tmpY = beginY
    call addVec(loopY, tmpY)
    call rabNst(grid, beginX, beginY)
    coo%x = beginX
    call addVec(loopX, coo%x)
    coo%y = beginY
    call addVec(loopY, coo%y)
    call goStrt(grid, grid(beginX, beginY)%child1, coo, loopX, loopY)
    call goStrt(grid, grid(beginX, beginY)%child2, coo, loopX, loopY)
    call sortYX(loopX, loopY)
    call morphS(gridC,grid(tmpX,tmpY), loopX, loopY, tmpX, tmpY)

    call initVc(verX, loopX%curSiz)
    call initVc(verY, loopX%curSiz)
    call initVc(horX, loopX%curSiz)
    call initVc(horY, loopX%curSiz)

    call horChk(loopX, loopY, horX, horY, gridC)
    call verChk(loopX, loopY, verX, verY, gridC)

    i = 1
    j = 1
    do while (i <= horX%curSiz.and.j<=verX%curSiz)
        if (atVect(horY,i) < atVect(verY,j)) then
            i = i + 1
        else if (atVect(horY,i) > atVect(verY,j)) then
            j = j + 1
        else if (atVect(horX,i) < atVect(verX,j)) then
            i = i + 1
        else if (atVect(horX,i) > atVect(verX,j)) then
            j = j + 1
        else
            res = res + 1
            i = i + 1
            j = j + 1
        endif
    enddo
    
    close(1)
    print*, 'res = ', res
endsubroutine


endmodule

program main
    use m

    print *,''
    print *,'Begin AOC main Day 10'
    print *,''

    input='input.txt'
    print*, 'Part 1'
    !call part1()
    print*, 'Part 2'
    call part2()

    print *,''
    print *,'Ending AOC main Day 10'
    print *,''
endprogram
