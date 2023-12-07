module m
    use iso_fortran_env
    implicit none
    character(len=:), allocatable :: input

contains 

function chrVal(c,p) result (val)
    character, intent(in) :: c
    integer,intent(in)    :: p
    integer :: val
    select case(c)
    case('A')
        val = 14
    case('K')
        val = 13
    case('Q')
        val = 12
    case('J')
        if (p== 1)then
        val = 11
        else
            val = 1
        endif
    case('T')
        val = 10
    case('9')
        val = 9
    case('8')
        val = 8
    case('7')
        val = 7
    case('6')
        val = 6
    case('5')
        val = 5
    case('4')
        val = 4
    case('3')
        val = 3
    case('2')
        val = 2
    case default
        val = 15
    endselect
endfunction

function cmpChr(c1,c2,p) result(res)
    character, intent(in) :: c1,c2
    integer,intent(in)    :: p
    integer :: res, val1, val2
    val1 = chrVal(c1,p)
    val2 = chrVal(c2,p)
    if (val1 > val2) then
        res = 1
    else if (val1 < val2) then
        res = -1
    else
        res = 0
    endif
endfunction

function cmpHnd(str1,str2,p) result(res)
    character(6), intent(in) :: str1,str2
    integer,intent(in)       :: p
    integer :: res, i
    res = 0
    do i = 1, 6
        res = cmpChr(str1(i:i), str2(i:i),p)
        if (res /= 0) exit
    enddo
endfunction

function chrHand(curMax, scnMax) result(res)
    integer,intent(in) :: curMax, scnMax
    character          :: res
    if (curMax== 5) then
        res = 'T'
    else if (curMax== 4) then
        res = '9'
    else if (curMax== 3) then
        if (scnMax == 2) then
            res = '8'
        else
            res = '7'
        endif
    else if (curMax == 2) then
        if (scnMax == 2) then
            res = '6'
        else
            res = '5'
        endif
    else
        res = '4'
    endif
endfunction


function typHnd(str,p) result(res)
    character(5), intent(in) :: str
    integer,intent(in)       :: p
    character(:), allocatable:: hist
    character :: res
    integer   :: curMax, scnMax, curr, i, j, tmp, nbJ
    curMax = 0
    scnMax = 0
    hist = ''
    nbJ = 0
    do i = 1, 5
        curr = 1
        if (str(i:i) == 'J'.and.p == 2) nbJ = nbJ + 1
        if ((str(i:i)/= 'J'.or.p==1) .and. index(hist,str(i:i)) == 0) then
            hist = hist // str(i:i)
            j = index(str(i + 1:),str(i:i))
            tmp = i
            do while(j /= 0)
                j = j + tmp
                curr = curr + 1
                tmp = j 
                j = index(str(tmp + 1:),str(i:i))
            enddo
            if (curr > curMax)then
                scnMax = curMax
                curMax = curr
            else if (curr  > scnMax) then
                scnMax = curr
            endif
        endif
    enddo
    curMax = curMax + nbJ
    res = chrHand(curMax,scnMax)
endfunction

subroutine getHnd(str,res,p)
    character(:),allocatable, intent(in)  :: str
    character(6), intent(out)             :: res
    integer,intent(in)                    :: p
    read(str(1:5),*) res
    res = typHnd(res,p)//res
endsubroutine


subroutine getNum(str, int_res, i)
    character(:), allocatable, intent(in) :: str
    integer, intent(inout)                :: i
    integer, parameter :: LargeInt_K = selected_int_kind(32)
    integer(kind=LargeInt_K), intent(out)                  :: int_res
    integer                               :: nb
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

subroutine part(p)
        integer, intent(in)         :: p
        integer, parameter :: bufflen=1024
        integer, parameter :: LargeInt_K = selected_int_kind(32)
        integer(kind=LargeInt_K)      :: bid,res, tmpbid
        integer :: io,  isize, nbHand, i,j
        character(len=:),allocatable  :: s
        character(len=bufflen) :: buffer
        character(6)                  :: hand, tmphnd
        character(6), dimension(1000) :: hands
        integer(kind=LargeInt_K), dimension(1000)      :: bids
        logical                            ::  eof
        print *,'input file = ', input

        open(UNIT=1, file=input, status='old')

        eof=.false.
        nbHand = 0
        hands = ''
        bids = -1
        res = 0
        do while(.not. eof)
            !Lexing part
            s = ''
            read(1,'(a)',advance='no', iostat=io, size=isize) buffer
            if(isize.gt.0)then 
                s = s//buffer(:isize)
            endif
            eof = io == iostat_end
            if (eof) exit

            hand = ''
            bid = 0

            call getHnd(s, hand,p)
            nbHand = nbHand + 1

            i = 6
            call nextNb(s,i)
            call getNum(s,bid,i)

            ! insert at the right place the pair hand/bid
            do j = 1, nbHand
                if (cmpHnd(hand, hands(j),p) < 0) then
                    tmphnd = hands(j)
                    tmpbid = bids(j)
                    hands(j) = hand
                    bids(j) = bid
                    hand = tmphnd
                    bid = tmpbid
                endif
            enddo
        enddo

        do j = 1, nbHand
            res = res + bids(j) * j
        enddo
        print*, 'res = ', res
        close(1)
endsubroutine

endmodule

program main
    use m

    print *,''
    print *,'Begin AOC main Day 7'
    print *,''

    input='input.txt'
    print*, 'Part 1'
    call part(1)
    print*, 'Part 2'
    call part(2)

    print *,''
    print *,'Ending AOC main Day 7'
    print *,''
endprogram
