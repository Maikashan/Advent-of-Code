module m
    use iso_fortran_env
    implicit none
    character(len=:), allocatable :: input

contains 


    subroutine getNum(str, int_res, i)
        character(:), allocatable, intent(in) :: str
        integer, intent(inout)                :: i
        integer, intent(out)                  :: int_res
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

    function checkS(prevS,j,i) result(ctnS)
        character, dimension(0:1023), intent(in)  :: prevS 
        integer, intent(in)                       :: j,i
        logical                                   :: ctnS
        integer                                   :: k

        ctnS =  .false.

        do k = j, i
            if (prevS(k) /= '.') then
                ctnS = .true.
                exit
            endif
        enddo
    endfunction
                
    subroutine rmvNb(prevN, i)
        integer, intent(in)  :: i
        integer, dimension(0:1023), intent(inout) :: prevN
        integer              :: j


        j = i
        do while (prevN(j) /= 0)
            prevN(j) = 0
            j = j - 1
        enddo
        j = i + 1
        do while (prevN(j) /= 0)
            prevN(j) = 0
            j = j + 1
        enddo
    endsubroutine

    subroutine part1()
        integer, parameter :: bufflen=1024
        integer :: io,  isize, res, i, nb, j
        character(len=:),allocatable  :: s
        character, dimension(0:1023)  :: prevS, buildS 
        character(len=bufflen) :: buffer
        integer, dimension(0:1023)    :: prevN, buildN
        logical                            ::  eof

        print *,'input file = ', input

        open(UNIT=1, file=input, status='old')

        eof=.false.
        res = 0
        prevS(:) = '.'
        prevN(:) = 0
        do while(.not. eof)
            !Lexing part
            s = ''
            read(1,'(a)',advance='no', iostat=io, size=isize) buffer
            if(isize.gt.0)then 
                s = s//buffer(:isize)
            endif
            eof = io == iostat_end
            if (eof) exit
            buildN(:) = 0
            buildS(:) = '.'


            !print*, 's  = ', s

            !Parsing part
            !Get current Id
            i = 1
            do while (i <= len(s))
                if (s(i:i) >=  '0' .and. s(i:i) <= '9') then
                    j = i
                    call getNum(s, nb, i)
                    ! checking left, on top, and right for a symbol
                    if (buildS(j - 1) /= '.' .or. checkS(prevS, j - 1,i) .or. (i <= len(s) .and. s(i:i) /= '.')) then
                        res = res + nb
                    else
                        ! else, we stock them
                        buildN(j:i - 1) = nb
                    endif
                else
                    if (s(i:i) /= '.') then
                        ! we stock the sym
                        buildS(i) = s(i:i)
                        do j = i - 1, i + 1
                            !we chec for numbers on top, add them and we remove the ones we added
                            if (prevN(j) /= 0) then
                                res = res + prevN(j)
                                call rmvNb(prevN, j)
                            endif
                        enddo
                    endif
                    i = i + 1
                endif
            enddo
            prevS = buildS
            prevN = buildN
        end do 
        print*, 'res = ', res
        close(1)
    endsubroutine


    subroutine part2()
        integer, parameter :: bufflen=1024
        integer :: io,  isize, res, i, nb, j, nbsym, nbVal, gearV, k
        character(len=:),allocatable  :: s
        character(len=bufflen) :: buffer
        integer, dimension(19600)  :: symsX
        integer, dimension(19600)  :: symsY
        integer, dimension(141,141)   :: nums
        logical                ::  eof

        print *,'input file = ', input

        open(UNIT=1, file=input, status='old')

        eof=.false.
        res = 0
        symsX(:) = 0
        symsY(:) = 0
        nums(:, :) = 0
        nbsym = 0
            j = 1
        do while(.not. eof)
            !Lexing part
            s = ''
            read(1,'(a)',advance='no', iostat=io, size=isize) buffer
            if(isize.gt.0)then 
                s = s//buffer(:isize)
            endif
            eof = io == iostat_end
            if (eof) exit

            !Parsing part
            !Get current Id
            i = 1
            do while (i <= len(s))
                if (s(i:i) >=  '0' .and. s(i:i) <= '9') then
                    k = i
                    call getNum(s, nb, i)
                    nums(k:i - 1, j) = nb
                else
                    if (s(i:i) == '*') then
                        nbsym = nbsym + 1
                        ! we stock the sym
                        symsX(nbsym) = i
                        symsY(nbsym) = j
                    endif
                    i = i + 1
                endif
            enddo
                j = j + 1
        end do 
addG:   do i = 1, nbsym
            nbVal = 0
            gearV = 1
col:        do j = max(symsY(i) - 1,0), min(symsY(i) + 1, 140)
                do k = max(symsX(i) - 1,0), min(symsX(i) + 1, 140)
                    if (nums(k,j) /= 0) then 
                        nbVal = nbVal + 1
                        if (nbVal > 2) cycle addG
                            gearV = nums(k,j) * gearV
                        !Don't continue if the number is at the middle 
                        if (nums(symsX(i),j) /= 0) cycle col
                    endif
                enddo
            enddo col
            if (nbVal == 2) then 
                res = res + gearV
            endif
        enddo addG
        print*, 'res = ', res
        close(1)
    endsubroutine

endmodule

program main
    use m

    print *,''
    print *,'Begin AOC main Day 1'
    print *,''

    input='input.txt'
    !call part1()
    call part2()

    print *,''
    print *,'Ending AOC main Day 1'
    print *,''
endprogram
