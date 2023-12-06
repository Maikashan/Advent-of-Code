module m
    use iso_fortran_env
    implicit none
    character(len=:), allocatable :: input

contains 


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

    subroutine gNKern(str, int_res, i)
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

function cptRan(time, record) result(res)
    integer, parameter :: LargeInt_K = selected_int_kind(32)
    integer(kind=LargeInt_K), intent(in) :: time, record
    integer             :: res
    integer, parameter :: LargeReal_K = selected_real_kind(32)
    integer(kind=LargeInt_K) :: timeSq
    real (kind=LargeReal_K):: delta, x1, x2
    res = 0
    timeSq = time * time
    delta = timeSq - (4 * record)
    if (delta == 0) then
        res = 1
    else if (delta > 0) then
        x1 = (-time - sqrt(delta))/(-2)
        x2 = (-time + sqrt(delta))/(-2)
        if (floor(x1) == x1) x1 = x1 - 1
        if (ceiling(x2) == x2) x2 = x2 + 1
        res = floor(x1) - ceiling(x2) + 1
    else
        res = 0
    endif
endfunction

subroutine parse(u, time, record, j, nbpart)
        integer, parameter :: bufflen=1024
        integer, intent(in) :: u, nbpart
        integer, intent(out) :: j
        integer :: io,  isize, i
        character(len=:),allocatable  :: s
        character(len=bufflen) :: buffer
        integer, parameter :: LargeInt_K = selected_int_kind(32)
        integer(kind=LargeInt_K) ::nb
        integer(kind=LargeInt_K), dimension(4), intent(inout) :: time, record
        record = -1
        time = -1
        j = 0
        s = ''
        read(u,'(a)',advance='no', iostat=io, size=isize) buffer
        if(isize.gt.0)then 
            s = s//buffer(:isize)
        endif

        i = 1 
        do while (i < len(s))
            nb = -1
            j = j + 1
            call nextNb(s,i)
            if (nbpart == 1)then 
                call getNum(s, nb, i)
            else 
                call gNKern(s,nb,i)
            endif
            time(j) = nb
        enddo

        s=''
        read(u,'(a)',advance='no', iostat=io, size=isize) buffer
        if(isize.gt.0)then 
            s = s//buffer(:isize)
        endif

        i = 1 
        j = 0
        do while (i < len(s))
            nb = -1
            j = j + 1
                call nextNb(s,i)
            if (nbpart == 1) then
                call getNum(s, nb, i)
            else
                call gNKern(s, nb, i)
            endif
            record(j) = nb
        enddo
endsubroutine

    subroutine part1()
        integer :: res, i, j, unit
        integer, parameter :: LargeInt_K = selected_int_kind(32)
        integer(kind=LargeInt_K), dimension(4)  :: time, record

        print *,'input file = ', input

        unit = 1
        open(UNIT=unit, file=input, status='old')

        !Parsing the file
        call parse(unit, time, record, j, 1)

        !Computing
        res = 1
        do i = 1, j
            res = res *  cptRan(time(i),record(i))
        enddo

        print*, 'res = ', res
        close(1)
    endsubroutine

    subroutine part2()
        integer :: res, i, j, unit
        integer, parameter :: LargeInt_K = selected_int_kind(32)
        integer(kind=LargeInt_K), dimension(4)  :: time, record

        print *,'input file = ', input

        unit = 1
        open(UNIT=unit, file=input, status='old')

        !Parsing the file
        call parse(unit, time, record, j, 2)

        !Computing
        res = 1
        do i = 1, j
            res = res *  cptRan(time(i),record(i))
        enddo

        print*, 'res = ', res
        close(1)
    endsubroutine

endmodule

program main
    use m

    print *,''
    print *,'Begin AOC main Day 6'
    print *,''

    input='input.txt'
    print*, 'Part 1'
    call part1()
    print*, 'Part 2'
    call part2()

    print *,''
    print *,'Ending AOC main Day 6'
    print *,''
endprogram
