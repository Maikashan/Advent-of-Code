module container
    implicit none
    integer, parameter :: LargeInt = selected_int_kind(32)
    type keyVal
        character(len=:),allocatable                           :: key
        integer(kind=LargeInt)                                 :: val
    endtype
    type contain
        type(keyVal), dimension(:), allocatable                :: vect
        integer(kind=LargeInt)                                 :: maxSiz
        integer(kind=LargeInt)                                 :: curSiz
    endtype
contains

    subroutine inCont(v, s)
        type(contain), intent(inout)              :: v
        integer(kind=LargeInt),intent(in)         :: s
        allocate(v%vect(s))
        v%maxSiz = s
        v%curSiz = 0
    endsubroutine

    subroutine adCont(v, el)
        type(contain), intent(inout)                 :: v
        type(keyVal),intent(in)                      :: el
        type(keyVal), dimension(:), allocatable      :: vect
        integer(kind=LargeInt)                       :: i
        v%curSiz = v%curSiz + 1
        if (v%curSiz == v%maxSiz) then
            v%maxSiz = v%maxSiz * 2
            vect = v%vect
            deallocate(v%vect)
            allocate(v%vect(v%maxSiz))
            do i = 1, v%maxSiz / 2 - 1
                v%vect(i) = vect(i)
            enddo
        endif
        v%vect(v%curSiz) = el
    endsubroutine

    subroutine slide(v, i)
        type(contain), intent(inout)                :: v
        integer(kind=LargeInt), intent(inout)       :: i
        type(keyVal)                                :: tmp
        do while (i < v%curSiz)
            tmp = atCont(v, i + 1)
            v%vect(i+1) = atCont(v,i)
            v%vect(i) = tmp
            i = i + 1
        enddo
    endsubroutine

    subroutine popCnt(v, el)
        type(contain), intent(inout)                :: v
        type(keyVal), intent(in)                    :: el
        type(keyVal)                                :: tmp
        integer(kind=LargeInt)                      :: i
        i = 1
        do while (i <= v%curSiz)
            tmp = atCont(v,i)
            if (tmp%key == el%key) then
                call slide(v, i)
                v%curSiz = v%curSiz - 1
                exit
            endif
            i = i + 1
        enddo
    endsubroutine

    function rplcCt(v,el) result(done)
        type(contain), intent(inout)                :: v
        type(keyVal), intent(in)                    :: el
        logical                                     :: done
        integer(kind=LargeInt)                      :: i
        type(keyVal)                                :: tmp
        done=.false.
        do i = 1, v%curSiz
            tmp = atCont(v,i)
            if (tmp%key == el%key) then
                v%vect(i) = el
                done = .true.
                exit
            endif
        enddo
    endfunction

    function atCont(v, i) result(val)
        type(contain), intent(inout)           :: v
        integer(kind=LargeInt),intent(in)      :: i
        type(keyVal)                           :: val
        val = v%vect(i)
    endfunction

    subroutine rmVect(v)
        type(contain), intent(inout)            :: v
        deallocate(v%vect)
    endsubroutine
    
endmodule
