module vect
    implicit none
    integer, parameter :: LargeInt_Vect = selected_int_kind(32)
    type vector
        integer(kind=LargeInt_Vect), dimension(:), allocatable      :: vect
        integer(kind=LargeInt_Vect)                                 :: maxSiz
        integer(kind=LargeInt_Vect)                                 :: curSiz
    endtype
contains

    subroutine initVc(v, s)
        type(vector), intent(inout)            :: v
        integer(kind=LargeInt_Vect),intent(in)    :: s
        allocate(v%vect(s))
        v%maxSiz = s
        v%curSiz = 0
    endsubroutine

    subroutine addVec(v, el)
        type(vector), intent(inout)            :: v
        integer(kind=LargeInt_Vect),intent(in)    :: el
        integer(kind=LargeInt_Vect), dimension(:), allocatable      :: vect
        integer(kind=LargeInt_Vect)               :: i
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

    function atVect(v, i) result(val)
        type(vector), intent(inout)            :: v
        integer(kind=LargeInt_Vect),intent(in) :: i
        integer(kind=LargeInt_Vect)            :: val
        val = v%vect(i)
    endfunction

    subroutine rmVect(v)
        type(vector), intent(inout)            :: v
        deallocate(v%vect)
    endsubroutine
    
endmodule
