module vectChar
    implicit none
    integer, parameter :: LargeInt_VectChar = selected_int_kind(32)
    type vectCh
        character, dimension(:), allocatable                        :: vect
        integer(kind=LargeInt_VectChar)                                 :: maxSiz
        integer(kind=LargeInt_VectChar)                                 :: curSiz
    endtype
contains

    subroutine inVcCh(v, s)
        type(vectCh), intent(inout)            :: v
        integer(kind=LargeInt_VectChar),intent(in)    :: s
        allocate(v%vect(s))
        v%maxSiz = s
        v%curSiz = 0
    endsubroutine

    subroutine adVcCh(v, el)
        type(vectCh), intent(inout)            :: v
        character,intent(in)    :: el
        character, dimension(:), allocatable      :: vect
        integer(kind=LargeInt_VectChar)               :: i
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

    function atVcCh(v, i) result(val)
        type(vectCh), intent(inout)            :: v
        integer(kind=LargeInt_VectChar),intent(in) :: i
        character                              :: val
        val = v%vect(i)
    endfunction

    subroutine rmVcCh(v)
        type(vectCh), intent(inout)            :: v
        deallocate(v%vect)
    endsubroutine
    
endmodule
