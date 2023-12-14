module vectLogi
    implicit none
    integer, parameter :: LargeInt_VectLogi = selected_int_kind(32)
    type vectLo
        logical, dimension(:), allocatable        :: vect
        integer(kind=LargeInt_VectLogi)               :: maxSiz
        integer(kind=LargeInt_VectLogi)               :: curSiz
    endtype
contains

    subroutine inVcLo(v, s)
        type(vectLo), intent(inout)               :: v
        integer(kind=LargeInt_VectLogi),intent(in)    :: s
        allocate(v%vect(s))
        v%maxSiz = s
        v%curSiz = 0
    endsubroutine

    subroutine adVcLo(v, el)
        type(vectLo), intent(inout)               :: v
        logical,intent(in)                        :: el
        logical, dimension(:), allocatable        :: vect
        integer(kind=LargeInt_VectLogi)               :: i
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

    function atVcLo(v, i) result(val)
        type(vectLo), intent(inout)               :: v
        integer(kind=LargeInt_VectLogi),intent(in)    :: i
        logical                                   :: val
        val = v%vect(i)
    endfunction

    subroutine rmVcLo(v)
        type(vectLo), intent(inout)            :: v
        deallocate(v%vect)
    endsubroutine
    
endmodule
