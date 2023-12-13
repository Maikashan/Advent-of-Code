module vectChar
    use vectInt
    implicit none
    type vectCh
        character, dimension(:), allocatable                        :: vect
        integer(kind=LargeInt_Vect)                                 :: maxSiz
        integer(kind=LargeInt_Vect)                                 :: curSiz
    endtype
contains

    subroutine inVcCh(v, s)
        type(vectCh), intent(inout)            :: v
        integer(kind=LargeInt_Vect),intent(in)    :: s
        allocate(v%vect(s))
        v%maxSiz = s
        v%curSiz = 0
    endsubroutine

    subroutine adVcCh(v, el)
        type(vectCh), intent(inout)            :: v
        character,intent(in)    :: el
        character, dimension(:), allocatable      :: vect
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

    function atVcCh(v, i) result(val)
        type(vectCh), intent(inout)            :: v
        integer(kind=LargeInt_Vect),intent(in) :: i
        character                              :: val
        val = v%vect(i)
    endfunction

    subroutine rmVcCh(v)
        type(vectCh), intent(inout)            :: v
        deallocate(v%vect)
    endsubroutine
    
endmodule
