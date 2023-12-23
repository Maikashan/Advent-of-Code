module vect
    implicit none
    integer, parameter :: LargeInt_Vect = selected_int_kind(32)
    type pos
        character                                                   :: from
        integer(kind=LargeInt_Vect)                                 :: x
        integer(kind=LargeInt_Vect)                                 :: y
    endtype
    type vector
        type(pos),dimension(:), allocatable                         :: vect
        integer(kind=LargeInt_Vect)                                 :: maxSiz
        integer(kind=LargeInt_Vect)                                 :: curSiz
    endtype
contains

    function posEq(pos1, pos2) result(res)
        type(pos),intent(in)                                        :: pos1, pos2
        logical                                                     :: res
        res = pos1%x==pos2%x.and.pos1%y==pos2%y.and.pos1%from==pos2%from
    endfunction

    subroutine initVc(v, s)
        type(vector), intent(inout)            :: v
        integer(kind=LargeInt_Vect),intent(in)    :: s
        allocate(v%vect(s))
        v%maxSiz = s
        v%curSiz = 0
    endsubroutine

    function addVec(v, el) result(pres)
        type(vector), intent(inout)                                 :: v
        type(pos),intent(in)                                        :: el
        logical                                                     :: pres
        type(pos), dimension(:), allocatable                        :: vect
        integer(kind=LargeInt_Vect)                                 :: i
        pres = .true.
        if (v%curSiz + 1 == v%maxSiz) then
            v%maxSiz = v%maxSiz * 2
            vect = v%vect
            deallocate(v%vect)
            allocate(v%vect(v%maxSiz))
            do i = 1, v%maxSiz / 2 - 1
                v%vect(i) = vect(i)
            enddo
        endif
        do i = 1, v%curSiz
            if (posEq(v%vect(i),el)) then
                pres=.false.
                exit
            endif
        enddo
        if (i == v%curSiz + 1) then
            v%curSiz = v%curSiz + 1
            v%vect(v%curSiz) = el
        endif
    endfunction

    function atVect(v, i) result(val)
        type(vector), intent(inout)            :: v
        integer(kind=LargeInt_Vect),intent(in) :: i
        type(pos)                              :: val
        val = v%vect(i)
    endfunction

    subroutine rmVect(v)
        type(vector), intent(inout)            :: v
        deallocate(v%vect)
    endsubroutine
    
endmodule
