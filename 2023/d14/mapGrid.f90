module vectGrid
    implicit none
    integer, parameter :: LargeInt_VectGrid = selected_int_kind(32)
    type vectGd
        character, dimension(:,:,:), allocatable                     :: vect
        integer(kind=LargeInt_VectGrid)                              :: maxSiz
        integer(kind=LargeInt_VectGrid)                              :: curSiz
        integer(kind=LargeInt_VectGrid)                              :: width
    endtype
contains

    subroutine inVcGd(v, s)
        type(vectGd), intent(inout)            :: v
        integer(kind=LargeInt_VectGrid),intent(in)    :: s
        allocate(v%vect(s,s,s))
        v%maxSiz = s
        v%curSiz = 0
        v%width = s
    endsubroutine

    function adVcGd(v, el) result(index)
        type(vectGd), intent(inout)            :: v
        character,dimension(:,:),allocatable, intent(in)    :: el
        character, dimension(:,:,:),allocatable :: vect
        integer(kind=LargeInt_VectGrid)               :: i, index
        integer                                       :: tmp1, tmp2
        index = 0
        do i = 1, v%curSiz
            if (ALL(v%vect(:,:,i) == el(:,:))) then
                index = i
                exit
            endif
        enddo
        if (i >v%curSiz) then
            v%curSiz = v%curSiz + 1
            if (v%curSiz == v%maxSiz) then
                v%maxSiz = v%maxSiz * 2
                tmp1 = v%maxSiz
                tmp2 = v%width
                allocate(vect(tmp2,tmp2,tmp1))
                vect = v%vect
                deallocate(v%vect)
                allocate(v%vect(v%width,v%width, v%maxSiz))
                do i = 1, v%maxSiz / 2 - 1
                    v%vect(:,:,i) = vect(:,:,i)
                enddo
                deallocate(vect)
            endif
            v%vect(:,:,v%curSiz) = el(:,:)
        endif
    endfunction

    function atVcGd(v, i) result(val)
        type(vectGd), intent(inout)            :: v
        integer(kind=LargeInt_VectGrid),intent(in) :: i
        character,dimension(:,:),allocatable :: val
        val = v%vect(:,:,i)
    endfunction

    subroutine rmVcGd(v)
        type(vectGd), intent(inout)            :: v
        deallocate(v%vect)
    endsubroutine
    
endmodule
