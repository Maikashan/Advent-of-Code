module queueL

    use vect
    implicit none
    type queue
        type(vector)                :: q
    endtype

contains
    
    subroutine initQu(q,s)
        type(queue), intent(inout)                :: q
        integer(kind=LargeInt_Vect),intent(in)    :: s
        call initVc(q%q,s)
    endsubroutine

    function sizeQu(q) result(res)
        type(queue), intent(in)     :: q
        integer(kind=LargeInt_Vect) :: res
        res = q%q%curSiz
    endfunction

    subroutine rmQu(q)
        type(queue), intent(inout)  :: q
        call rmVect(q%q)
    endsubroutine
        
    function peekQu(q) result(val)
        type (queue), intent(inout)    :: q
        integer(kind=LargeInt_Vect) :: val, i
        i = 1
        val = atVect(q%q, i)
    endfunction

    subroutine moveQu(q, i)
        type(queue), intent(inout)              :: q
        integer(kind=LargeInt_Vect), intent(in) :: i
        integer(kind=LargeInt_Vect)             :: j
        j = i + 1
        q%q%vect(i) = atVect(q%q,j)
    endsubroutine

    subroutine pushQu(q, val)
        type(queue), intent(inout)              :: q
        integer(kind=LargeInt_Vect), intent(in) :: val
        call addVec(q%q, val)
    endsubroutine

    subroutine popQu(q)
        type(queue), intent(inout)              :: q
        integer(kind=LargeInt_Vect)             :: i
        do i = 1, q%q%curSiz - 1
            call moveQu(q, i)
        enddo
        q%q%curSiz = q%q%curSiz - 1
    endsubroutine
        
endmodule
