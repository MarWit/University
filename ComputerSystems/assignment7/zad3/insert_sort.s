.globl insert_sort
.type insert_sort, @function

first       = %rdi
last        = %rsi
element     = %rax
tmp         = %rcx
f_elem      = %r8
pivot       = %r9

.text
insert_sort:
    cmp first, last         #   if( first == last )
    je end                  #       return first
    mov first, f_elem       #   f_elem = first
loop:                       #   do {
    mov f_elem, pivot       #       pivot = f_elem
    lea 8(f_elem), f_elem   #       f_elem = f_elem + 8
    mov (f_elem), element   #       element = *f_elem
shift:                      #       while( true ) {
    mov (pivot), tmp        #           tmp = *pivot
    cmp tmp, element        #           if( tmp > element )
    jge end_shift           #                break;
    mov tmp, 8(pivot)       #           *(pivot + 8) = *pivot
    cmp pivot, first        #           if( pivot == first )
    ja end_shift            #               break;
    lea -8(pivot), pivot    #           pivot = pivot - 8
    jmp shift               #       }
end_shift:
    mov element, 8(pivot)   #       *(pivot + 8) = element
    cmp f_elem, last        #   while( f_elem != last )
    jne loop
end:
    ret

.size insert_sort, . - insert_sort
