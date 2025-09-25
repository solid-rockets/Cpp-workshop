/******************************************************************************

Below, some example of arrays.
The point is that an array variable is actually a pointer to some section in
a running program's memory.
The only thing that differs between them is how the memory is allocated.

Records are different - records can be viewed as "supervariables" which have
individually accessible fields. In a sense, the are arrays, but are accessed
like variables, not pointers.
May be useful to use them in some capacity in the future.

*******************************************************************************/

#include <iostream>
#define SIZE 5

//int arr[SIZE]; // Array on the heap, bss section, deallocated on program close.

int main()
{
    std::cout << "Hello World" << std::endl;

    //int arr[SIZE]; // Array on the stack; deallocated on function return.
    auto arr = new int[5]; // Array on the heap; requires manual deallocation.

    for(int i = 0; i < SIZE; i++)
        std::cout << arr[i] << std::endl;

    return 0;
}
