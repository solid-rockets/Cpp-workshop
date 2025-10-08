extern int func(int a, int b, int c);

extern int test_sym;

int main()
{
    // Init.
    int Result = 0;

    // Logic.
    Result = func(test_sym, 2, 3);

    // Return.
    return Result;
}
