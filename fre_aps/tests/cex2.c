

 __uint128_t InterlockedCompareExchange128( volatile __uint128_t * src, __uint128_t cmp, __uint128_t val)
 {
   return __sync_val_compare_and_swap(src, cmp, val);
 };


 int main()
  {
    __uint128_t a=0, b=0, c=0x0123456789ABCDEFULL;
    //__uint128_t a=0, b=0, c=0x0123456789ABCDEFULL;

    InterlockedCompareExchange128(&a, b, c);

    //int foo= -10; int bar= 10;
    //long long int foo64= -10; long long int bar64= 10;
    //if (!__sync_fetch_and_add(&foo, bar) || foo)
    //  return -1;
    //bar= __sync_lock_test_and_set(&foo, bar);
    //if (bar || foo != 10)
    //  return -1;
    //bar= __sync_val_compare_and_swap(&bar, foo, 15);
    //if (bar)
    //  return -1;
    //if (!__sync_fetch_and_add(&foo64, bar64) || foo64)
    //  return -1;
    //bar64= __sync_lock_test_and_set(&foo64, bar64);
    //if (bar64 || foo64 != 10)
    //  return -1;
    //bar64= __sync_val_compare_and_swap(&bar64, foo, 15);
    //if (bar64)
    //  return -1;
    //return 0;
  }
