#include <stdio.h>
#include <assert.h>

#ifdef VARIANT
int strdrop(char *str, const char *set);
int strcnt(const char *str, const char *set);
#else
#include <dlfcn.h>

int (*strdrop)(char*, const char*);
int (*strcnt)(const char*, const char*);
#endif

int
main( void ) {
    char my_string[] = "aaaaabbbbbcccccddddd";
    char my_hated_chars[] = "bc";
    char my_favourite_chars[] = "d";

#ifdef VARIANT
    printf( "Variant: Load-time linking\n" );
#else
    printf( "Variant: Run-time linking\n" );
    void *handle = dlopen( "./zad5_lib.so", RTLD_LAZY );

    strdrop = dlsym( handle, "strdrop" );
    strcnt = dlsym( handle, "strcnt" );
#endif

    printf( "Original string: %s\n", my_string );

    int cnt = strdrop( my_string, my_hated_chars );
    printf( "String w/o \"%s\": %s (%d chars)\n", my_hated_chars, my_string, cnt );
    printf( "Number of \"%s\" chars in new string: %d\n", my_hated_chars, strcnt( my_string, my_hated_chars ) );
    printf( "Number of \"%s\" chars in new string: %d\n", my_favourite_chars, strcnt( my_string, my_favourite_chars ) );

    return 0;
}
