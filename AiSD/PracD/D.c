#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define NOT_EXIST 0x7FFFFFFFFFFFFFFF

typedef long long int val_t;
#define VAL_T "%lld"

typedef struct node_s {
    struct node_s* right;
    struct node_s* left;

    val_t value;
    val_t priority;
} *node_t;

node_t
create( val_t value ) {
    node_t node = calloc( 1, sizeof( struct node_s ) );
    node -> value = value;
    node -> priority = rand();

    return node;
}

node_t
rotate_left( node_t node ) {
    node_t r = node -> right;
    node_t rl = r -> left;

    r -> left = node;
    node -> right = rl;
    return r;
}

node_t
rotate_right( node_t node ) {
    node_t l = node -> left;
    node_t lr = l -> right;

    l -> right = node;
    node -> left = lr;
    return l;
}

node_t
insert( node_t root, val_t value ) {
    if( root == NULL ) {
        return create( value );
    }

    if( root -> value == value ) {
        return root;
    }

    if( value > root -> value ) {
        root -> right = insert( root -> right, value );
        if( root -> right -> priority > root -> priority ) {
            root = rotate_left( root );
        }
    } else {
        root -> left = insert( root -> left, value );
        if( root -> left -> priority > root -> priority ) {
            root = rotate_right( root );
        }
    }

    return root;
}

node_t
delete( node_t root, val_t value, bool * ok ) {
    if( root == NULL ) {
        *ok = false;
        return root;
    }

    if( value > root -> value ) {
        root -> right = delete( root -> right, value, ok );
    } else if( value < root -> value ) {
        root -> left = delete( root -> left, value, ok );
    } else {
        if( root -> left == NULL || root -> right == NULL ) {
            node_t new = root -> left != NULL ? root -> left : root -> right;
            free( root );
            return new;
        }

        if( root -> left -> priority > root -> right -> priority ) {
            root = rotate_right( root );
            root -> right = delete( root -> right, value, ok );
        } else {
            root = rotate_left( root );
            root -> left = delete( root -> left, value, ok );
        }
    }

    return root;
}

val_t
upper( node_t root, val_t value ) {
    if( root == NULL ) {
        return NOT_EXIST;
    }

    node_t query = root;
    val_t closest = NOT_EXIST;

    while( query != NULL ) {
        if( query -> value == value ) {
            return value;
        }

        if( query -> value > value && query -> value < closest ) {
            closest = query -> value;
        }

        if( value > query -> value ) {
            query = query -> right;
        } else {
            query = query -> left;
        }
    }

    return closest;
}

val_t
lower( node_t root, val_t value ) {
    if( root == NULL ) {
        return NOT_EXIST;
    }

    node_t query = root;
    val_t closest = NOT_EXIST;

    while( query != NULL ) {
        if( query -> value == value ) {
            return value;
        }

        if( query -> value < value && (query -> value > closest || closest == NOT_EXIST) ) {
            closest = query -> value;
        }

        if( value > query -> value ) {
            query = query -> right;
        } else {
            query = query -> left;
        }
    }

    return closest;
}

int
main( void ) {
    srand( time( NULL ) );

    int n;
    scanf( "%d\n", & n );

    node_t root = NULL;
    char op; val_t value;

    while( n -- ) {
        scanf( "%c " VAL_T "\n", & op, & value );

        switch( op ) {
            case 'I':
                root = insert( root, value );
                break;
            case 'D': {
                bool ok = true;
                root = delete( root, value, &ok );
                if( ok ) {
                    puts( "OK" );
                } else {
                    puts( "BRAK" );
                }

                break;
            }
            case 'L': {
                val_t res = lower( root, value );
                if( res == NOT_EXIST ) {
                    puts( "BRAK" );
                } else {
                    printf( VAL_T "\n", res );
                }
                break;
            }
            case 'U': {
                val_t res = upper( root, value );
                if( res == NOT_EXIST ) {
                    puts( "BRAK" );
                } else {
                    printf( VAL_T "\n", res );
                }
            }
        }
    }

    return 0;
}
