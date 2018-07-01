#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define NOT_EXIST 0x7FFFFFFFFFFFFFFF

typedef long long int val_t;
#define VAL_T "%lld"

typedef struct node_s {
    struct node_s* parent;
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

void
rotate_left( node_t node ) {
    node_t r = node -> right;
    node_t rl = r -> left;

    r -> parent = node -> parent;
    node -> parent = r;

    if( rl != NULL ) {
        rl -> parent = node;
    }

    r -> left = node;
    node -> right = rl;

    if( r -> parent != NULL ) {
        if( r -> value > r -> parent -> value ) {
            r -> parent -> right = r;
        } else {
            r -> parent -> left = r;
        }
    }
}

void
rotate_right( node_t node ) {
    node_t l = node -> left;
    node_t lr = l -> right;

    l -> parent = node -> parent;
    node -> parent = l;

    if( lr != NULL ) {
        lr -> parent = node;
    }

    l -> right = node;
    node -> left = lr;

    if( l -> parent != NULL ) {
        if( l -> value > l -> parent -> value ) {
            l -> parent -> right = l;
        } else {
            l -> parent -> left = l;
        }
    }
}

void
print2DUtil(node_t root, int space) {
    // Base case
    if (root == NULL)
        return;

    // Increase distance between levels
    space += 10;

    // Process right child first
    print2DUtil(root->right, space);

    // Print current node after space
    // count
    printf("\n");
    for (int i = 10; i < space; i++)
        printf(" ");
    printf("%lld\n", root->value);

    // Process left child
    print2DUtil(root->left, space);
}

node_t
insert( node_t root, val_t value ) {
    if( root == NULL ) {
        return create( value );
    }

    node_t query = root;
    node_t parent = NULL;

    while( query != NULL ) {
        parent = query;

        if( value == query -> value ) {
            return root;
        }

        if( value > query -> value ) {
            query = query -> right;
        } else {
            query = query -> left;
        }
    }

    query = create( value );
    query -> parent = parent;

    if( value > parent -> value ) {
        parent -> right = query;

        if( query -> priority > parent -> priority ) {
            rotate_left( parent );
            if( parent == root ) {
                root = parent -> parent;
            }
        }
    } else {
        parent -> left = query;

        if( query -> priority > parent -> priority ) {
            rotate_right( parent );
            if( parent == root ) {
                root = parent -> parent;
            }
        }
    }

    return root;
}

node_t
delete( node_t root, val_t value, bool * ok ) {
    *ok = true;

    if( root == NULL ) {
        *ok = false;
        return root;
    }

    node_t query = root;
    while( query != NULL ) {
        if( value == query -> value ) {
            break;
        }

        if( value > query -> value ) {
            query = query -> right;
        } else {
            query = query -> left;
        }
    }

    if( query == NULL ) {
        *ok = false;
        return root;
    }

    if( query -> left == NULL || query -> right == NULL ) {
        node_t new = query -> left != NULL ? query -> left : query -> right;
        if( query == root ) {
            root = new;
        } else {
            if( value > query -> parent -> value ) {
                query -> parent -> right = new;
            } else {
                query -> parent -> left = new;
            }
        }

        free( query );
        return root;
    }

    node_t lowest = query -> left;
    while( lowest -> left != NULL ) {
        lowest = lowest -> left;
    }

    query -> value = lowest -> value;
    lowest -> parent -> left = NULL;
    free( lowest );

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

        print2DUtil( root, 0 );
        puts( "---" );
    }

    return 0;
}
