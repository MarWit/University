#pragma once

#include <string.h>

void serve( int sockfd, short port, char *rootpath );

inline static char *
get_mime_from_path( char * file ) {
    int len = strlen( file );
    while( -- len ) {
        if( file[ len ] == '.' ) {
            file = &file[ len ];
            break;
        }

        if( file[ len ] == '/' ) {
            return "application/octet-stream";
        }
    }

    if( len <= 0 ) {
        return "application/octet-stream";
    }

    if( strcmp( file, ".jpg" ) == 0 )
        return "image/jpg";
    else if( strcmp( file, ".png" ) == 0 )
        return "image/png";
    else if( strcmp( file, ".pdf" ) == 0 )
        return "application/pdf";
    else if( strcmp( file, ".txt" ) == 0 )
        return "text/plain";
    else if( strcmp( file, ".css" ) == 0 )
        return "text/css";
    else if( strcmp( file, ".html" ) == 0 )
        return "text/html";
    else
        return "application/octet-stream";
}
