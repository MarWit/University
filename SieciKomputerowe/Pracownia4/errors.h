#pragma once

#include <stdio.h>

enum HTTPError {
    InternalError    = -1,
    OK               = 200,
    MovedPermanently = 301,
    Forbidden        = 403,
    NotFound         = 404,
    NotImplemented   = 501
};

inline static char *
http_error( enum HTTPError error ) {
    switch( error ) {
        case OK:
            return "OK";
        case MovedPermanently:
            return "Moved Permanently";
        case Forbidden:
            return "Forbidden";
        case NotFound:
            return "Not Found";
        case NotImplemented:
            return "Not Implemented";
        default:
            return (char*) 0;
    }
}

inline static char *
full_http_error( enum HTTPError error ) {
    static char buffer[ 128 ];
    sprintf( buffer, "%d %s", error, http_error( error ) );

    return buffer;
}
