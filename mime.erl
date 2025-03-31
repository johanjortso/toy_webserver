-module(mime).
-export([get/1]).

get([]) -> "text/html; charset=utf-8";
get(".html") -> "text/html; charset=utf-8";
get(".gif") -> "image/gif";
get(".jpg") -> "image/jpeg";
get(".jpeg") -> "image/jpeg";
get(".png") -> "image/png";
get(".js") -> "text/javascript";
get(_) -> "text/plain".