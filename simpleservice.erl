-module(simpleservice).
-export([start/2, loop/2, handle_request/2]).

start(Port, Functions)->    
    {ok, ListenSock}=gen_tcp:listen(Port, [list,{active, false},{packet,http}]),
    ?MODULE:loop(ListenSock, Functions).

loop(ListenSock, Functions) ->
    
    {ok, Sock}=gen_tcp:accept(ListenSock),
    spawn(?MODULE, handle_request, [Sock, Functions]),
    ?MODULE:loop(ListenSock, Functions).


handle_request(Sock, Functions) ->
    {ok, {http_request, Method, Path, Version}}=gen_tcp:recv(Sock, 0),
    {Headers, Body}=marshall_request(Sock, Method),
   
    MethodAtom=convert_method_to_atom(Method),
    Function=get_function(MethodAtom, Functions),
    io:format("function is ~p", [Function]), 
    case Function of 
	error-> gen_tcp:send(Sock, lists:flatten(["HTTP/1.1 501 Not Implemented\r\nContent-Type: text/plain; charset=UTF-8\r\nConnection: close\r\n\r\n", "Method", " is not supported by this service.\r\n\r\n"]));
	_->     Function(Headers, Body, Sock)
    end,
    gen_tcp:close(Sock).

get_function(MethodAtom, [Head|Tail])->
    case Head of
	    {MethodAtom, Function}->
			 Function;
		_ ->get_function(MethodAtom, Tail)
		 end;
get_function(MethodAtom, []) ->
    error.

marshall_request(Sock, Method) ->
	 headers(Sock, Method, []).
	
headers(Sock, Method, Headers) ->
	case gen_tcp:recv(Sock, 0) of	
	    {ok, http_eoh} -> body(Sock, Method, Headers);
	    {ok, {http_header,_,HeaderName,_, HeaderValue}} -> io:fwrite("~s: ~s~n", [HeaderName, HeaderValue]), headers(Sock, Method, lists:append([{HeaderName, HeaderValue}], Headers))
	end.

body(Sock, Method, Headers) ->
    case (Method) of 
	'GET' -> {Headers, []};
	'POST' -> read_body(Sock, Headers);
	'PUT' ->read_body(Sock, Headers);
	'DELETE' -> {Headers, []}

	end.

read_body(Sock, Headers)->
    inet:setopts(Sock, [{packet, raw}]),
    case gen_tcp:recv(Sock, 0) of
	{ok, Body} -> {Headers, Body};
	_ -> {error, "We No Habeas Corpus!"}
    end.

convert_method_to_atom(Method)->
    case (Method) of 
		'GET' -> get;
		'POST' -> post;
		'DELETE' -> delete;
		'PUT' -> put
    
    end.
