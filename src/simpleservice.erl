-module(simpleservice).
-export([start/2, loop/2, handle_request/2, separate_path_parts/2, extract_params/1]).

start(Port, Functions)->    
    {ok, ListenSock}=gen_tcp:listen(Port, [list,{active, false},{packet,http}]),
    ?MODULE:loop(ListenSock, Functions).

loop(ListenSock, Functions) ->
    {ok, Sock}=gen_tcp:accept(ListenSock),
    spawn(?MODULE, handle_request, [Sock, Functions]),
    ?MODULE:loop(ListenSock, Functions).


handle_request(Sock, Functions) ->
    {ok, {http_request, Method, Path, Version}}=gen_tcp:recv(Sock, 0),
    {abs_path,AbsPath}=Path,
    {Headers, Body}=marshall_request(Sock, Method),
    {PathString, QueryString, Params, Fragment}=handle_path(AbsPath),
    Function=get_function(Method, Functions),
    case Function of 
	error-> gen_tcp:send(Sock, lists:flatten(["HTTP/1.1 501 Not Implemented\r\nContent-Type: text/plain; charset=UTF-8\r\nConnection: close\r\n\r\n", atom_to_list(Method), " is not supported by this service.\r\n\r\n"]));
	_->     Function(Sock, PathString, QueryString, Params, Fragment, Headers, Body)
    end,
    gen_tcp:close(Sock).


separate_path_parts(Path, Token)->
    PartList=string:tokens(Path, Token),
    [PathString|Part]=PartList,
    {PathString,lists:flatten(Part)}.
 
handle_path(Path)->
    {PathStringWithQuery, Fragment}=separate_path_parts(Path, "#"),
    {PathString, QueryString}=separate_path_parts(Path, "?"),
    Params=extract_params(QueryString),
    {PathString, QueryString, Params, Fragment}.
    

extract_params(QueryString)->    
    Params=string:tokens(QueryString, "&"),
    F=fun(X)->
	      Tokens=string:tokens(X, "="),
	      case length(Tokens) of
		  2 ->
		      [Name|[Value|_]]=Tokens,
		      {Name, Value};
		  _ -> lists:flatten(Tokens)
	      end
      end,
    lists:map(F, Params).
	      
				   
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
	{ok, {http_header,_,HeaderName,_, HeaderValue}} -> headers(Sock, Method, lists:append([{HeaderName, HeaderValue}], Headers))
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
