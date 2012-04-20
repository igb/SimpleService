-module(simpleservice).
-export([start/2, start/3, loop/3, handle_request/3, separate_path_parts/2, extract_params/1, send_html_message/2, send_plaintext_message/2, send_message/5]).


start(Port, Functions)->    
    start(Port, Functions, null).

start(Port, Functions, Pid)->    
    {ok, ListenSock}=gen_tcp:listen(Port, [list,{active, false},{packet,http}]),
    ?MODULE:loop(ListenSock, Functions, Pid).

loop(ListenSock, Functions, Pid) ->
    {ok, Sock}=gen_tcp:accept(ListenSock),
    spawn(?MODULE, handle_request, [Sock, Functions, Pid]),
    ?MODULE:loop(ListenSock, Functions, Pid).


handle_request(Sock, Functions, Pid) ->
    {ok, {http_request, Method, Path, Version}}=gen_tcp:recv(Sock, 0),
    {abs_path,AbsPath}=Path,
    {Headers, Body}=marshall_request(Sock, Method),
    {PathString, QueryString, Params, Fragment}=handle_path(http_uri:decode(AbsPath)),
    Function=get_function(Method, Functions),
    case Function of 
	error-> send_message(Sock, lists:flatten([atom_to_list(Method), " is not supported by this service."]), "text/plain", 501, "Not Implemented");
	_->     case Pid of
		    null -> Function(Sock, PathString, QueryString, Params, Fragment, Headers, Body);
		    _  -> Function(Sock, PathString, QueryString, Params, Fragment, Headers, Body, Pid)
		end
    end,
    gen_tcp:close(Sock).

send_html_message(Sock,Message)->
    send_message(Sock, Message, "text/html", 200, "OK").

send_plaintext_message(Sock,Message)->
    send_message(Sock, Message, "text/plain", 200, "OK").

send_message(Sock, Message, ContentType, StatusCode, StatusDescription)->
    case Message of
	nil -> 
	    gen_tcp:send(Sock, lists:flatten(["HTTP/1.1 ", io_lib:format("~p ", [StatusCode]), StatusDescription, "\r\nContent-Type: ", ContentType, "; charset=UTF-8\r\nConnection: close\r\n\r\n"]));
	_ ->
	    gen_tcp:send(Sock, lists:flatten(["HTTP/1.1 ", io_lib:format("~p ", [StatusCode]), StatusDescription, "\r\nContent-Type: ", ContentType, "; charset=UTF-8\r\nConnection: close\r\n\r\n", Message,"\r\n\r\n"]))
    end.
    


				  
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
