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
    {Headers, Body, PathString, QueryString,Params, Fragment}=marshall_request(Sock, Method, AbsPath),        
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
		      {Name, decode(re:replace(Value,"\\\+"," ",[global, {return,list}]))};
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

marshall_request(Sock, Method, AbsPath) ->    
    {Headers, Body}=headers(Sock, Method, []),
    {PathString, QueryString, PathParams, Fragment}=handle_path(decode(AbsPath)),
    ContentType=lists:keyfind('Content-Type', 1, Headers),
    case ContentType of 
	{_,"application/x-www-form-urlencoded"} ->BodyParams=extract_params(Body),{Headers, Body, PathString, QueryString, BodyParams, Fragment};
	_->{Headers, Body, PathString, QueryString, PathParams, Fragment}
    end.



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
    ContentLength=lists:keyfind('Content-Length', 1, Headers),
    {_,Length}=ContentLength,
    LengthInt=list_to_integer(Length),
    case LengthInt of 
	0 -> {Headers, []};
	_ ->{ok,Body}=gen_tcp:recv(Sock, LengthInt),
	    {Headers, Body}
    end.



read_body(Sock, Headers, Count)->
    inet:setopts(Sock, [{packet, raw}]),
    case gen_tcp:recv(Sock, 0, 1000) of
	{ok, Body} -> {Headers, Body};
	{error, timeout}-> 
	    case Count of 
		3 -> {error, "We No Habeas Corpus! (too many tries)"};
		_ -> read_body(Sock, Headers, Count + 1)
	    end;
	_ ->{error, "We No Habeas Corpus!"}
    end.


%% taking this from inets/http_uri cause I can't seem to figure out import/export


decode(String) ->
    do_decode(String).

do_decode([$%,Hex1,Hex2|Rest]) ->
    [hex2dec(Hex1)*16+hex2dec(Hex2)|do_decode(Rest)];
do_decode([First|Rest]) ->
    [First|do_decode(Rest)];
do_decode([]) ->
    [].

hex2dec(X) when (X>=$0) andalso (X=<$9) -> X-$0;
hex2dec(X) when (X>=$A) andalso (X=<$F) -> X-$A+10;
hex2dec(X) when (X>=$a) andalso (X=<$f) -> X-$a+10.


