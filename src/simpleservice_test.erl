-module(simpleservice_test).

-export([start_harness/1]).


start_harness(Port)->
    GetFunction=fun(Sock, PathString, QueryString, Params, Fragment, Headers, Body)->			 
			gen_tcp:send(Sock, "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\nConnection: close\r\n\r\n<html><body><form method='post'><input name='foo' type='text'/><br/><input name='bar' type='text'/><br/><input type='submit'/></form></body></html>\r\n\r\n") 
		end,
       PostFunction=fun(Sock, PathString, QueryString, Params, Fragment, Headers, Body)->
			    io:fwrite("body: ~p~n", [Body]),
			    io:fwrite("headers: ~p~n", [Headers]),
			    io:fwrite("params: ~p~n", [Params]),

			    gen_tcp:send(Sock, "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\nConnection: close\r\n\r\n<body><h1>Hello</H1></body>\r\n\r\n") 
		end,
    Functions=[{'GET', GetFunction}, {'POST', PostFunction}],
    simpleservice:start(8000, Functions).
