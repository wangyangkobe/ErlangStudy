-module(httpclient).

-compile(export_all).

-include_lib("inets/include/httpd.hrl").

start() ->
    client:start(),
    inets:start(),
    inets:start(httpd, [
			{modules, [
				   mod_alias, 
				   mod_auth, 
				   mod_esi, 
				   mod_actions, 
				   mod_cgi, 
				   mod_dir, 
				   mod_get, 
				   mod_head, 
				   mod_log, 
				   mod_disk_log,
				   ?MODULE
				  ]},
			{port,8080},
			{server_root,"." },
			{document_root, "."},
			{server_name,"httpclient"},
			{erl_script_alias, {"", [httpclient]}},
			{error_log, "error.log"},
			{security_log, "security.log"},
			{transfer_log, "transfer.log"},
			{mime_types,[
				     {"html","text/html"},
				     {"css","text/css"},
				     {"js","application/x-javascript"}
				    ]}
		       ]).

stop() ->
    client:stop(),
    inets:stop().

do(#mod{request_uri = Request_Uri} = _ModData) ->
    io:format("do~p~n", [Request_Uri]),
    io:format("~p~n", [httpd_util:split_path(Request_Uri)]),
    {PathInfo, QueryString} = httpd_util:split_path(Request_Uri),
    case QueryString of
	[] ->
	    decode_path_info(PathInfo);
	_ ->
	    Body = case decode_request_parameter(QueryString) of
		       ["", _, _] ->
			   "<html><body>Hello world</body></html>";
		       [add_db_server, ServerName, _] ->
			   Res = client:add_db_server(ServerName),
			   format_response(Res);
		       [ls_db_server, "", ""] ->
			   format_response(client:ls_db_server());
		       [rm_db_server, ServerName, ""] ->
			   format_response(client:rm_db_server(ServerName));
		       [insert, Key, Value] ->
			   format_response(client:insert(Key, Value));
		       [update, Key, Value] ->
			   format_response(client:update(Key, Value));
		       [delete, Key, ""] ->
			   format_response(client:delete(Key));
		       [search, Key, ""] ->
			   format_response(client:search(Key))
		   end,
	    {proceed, [{response, {200, Body}}]}
    end.

decode_request_parameter(QueryString) ->
    QueryList = httpd:parse_query(QueryString),
    Fun = fun(Key) ->
		  case proplists:get_value(Key, QueryList) of
		      undefined -> "";
		      Value -> list_to_atom(Value)
		  end
	  end,
    lists:map(Fun, ["?fun", "key", "value"]).


format_response(Response) ->
    lists:flatten(io_lib:format("<html><body>~p</body></html>", [Response])).

decode_path_info("/httpclient") ->
      {proceed, [{response, {200, index_page_body()}}]};
decode_path_info("/httpclient/add") ->
    Body = "<html><body>
             <form name=\"input\" action=\"http://127.0.0.1:8080/httpclient\" mepthod=\"get\">
              ServerName:<input type=\"hidden\" name=\"fun\" value=\"add_db_server\"/><input type=\"text\" name=\"key\"/><input type=\"submit\" value=\"Submit\"/>
             </form>
             </html></body>",
    {proceed, [{response, {200, Body}}]};
decode_path_info("/httpclient/insert") ->
    Body = "<html><body>
           <form name=\"input\" action=\"http://127.0.0.1:8080/httpclient\" mepthod=\"get\">
           <input type=\"hidden\" name=\"fun\" value=\"insert\"/>Key:<input type=\"text\" name=\"key\"/><br>Value:<input type=\"text\" name=\"value\"/><input type=\"submit\" value=\"Submit\"/>
           </form>
           </html></body>",
     {proceed, [{response, {200, Body}}]};
decode_path_info("/httpclient/search") ->
    Body = "<html><body>
             <form name=\"input\" action=\"http://127.0.0.1:8080/httpclient\" mepthod=\"get\">
             <input type=\"hidden\" name=\"fun\" value=\"search\"/>Key:<input type=\"text\" name=\"key\"/><input type=\"submit\" value=\"Submit\"/>
             </form>
            <html><body>",
      {proceed, [{response, {200, Body}}]};
decode_path_info("/httpclient/delete") ->
    Body = "<html><body>
             <form name=\"input\" action=\"http://127.0.0.1:8080/httpclient\" mepthod=\"get\">
              Key:<input type=\"hidden\" name=\"fun\" value=\"delete\"/><input type=\"text\" name=\"key\"/><input type=\"submit\" value=\"Submit\"/>
             </form>
             </html></body>",
    {proceed, [{response, {200, Body}}]};
decode_path_info("/httpclient/update") ->
    Body = "<html><body>
           <form name=\"input\" action=\"http://127.0.0.1:8080/httpclient\" mepthod=\"get\">
           <input type=\"hidden\" name=\"fun\" value=\"update\"/>Key:<input type=\"text\" name=\"key\"/><br>Value:<input type=\"text\" name=\"value\"/><input type=\"submit\" value=\"Submit\"/>
           </form>
           </html></body>",
     {proceed, [{response, {200, Body}}]};
decode_path_info("/httpclient/rm") ->
    Body = "<html><body>
             <form name=\"input\" action=\"http://127.0.0.1:8080/httpclient\" mepthod=\"get\">
             <input type=\"hidden\" name=\"fun\" value=\"rm_db_server\"/>Key:<input type=\"text\" name=\"key\"/><input type=\"submit\" value=\"Submit\"/>
             </form>
            <html><body>",
      {proceed, [{response, {200, Body}}]}.


index_page_body() ->
    "<html><body><h1>Web DataBase Cluser</h1>
	<a href=\"http://127.0.0.1:8080/httpclient/add\">add_db_server</a><br>
        <a href=\"http://127.0.0.1:8080/httpclient/rm\">rm_db_server</a><br>
        <a href=\"http://127.0.0.1:8080/httpclient?fun=ls_db_server\">ls_db_server</a><br>
        <a href=\"http://127.0.0.1:8080/httpclient/insert\">insert</a><br>
        <a href=\"http://127.0.0.1:8080/httpclient/search\">search</a><br>
        <a href=\"http://127.0.0.1:8080/httpclient/update\">update</a><br>
        <a href=\"http://127.0.0.1:8080/httpclient/delete\">delete</a><br>
     <html><body>".
