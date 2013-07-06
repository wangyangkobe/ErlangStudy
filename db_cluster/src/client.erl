-module(client).
-compile(export_all).

start()->
    application:start(db_cluser).
stop()->
    application:stop(db_cluser).

add_db_server(ServerName) ->
    db_proxy:add_db_server(ServerName).
ls_db_server()->
    db_proxy:ls_db_server().
rm_db_server(ServerName) ->
    db_proxy:rm_db_server(ServerName).

insert(Key, Value) ->
    db_proxy:insert(Key, Value).
update(Key, Value) ->
    db_proxy:update(Key, Value).
search(Key) ->
    db_proxy:search(Key).
delete(Key) ->
    db_proxy:delete(Key).
main()->
    start(),
    add_db_server(s1),
    add_db_server(s2),
    add_db_server(s3),
    insert(t, 1),
    insert(d, 2),
    insert(x, 6),
    insert(y, 8).
