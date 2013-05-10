%%%-------------------------------------------------------------------
%%% @author Yang Wang 
%%% @copyright (C) 2013, Yang Wang
%%% @doc
%%%
%%% @end
%%% Created : 10 May 2013 by Yang Wang
%%% Purposed: 统计文件中每个单词出现的行数以及次数。
%%%-------------------------------------------------------------------
-module(text_index).
-compile(export_all).

-import(re, [run/2,replace/4]).

-record(data, {word, line = []}).

-define(DBNAME, dataDB).

%%% 数据库操作
createDB() ->
    ets:new(?DBNAME, [public, named_table, set, {keypos, #data.word}]).
closeDB() ->
    ets:delete(?DBNAME).

start(File) ->
    case file:open(File, read) of
        {ok, IoDevice} ->
            Content = readAllText(IoDevice),
            Lines = string:tokens(Content, "\r\n"), %讲文件分解每行
            processLine(Lines);
        _ ->
            io:format("Open the file failed!")
    end.

%%%将文件的内容全部读入到内存中
readAllText(IoDevice) ->
    readAllText(IoDevice, []).
readAllText(IoDevice, Content)->
    case file:read(IoDevice, 1024) of
        {ok, Text} ->
            readAllText(IoDevice, [Content | Text]);
        eof ->
            file:close(IoDevice),
            lists:flatten(Content);
        {error, Reason}->
            io:format("Read file failed! The reason is:~p~n", [Reason]),
            file:close(IoDevice),
            {error, Reason}
    end.

-define(PATTERN, "[a-zA-Z\.]+").

%%%对文件的每一行进行处理，提取出每一个单词
processLine(Lines) ->
    processLine(Lines, 1).
processLine([H | T], LineNumber) ->
    case re:run(H, ?PATTERN, [global]) of
        {match, MatchItem} ->
           Words =  splitWords(H, lists:flatten(MatchItem)),
           writeDB(Words, LineNumber);
        nomatch ->
            ok
    end,
    processLine(T, LineNumber + 1);
processLine([], _LineNumber) ->
    ok.

splitWords(Line, MatchItem) ->
    splitWords(Line, MatchItem, []).
splitWords(Line, [{Start, Length} | T], Words)->
    Word = string:substr(Line, Start+1, Length),
    splitWords(Line, T, [Word | Words]);
splitWords(_Line, [], Words)->
    lists:reverse(Words).

%%% 将数据额写入到ets数据库中
writeDB([Key | T], LineNumber)->
    case ets:match_object(dataDB, #data{word = Key, _ =  '_'}) of
        [#data{word = Key, line = Value}] ->
            ets:insert(dataDB, #data{word = Key, line = lists:keysort(1, updateData(Value, LineNumber))}),
            writeDB(T, LineNumber);
        _ ->
            ets:insert(dataDB, #data{word = Key, line = [{LineNumber, 1}]}),
            writeDB(T, LineNumber)
    end;
writeDB([], _) ->
    ok.

%%%对数据进行更新，此方法不太好。
updateData(LineValue, LineNumber)->
    case lists:keysearch(LineNumber, 1, LineValue) of
        {value, {LineNumber, Times}} ->
            lists:keyreplace(LineNumber, 1, LineValue, {LineNumber, Times + 1});
        false ->
            [{LineNumber, 1} | LineValue]
    end.

main() ->
    FileName = "C:\\Users\\elqstux\\Desktop\\wy.py",
    createDB(),
    start(FileName).
   %closeDB().
