-module(hlr_db).
-export([new/1]).
-export([destroy/1]).
-export([attach/3]).
-export([detach/2]).
-export([lookup_id/2]).
-export([lookup_phone/2]).
-export([list_numbers/1]).

new([]) -> ets:new(?MODULE, [ordered_set]).

destroy(Db) -> ets:delete(Db), ok.

attach(Db, Pid, Number) ->
    io:format("attaching ~p~n", [{Number, Pid}]),
    true = ets:insert(Db, {Number, Pid}),
    io:format("pid ~p~n", [lookup_id(Db, Number)]),
    io:format("num ~p~n", [lookup_phone(Db, Pid)]),
    Db.

detach(Db, Pid) ->
    case lookup_phone(Db, Pid) of
        {ok, Number} -> ets:delete(Db, Number);
        {error, invalid} -> ok
    end,
    Db.

lookup_id(Db, Number) ->
    case ets:select(Db, [{{Number, '$1'}, [], ['$1']}]) of
        [Pid] -> {ok, Pid};
        [] -> {error, invalid}
    end.

lookup_phone(Db, Pid) ->
    case ets:select(Db, [{{'$1', Pid}, [], ['$1']}]) of
        [Number] -> {ok, Number};
        [] -> {error, invalid}
    end.

list_numbers(Db) ->
    {ok, ets:select(Db, [{{'$1', '_'}, [], ['$1']}])}.
