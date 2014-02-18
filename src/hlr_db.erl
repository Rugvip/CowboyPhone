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
    true = ets:insert_new(Db, {Pid, Number}),
    Db.

detach(Db, Pid) ->
    true = ets:delete(Db, Pid),
    Db.

lookup_id(Db, Number) ->
    case ets:select(Db, [{{'$1', Number}, [], ['$1']}]) of
        [Pid] -> {ok, Pid};
        [] -> {error, invalid}
    end.

lookup_phone(Db, Pid) ->
    case ets:select(Db, [{{Pid, '$1'}, [], ['$1']}]) of
        [Number] -> {ok, Number};
        [] -> {error, invalid}
    end.

list_numbers(Db) ->
    {ok, ets:select(Db, [{{'_', '$1'}, [], ['$1']}])}.
