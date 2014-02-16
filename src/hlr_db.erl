-module(hlr_db).
-export([new/1, destroy/1, attach/3, detach/2, lookup_id/2, lookup_phone/2]).

new([]) -> ets:new(?MODULE, []).

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
