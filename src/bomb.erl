-module (bomb).
-export ([ard/0]).

ard() ->
    hlr:start_link(),
    bsc_sup:start_link(),
    [bsc_sup:add_controller([N]) || N <- "123456789"],
    Phones = [Phone || {ok, Phone} <- [phone:start_link([N]) || N <- "123456789"]],
    destroy_cpu(Phones, 10000),
    ok.

destroy_cpu(_, 0) -> ok;
destroy_cpu(Phones, N) ->
    phone:action(random(Phones), random(actions())),
    destroy_cpu(Phones, N - 1).

random(L) -> {_, E} = lists:max([{random:uniform(), A} || A <- L]), E.
actions() -> [{call, [$0 + random:uniform(9)]}, accept, reject, hangup].
