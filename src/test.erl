-module (test).
-export ([test/0]).

test() ->


    io:format("Starting supervisors: ~n",[]),
    {ok, _} = bsc_sup:start_link(),

    io:format("Adding controllers ~n",[]),
    {ok, _} = bsc_sup:add_controller("123"),
    {ok, _} = bsc_sup:add_controller("124"),
    {ok, Pid} = bsc_sup:add_controller("125"),
    {ok, _} = bsc_sup:add_controller("126"),
    io:format("Supervisors started ~n",[]),

    ok = bsc_sup:remove_controller("126"),
    io:format("Controller 126 removed ~n",[]),

    true = exit(Pid,normal),
    io:format("125 exited normal, sup should start this ~n",[]),

    io:format("Starting phones. ~n",[]),
    {ok, P123} = phone:start_link("123"),
    {ok, P124} = phone:start_link("124"),
    {ok, P125} = phone:start_link("125"),
    io:format("Phones started. ~n",[]),

    io:format("Calling 125 from 123 ~n",[]),
    phone:action(P123,{call,"125"}),
    timer:sleep(1000),
    phone:action(P124,accept),
    timer:sleep(1000),

    io:format("Calling 123 and 125 while busy from 124 ~n",[]),
    phone:action(P124,{call,"123"}),
    timer:sleep(2000),
    phone:action(P124,{call,"125"}),
    timer:sleep(1000),

    io:format("123 hanging up ~n",[]),
    phone:action(P123,hangup),
    timer:sleep(1000),

    io:format("Calling 124 from 125, should reject ~n",[]),
    phone:action(P125,{call,"124"}),
    timer:sleep(1000),
    phone:action(P124,reject),
    ok.
