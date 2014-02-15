-module(hello_world_app).
-behavior(application).

-export([start/0]).
-export([start/2]).
-export([stop/1]).

start() ->
    application:start(crypto),
    application:start(cowlib),
    application:start(ranch),
    application:start(cowboy),
    application:start(hello_world),
    ok.

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        %% {URIHost, list({URIPath, Handler, Opts})}
        {'_', [{'_', hello_handler, []}]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8081}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    hello_world_sup:start_link().

stop(_State) ->
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(cowlib),
    application:stop(crypto),
    ok.
