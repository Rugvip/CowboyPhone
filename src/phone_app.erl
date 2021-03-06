-module(phone_app).
-behavior(application).

-export([start/0]).
-export([start/1]).
-export([start/2]).
-export([stop/1]).

start([Node]) ->
    net_kernel:connect(Node),
    start().

start() ->
    application:start(crypto),
    application:start(cowlib),
    application:start(ranch),
    application:start(cowboy),
    application:start(phone),
    ok.

start(_Type, _Args) ->
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
        {env, [{dispatch, cowboy_router:compile([
            {'_', [
                {"/", cowboy_static, {file, "www/index.html", [{mimetypes, cow_mimetypes, all}]}},
                {"/js/[...]", cowboy_static, {dir, "www/js", [{mimetypes, cow_mimetypes, all}]}},
                {"/css/[...]", cowboy_static, {dir, "www/css", [{mimetypes, cow_mimetypes, all}]}},
                {"/html/[...]", cowboy_static, {dir, "www/html", [{mimetypes, cow_mimetypes, all}]}},
                {"/api/phone/[:number]", hlr_rest, []},
                {"/sock/:number", phone_socket, []},
                {'_', cowboy_static, {file, "www/404.html", [{mimetypes, cow_mimetypes, all}]}}
            ]}
        ])}]}
    ]),
    bsc_sup:start_link(),
    phone_sup:start_link().

stop(_State) ->
    hlr:stop(),
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(cowlib),
    application:stop(crypto),
    ok.
