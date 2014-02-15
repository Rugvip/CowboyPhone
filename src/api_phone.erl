-module(api_phone).

-export([init/3]).
-export([rest_init/2]).
-export([rest_terminate/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([delete_resource/2]).
-export([is_conflict/2]).
-export([on_post_request/2]).
-export([on_get_request/2]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) -> {ok, Req, []}.

rest_terminate(_Req, _State) -> ok.

allowed_methods(Req, State) -> {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) -> {[{<<"application/json">>, on_post_request}], Req, State}.

content_types_provided(Req, State) -> {[{<<"application/json">>, on_get_request}], Req, State}.

% DELETE
delete_resource(Req, State) -> {false, Req, State}.

% PUT
is_conflict(Req, State) -> {false, Req, State}.

on_post_request(Req, State) ->
    Json = mochijson2:encode({struct, [{herp, derp}, {is, dog}]}),
    {true, cowboy_req:set_resp_body(Json, Req), State}.

on_get_request(Req, State) ->
    Json = mochijson2:encode({struct, [{yes, this}, {is, dog}]}),
    {Json, Req, State}.
