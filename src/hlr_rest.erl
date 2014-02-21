-module(hlr_rest).

-export([init/3]).
-export([rest_init/2]).
-export([rest_terminate/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([delete_resource/2]).
-export([is_conflict/2]).
-export([on_put_request/2]).
-export([on_get_request/2]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) -> {ok, Req, []}.

rest_terminate(_Req, _State) -> ok.

allowed_methods(Req, State) -> {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) -> {[{<<"application/json">>, on_put_request}], Req, State}.

content_types_provided(Req, State) -> {[{<<"application/json">>, on_get_request}], Req, State}.

% GET
on_get_request(Req, State) ->
    {ok, List} = hlr:list_numbers(),
    Json = mochijson2:encode([number_to_json(Number) || Number <- List]),
    {Json, Req, State}.

% PUT
is_conflict(Req, State) ->
    {NumberBin, Req2} = cowboy_req:binding(number, Req, ""),
    Number = binary_to_list(NumberBin),
    Conflict = case hlr:lookup_id(Number) of
        {ok, _} -> true;
        _ -> false
    end,
    {Conflict, Req2, State}.

% PUT
on_put_request(Req, State) ->
    {NumberBin, Req2} = cowboy_req:binding(number, Req, ""),
    Number = binary_to_list(NumberBin),
    bsc_sup:remove_controller(Number),
    bsc_sup:add_controller(Number),
    Json = mochijson2:encode(number_to_json(Number)),
    {true, cowboy_req:set_resp_body(Json, Req2), State}.

% DELETE
delete_resource(Req, State) ->
    {NumberBin, Req2} = cowboy_req:binding(number, Req, ""),
    Number = binary_to_list(NumberBin),
    bsc_sup:remove_controller(Number),
    {true, Req2, State}.

number_to_json(Number) -> {struct, [{number, list_to_binary(Number)}]}.
