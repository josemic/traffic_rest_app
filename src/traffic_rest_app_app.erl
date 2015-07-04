-module(traffic_rest_app_app).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

start(_Type, _StartArgs) ->
    traffic_rest_app_sup:start_link().

stop(_State) ->
    ok.
