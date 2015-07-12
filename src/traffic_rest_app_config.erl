-module(traffic_rest_app_config).

-export([
  dispatch/0,
  web_config/0
]).

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
  lists:flatten([
    {["fetch", '*'], traffic_rest_app_fetch_resource, [{trace_dir, "/tmp/traces"}]},
    {["update"],     traffic_rest_app_update_resource, []}
  ]).

web_config() ->
  {ok, App} = application:get_application(?MODULE),
  {ok, Ip} = application:get_env(App, web_ip),
  {ok, Port} = application:get_env(App, web_port),
  [
    {ip, Ip},
    {port, Port},
    {log_dir, "priv/log"},
    {dispatch, dispatch()}
  ].
  
