-module(traffic_rest_app_config).

-export([
  dispatch/0,
  web_config/0
]).

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
  lists:flatten([
    {["fetch", '*'], traffic_rest_app_fetch_resource, []}, % For traccing add here: [{trace_dir, "/tmp/traces"}]} instead of []},
    {["update"],     traffic_rest_app_update_resource, []}
  ]).

web_config() ->
  {ok, App} = application:get_application(?MODULE),
  IpName = list_to_atom(atom_to_list(web_ip) ++ "_" ++ atom_to_list(node())),
  PortName = list_to_atom(atom_to_list(web_port) ++ "_" ++ atom_to_list(node())),
  {ok, Ip} = application:get_env(App, IpName),
  {ok, Port} = application:get_env(App, PortName),
  io:format("Node name:~p, Ip address ~p, Port number~p~n", [node(), Ip, Port]),
  [
    {ip, Ip},
    {port, Port},
    {log_dir, "priv/log"},
    {dispatch, dispatch()}
  ].
  
