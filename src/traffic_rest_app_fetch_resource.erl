-module(traffic_rest_app_fetch_resource).
-export([
  init/1,
  allowed_methods/2,
  content_types_provided/2,
  to_text/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init(Config) ->
  %%{{trace, "/tmp"}, Config}.  %% debugging code
  {ok, Config}.             %% regular code


allowed_methods(Req, State) ->
  {['GET'], Req, State}.

content_types_provided(ReqData, Context) ->
  {[{"text/plain", to_text}], ReqData, Context}.

to_text(ReqData, Context) ->
   Path = wrq:disp_path(ReqData),
   Result = case Path of
            "num" ->
              get_params_num(ReqData, Context);
            "deg" ->
              get_params_deg(ReqData, Context);
            _Other ->
              io_lib:format("Unsupported operation: ~p~n", [Path])
          end,
   {Result, ReqData, Context}.

 get_params_deg(ReqData, Context)->
   Zoom = wrq:get_qs_value("zoom", ReqData),
   Lat = wrq:get_qs_value("lat", ReqData),
   Lon = wrq:get_qs_value("lon", ReqData),
   OSMWayID = wrq:get_qs_value("wayid", "0", ReqData),
  OSMNodeID = wrq:get_qs_value("nodeid", "0", ReqData),
   ZoomParam = list_to_integer(Zoom),
   LatParam = list_to_integer(Lat),
   LonParam = list_to_integer(Lon),
   OSMWayIDParam = list_to_integer(OSMWayID),
   OSMNodeIDParam = list_to_integer(OSMNodeID),
   {Status, Res} = trafficapp:fetch({deg, ZoomParam, LatParam, LonParam}, {OSMWayIDParam, OSMNodeIDParam}),
   lists:flatten(io_lib:format("~p", [Res])).

 get_params_num(ReqData, Context)->
   Zoom = wrq:get_qs_value("zoom", ReqData),
   TileX = wrq:get_qs_value("tilex", ReqData),
   TileY = wrq:get_qs_value("tiley", ReqData),
   OSMWayID = wrq:get_qs_value("wayid", "0", ReqData),
   OSMNodeID = wrq:get_qs_value("nodeid", "0", ReqData),
   ZoomParam = list_to_integer(Zoom),
   TileXParam = list_to_integer(TileX),
   TileYParam = list_to_integer(TileY),
   OSMWayIDParam = list_to_integer(OSMWayID),
   OSMNodeIDParam = list_to_integer(OSMNodeID),
   {Status, Res} = trafficapp:fetch({num, ZoomParam, TileXParam, TileYParam}, {OSMWayIDParam, OSMNodeIDParam}),
   lists:flatten(io_lib:format("~p", [Res])).

%% Example curl:
%% curl -v -H "Content-Type:text/plain" "http://127.0.0.1:8080/fetch/num?zoom=14&tilex=34&tiley=45&wayid=12&nodeid=640"
