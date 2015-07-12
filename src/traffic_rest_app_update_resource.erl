-module(traffic_rest_app_update_resource).
-export([
  init/1,
  allowed_methods/2,
  process_post/2
%%   content_types_provided/2,
%%   to_text/2,
%%   to_xml/2
]).

-record(context, {
  type,
  zoom,
  tilex,
  tiley,
  osmwayID,
  osmnodeID,
  lat,
  lon
}).

-include_lib("erlsom/src/erlsom_parse.hrl").
-include_lib("webmachine/include/webmachine.hrl").
-include_lib("erlsom/src/erlsom.hrl").
-include("../include/update_record.hrl").

-spec init(list()) -> {ok, term()}.
init(Config) ->
  %%{{trace, "/tmp"}, Config}.  %% debugging code
  {ok, Config}.             %% regular code

%% content_types_provided(ReqData, Context) ->
%%   {[{"text/plain", to_text}, {"text/xml", to_xml}, {"application/xml", to_xml}], ReqData, Context}.
%%
%% to_text(ReqData, Context) ->
%%   Path = wrq:disp_path(ReqData),
%%   {Body, Context} = case Path of
%%                       "num" ->
%%                         get_params_num(ReqData, Context);
%%                       "deg" ->
%%                         get_params_deg(ReqData, Context);
%%                       _Other ->
%%                         io_lib:format("Unsupported operation: ~p~n", [Path])
%%                     end,
%%   {Body, ReqData, Context}.
%%
%% to_xml(ReqData, Context) ->
%%   Path = wrq:disp_path(ReqData),
%%   {Body, Context} = case Path of
%%                       "num" ->
%%                         get_params_num(ReqData, Context);
%%                       "deg" ->
%%                         get_params_deg(ReqData, Context);
%%                       _Other ->
%%                         io_lib:format("Unsupported operation: ~p~n", [Path])
%%                     end,
%%   {Body, ReqData, Context}.
%%
%% get_params_deg(ReqData, Context) ->
%%   Zoom = wrq:get_qs_value("zoom", ReqData),
%%   Lat = wrq:get_qs_value("lat", ReqData),
%%   Lon = wrq:get_qs_value("lon", ReqData),
%%   OSMWayID = wrq:get_qs_value("wayid", "0", ReqData),
%%   OSMNodeID = wrq:get_qs_value("nodeid", "0", ReqData),
%%   {"Ok", Context#context{type = deg, zoom = Zoom, osmwayID = OSMWayID, osmnodeID = OSMNodeID, lat = Lat, lon = Lon}}.
%%
%% get_params_num(ReqData, Context) ->
%%   Zoom = wrq:get_qs_value("zoom", ReqData),
%%   TileX = wrq:get_qs_value("tilex", ReqData),
%%   TileY = wrq:get_qs_value("tiley", ReqData),
%%   OSMWayID = wrq:get_qs_value("wayid", "0", ReqData),
%%   OSMNodeID = wrq:get_qs_value("nodeid", "0", ReqData),
%%   {ok, _Res} = trafficapp:fetch({num, list_to_integer(Zoom), list_to_integer(TileX), list_to_integer(TileY)},
%%     {list_to_integer(OSMWayID), list_to_integer(OSMNodeID)}, Context).


allowed_methods(Req, State) ->
  {['POST', 'PUT'], Req, State}.

process_post(ReqData, Context) ->
  PayloadXml = wrq:req_body(ReqData),

  %% example update.xml
  %%   <?xml version="1.0" encoding="UTF-8"?>
  %%   <osm version="0.6" generator="OpenStreetMap server">
  %%   <api>
  %%   <version minimum="0.1" maximum="0.1"/>
  %%   <tile_num level="14" x="34" y= "45"/>
  %%   <nodes maximum="2000"/>
  %%   <way id="500"/>  <- optional, but required if node id given ->
  %%   <node id="5000"/>  <- optional ->
  %%   </api>
  %%   </osm>

  %% curl -v -v  "http://localhost:8080/update/num" -d @update.xml

  %{Status, Res} = trafficapp:update({num, list_to_integer(Zoom), list_to_integer(TileX), list_to_integer(TileY)},
  %  {list_to_integer(OSMWayID), list_to_integer(OSMNodeID)}, Payload),

  %Erlsom model generated from DTD converetd with Visual Studio to XSD
  % this model needs to be updated when the dtd is changed.
  Model = #model{
    tps =
    [#type{
      nm = '_document', tp = sequence,
      els =
      [#el{alts =
      [#alt{
        tag = osm_traffic, tp = osm_traffic, nxt = [],
        mn = 1, mx = 1, rl = true, anyInfo = undefined},
        #alt{
          tag = traffic_api, tp = traffic_api, nxt = [],
          mn = 1, mx = 1, rl = true, anyInfo = undefined},
        #alt{
          tag = version, tp = version, nxt = [], mn = 1,
          mx = 1, rl = true, anyInfo = undefined},
        #alt{
          tag = tile_num, tp = tile_num, nxt = [], mn = 1,
          mx = 1, rl = true, anyInfo = undefined},
        #alt{
          tag = tile_deg, tp = tile_deg, nxt = [], mn = 1,
          mx = 1, rl = true, anyInfo = undefined},
        #alt{
          tag = waynodes, tp = waynodes, nxt = [], mn = 1,
          mx = 1, rl = true, anyInfo = undefined},
        #alt{
          tag = noderef, tp = noderef, nxt = [], mn = 1,
          mx = 1, rl = true, anyInfo = undefined},
        #alt{
          tag = wayref, tp = wayref, nxt = [], mn = 1,
          mx = 1, rl = true, anyInfo = undefined},
        #alt{
          tag = payload, tp = payload, nxt = [], mn = 1,
          mx = 1, rl = simple, anyInfo = undefined}],
        mn = 1, mx = 1, nillable = undefined, nr = 1}],
      atts = [], anyAttr = undefined, nillable = undefined, nr = 1,
      mn = 1, mx = 1, mxd = false, typeName = undefined},
      #type{
        nm = payload, tp = sequence,
        els =
        [#el{alts =
        [#alt{
          tag = payload,
          tp = {'#PCDATA', char},
          nxt = [], mn = 1, mx = 1, rl = true,
          anyInfo = undefined}],
          mn = 1, mx = 1, nillable = undefined, nr = 1}],
        atts = [], anyAttr = undefined, nillable = undefined, nr = 2,
        mn = 1, mx = 1, mxd = false, typeName = undefined},
      #type{
        nm = wayref, tp = sequence, els = [],
        atts = [#att{nm = id, nr = 1, opt = false, tp = char}],
        anyAttr = undefined, nillable = undefined, nr = 2, mn = 1,
        mx = 1, mxd = undefined, typeName = undefined},
      #type{
        nm = noderef, tp = sequence, els = [],
        atts = [#att{nm = id, nr = 1, opt = false, tp = char}],
        anyAttr = undefined, nillable = undefined, nr = 2, mn = 1,
        mx = 1, mxd = undefined, typeName = undefined},
      #type{
        nm = waynodes, tp = sequence, els = [],
        atts = [#att{nm = maximum, nr = 1, opt = false, tp = char}],
        anyAttr = undefined, nillable = undefined, nr = 2, mn = 1,
        mx = 1, mxd = undefined, typeName = undefined},
      #type{
        nm = tile_deg, tp = sequence, els = [],
        atts =
        [#att{nm = level, nr = 1, opt = false, tp = char},
          #att{nm = lat, nr = 2, opt = false, tp = char},
          #att{nm = lon, nr = 3, opt = false, tp = char}],
        anyAttr = undefined, nillable = undefined, nr = 4, mn = 1,
        mx = 1, mxd = undefined, typeName = undefined},
      #type{
        nm = tile_num, tp = sequence, els = [],
        atts =
        [#att{nm = level, nr = 1, opt = false, tp = char},
          #att{nm = x, nr = 2, opt = false, tp = char},
          #att{nm = y, nr = 3, opt = false, tp = char}],
        anyAttr = undefined, nillable = undefined, nr = 4, mn = 1,
        mx = 1, mxd = undefined, typeName = undefined},
      #type{
        nm = version, tp = sequence, els = [],
        atts =
        [#att{nm = minimum, nr = 1, opt = false, tp = char},
          #att{nm = maximum, nr = 2, opt = false, tp = char}],
        anyAttr = undefined, nillable = undefined, nr = 3, mn = 1,
        mx = 1, mxd = undefined, typeName = undefined},
      #type{
        nm = traffic_api, tp = sequence,
        els =
        [#el{alts =
        [#alt{
          tag = version, tp = version, nxt = [], mn = 1,
          mx = 1, rl = true, anyInfo = undefined}],
          mn = 1, mx = 1, nillable = undefined, nr = 1},
          #el{alts =
          [#alt{
            tag = tile_num, tp = tile_num, nxt = [], mn = 1,
            mx = 1, rl = true, anyInfo = undefined},
            #alt{
              tag = tile_deg, tp = tile_deg, nxt = [], mn = 1,
              mx = 1, rl = true, anyInfo = undefined}],
            mn = 1, mx = 1, nillable = undefined, nr = 2},
          #el{alts =
          [#alt{
            tag = waynodes, tp = waynodes, nxt = [], mn = 1,
            mx = 1, rl = true, anyInfo = undefined}],
            mn = 1, mx = 1, nillable = undefined, nr = 3},
          #el{alts = [#alt{tag = wayref, tp = 'traffic_api/SEQ1',
            nxt = [], mn = 1, mx = 1, rl = false,
            anyInfo = undefined}],
            mn = 0, mx = 1, nillable = undefined, nr = 4},
          #el{alts = [#alt{tag = payload,
            tp = {'#PCDATA', char},
            nxt = [], mn = 1, mx = 1, rl = true,
            anyInfo = undefined}],
            mn = 1, mx = 1, nillable = undefined, nr = 5}],
        atts = [], anyAttr = undefined, nillable = undefined, nr = 6,
        mn = 1, mx = 1, mxd = undefined, typeName = undefined},
      #type{
        nm = 'traffic_api/SEQ1', tp = sequence,
        els =
        [#el{alts =
        [#alt{
          tag = wayref, tp = wayref, nxt = [], mn = 1,
          mx = 1, rl = true, anyInfo = undefined}],
          mn = 1, mx = 1, nillable = undefined, nr = 1},
          #el{alts =
          [#alt{
            tag = noderef, tp = noderef, nxt = [], mn = 1,
            mx = 1, rl = true, anyInfo = undefined}],
            mn = 0, mx = 1, nillable = undefined, nr = 2}],
        atts = [], anyAttr = undefined, nillable = undefined, nr = 3,
        mn = 1, mx = 1, mxd = undefined, typeName = undefined},
      #type{
        nm = osm_traffic, tp = sequence,
        els =
        [#el{alts =
        [#alt{
          tag = traffic_api, tp = traffic_api, nxt = [],
          mn = 1, mx = 1, rl = true, anyInfo = undefined}],
          mn = 1, mx = 1, nillable = undefined, nr = 3}],
        atts =
        [#att{nm = version, nr = 1, opt = true, tp = char},
          #att{nm = generator, nr = 2, opt = true, tp = char}],
        anyAttr = undefined, nillable = undefined, nr = 4, mn = 1,
        mx = 1, mxd = undefined, typeName = undefined}],
    nss =
    [#ns{uri = "http://tempuri.org/update", prefix = undefined},
      #ns{uri = "http://www.w3.org/2001/XMLSchema",
        prefix = "xsd"}],
    tns = "http://tempuri.org/update", th = []},
  {ok, Result, _} = erlsom:scan(PayloadXml, Model),

  Traffic_api = Result#osm_traffic.traffic_api,

  _Version = Traffic_api#traffic_api.version,
  Choice = Traffic_api#traffic_api.choice,
  Waynodes = Traffic_api#traffic_api.waynodes,
  Traffic_api_SEQ1 = Traffic_api#traffic_api.'traffic_api/SEQ1',
  Payload = Traffic_api#traffic_api.payload,

  _Maximum = Waynodes#waynodes.maximum,

  {Level_num, X, Y} = case is_record(Choice, tile_num) of
                        true ->
                          {Choice#tile_num.level, Choice#tile_num.x, Choice#tile_num.y};
                        false ->
                          {undefined, undefined, undefined}
                      end,

  {Level_deg, Lat, Lon} = case is_record(Choice, tile_deg) of
                            true ->
                              {Choice#tile_deg.level, Choice#tile_deg.lat, Choice#tile_deg.lon};
                            false ->
                              {undefined, undefined, undefined}
                          end,

  Traffic_api_SEQ1 = Traffic_api#traffic_api.'traffic_api/SEQ1',

  {Wayref_id, Noderef_id} = case Traffic_api_SEQ1 of
                              undefined ->
                                {0, 0};
                              #'traffic_api/SEQ1'{} ->
                                case Traffic_api_SEQ1#'traffic_api/SEQ1'.noderef of
                                  undefined ->
                                    {Traffic_api_SEQ1#'traffic_api/SEQ1'.wayref#wayref.id,
                                      0};
                                  _Other ->
                                    {Traffic_api_SEQ1#'traffic_api/SEQ1'.wayref#wayref.id,
                                      Traffic_api_SEQ1#'traffic_api/SEQ1'.noderef#noderef.id}
                                end
                            end,
  {true, ReqData2, Context2} = case is_record_type(Choice) of
                                 tile_num ->
                                   {ok, _Res} = update({num, list_to_integer(Level_num), list_to_integer(X), list_to_integer(Y)},
                                     {Wayref_id, Noderef_id}, Payload),
                                   ReqData1 = wrq:set_resp_header("location", ["/upload?msg=", PayloadXml], ReqData),
                                   {true, ReqData1, Context};
                                 tile_deg ->
                                   {ok, _Res} = update({deg, Level_deg, Lat, Lon},{Wayref_id, Noderef_id}, Payload),
                                   ReqData1 = wrq:set_resp_header("location", ["/upload?msg=", PayloadXml], ReqData),
                                   {true, ReqData1, Context};
                                 false ->
                                   ReqData1 = wrq:set_resp_header("location", ["/upload?msg=", "Error neighter tile_num nor tile_deg"], ReqData),
                                   {true, ReqData1, Context}
                               end,
  {true, ReqData2, Context2}.

is_record_type(#tile_num{}) -> tile_num;
is_record_type(#tile_deg{}) -> tile_deg;
is_record_type(_) -> false.

update({num, Level_numParam, XParam, YParam}, {Wayref_idParam, Noderef_idParam}, Payload) when Wayref_idParam ==0, Noderef_idParam ==0->
  trafficapp:update({num, list_to_integer(Level_numParam), list_to_integer(XParam), list_to_integer(YParam)},
    {list_to_integer(Wayref_idParam), list_to_integer(Noderef_idParam)}, Payload);

update({num, Level_numParam, XParam, YParam}, {Wayref_idParam, Noderef_idParam}, Payload) when Wayref_idParam ==0->
  trafficapp:update({num, list_to_integer(Level_numParam), list_to_integer(XParam), list_to_integer(YParam)},
    {list_to_integer(Wayref_idParam), list_to_integer(Noderef_idParam)}, Payload);

update({num, Level_numParam, XParam, YParam}, {Wayref_idParam, Noderef_idParam}, Payload) ->
  trafficapp:update({num, list_to_integer(Level_numParam), list_to_integer(XParam), list_to_integer(YParam)},
    {list_to_integer(Wayref_idParam), list_to_integer(Noderef_idParam)}, Payload);


update({deg, Level_degParam, LatParam, LonParam}, {Wayref_idParam, Noderef_idParam}, Payload) when Wayref_idParam ==0, Noderef_idParam ==0->
  trafficapp:update({deg, list_to_integer(Level_degParam), list_to_integer(LatParam), list_to_integer(LonParam)}, Payload);


update({deg, Level_degParam, LatParam, LonParam}, {Wayref_idParam, Noderef_idParam}, Payload) when Noderef_idParam ==0->
  trafficapp:update({deg, list_to_integer(Level_degParam), list_to_integer(LatParam), list_to_integer(LonParam)},
        {list_to_integer(Wayref_idParam)}, Payload);

update({deg, Level_degParam, LatParam, LonParam}, {Wayref_idParam, Noderef_idParam}, Payload)->
  trafficapp:update({deg, list_to_integer(Level_degParam), list_to_integer(LatParam), list_to_integer(LonParam)},
    {list_to_integer(Wayref_idParam), list_to_integer(Noderef_idParam)}, Payload).