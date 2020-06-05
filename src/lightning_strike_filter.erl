-module(lightning_strike_filter).

%% Malmo Area
-define(LAT_MAX, 55.65).
-define(LAT_MIN, 55.48).
-define(LON_MAX, 13.23).
-define(LON_MIN, 12.75).

-export([strike_filter/1, strike_filter/2]).

strike_filter(LightningStrikes) ->
    strike_filter(LightningStrikes, #{"lat_min" => ?LAT_MIN, "lat_max" => ?LAT_MAX, "lon_min" => ?LON_MIN, "lon_max" => ?LON_MAX}, []).

strike_filter(LightningStrikes, LatLonMap) ->
    strike_filter(LightningStrikes, LatLonMap, []).

strike_filter([], _LatLonMap, Acc) ->
    {ok, lists:reverse(Acc)};
strike_filter([LightningStrike | Rest],
               #{"lat_min" := LatMin, "lat_max" := LatMax, "lon_min" := LonMin, "lon_max" := LonMax} = LatLonMap, 
               Acc) ->
    #{<<"lat">> := Latitude, <<"lon">> := Longitude} = LightningStrike,
    NewAcc = case {Latitude, Longitude} of
        {Lat, Lon} when ((Lat >= LatMin) and (Lat =< LatMax) and (Lon >= LonMin) and (Lon =< LonMax)) ->
            lists:concat([Acc, [LightningStrike]]);
        {_Lat, _Lon} -> 
            Acc
    end,
    strike_filter(Rest, LatLonMap, NewAcc).