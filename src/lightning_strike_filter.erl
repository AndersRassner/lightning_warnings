%% Stuff
-module(lightning_strike_filter).
%% Malmo Area
%% -define(LAT_MAX, 55.65).
%% -define(LAT_MIN, 55.48).
%% -define(LON_MAX, 13.23).
%% -define(LON_MIN, 12.75).
%% Testing
-define(LAT_MAX, 59.9209).
-define(LAT_MIN, 59.6728).
-define(LON_MAX, 17.2666).
-define(LON_MIN, 16.5573).

-export([strike_filter/1]).

strike_filter(LightningStrikes) ->
    strike_filter(LightningStrikes, []).

strike_filter([], Acc) ->
    {ok, lists:reverse(Acc)};
strike_filter([LightningStrike | Rest], Acc) ->
    #{<<"lat">> := Latitude, <<"lon">> := Longitude} = LightningStrike,
    %%Latitude = maps:get(<<"lat">>, LightningStrike),
    %%Longitude = maps:get(<<"lon">>, LightningStrike),
    io:format("LAT_MAX ~p, LAT_MIN ~p, LON_MAX ~p, LON_MIN ~p\n", [?LAT_MAX, ?LAT_MIN, ?LON_MAX, ?LON_MIN]),
    NewAcc = case {Latitude, Longitude} of
        {Lat, Lon} when ((Lat >= ?LAT_MIN) and (Lat =< ?LAT_MAX) and (Lon >= ?LON_MIN) and (Lon =< ?LON_MAX)) ->
            io:format("Strike with ~p,~p was kept\n", [Lat, Lon]),
            lists:concat([Acc, [LightningStrike]]);
        {Lat, Lon} -> 
            io:format("Strike with ~p,~p was dropped\n", [Lat, Lon]),
            Acc
    end,
    strike_filter(Rest, NewAcc).

%% lightning_warnings:check_for_strikes().