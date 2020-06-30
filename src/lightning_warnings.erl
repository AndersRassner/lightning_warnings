-module(lightning_warnings).

-export([check_for_strikes/0]).

check_for_strikes() ->
    SkaneLatLon =
        #{"lat_min" => 55.29,
          "lat_max" => 56.42,
          "lon_min" => 12.35,
          "lon_max" => 14.45},
    Values = get_lightning_strikes:get_strikes(smhi),
    FileContentDecoded = jsx:decode(Values, [return_maps]),
    LightningStrikes = maps:get(<<"values">>, FileContentDecoded),
    {ok, MalmoFilteredStrikes} = lightning_strike_filter:strike_filter(LightningStrikes),
    {ok, SkaneFilteredStrikes} = lightning_strike_filter:strike_filter(LightningStrikes, SkaneLatLon),
    {ok, {{Year, Month, Day}, {Hour,Minute,Second}}} = lightning_strike_filter:get_time_of_latest_strike(SkaneFilteredStrikes),
    DateTimeString = lists:flatten(io_lib:format("~2..0B:~2..0B:~2..0B ~4..0B-~2..0B-~2..0B", [Hour, Minute, Second, Year, Month, Day])),
    io:format("Out of ~p lightning strikes, ~p were within the Default area\n",
        [length(LightningStrikes), length(MalmoFilteredStrikes)]),
    io:format("Out of ~p lightning strikes, ~p were within the Skane area, with the latest strike occuring at ~s\n",
        [length(LightningStrikes), length(SkaneFilteredStrikes), DateTimeString]).
