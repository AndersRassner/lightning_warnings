-module(lightning_strike_filter_test).

-include_lib("eunit/include/eunit.hrl").

-define(LAT_MAX, 59.9209).
-define(LAT_MIN, 59.6728).
-define(LON_MAX, 17.2666).
-define(LON_MIN, 16.5573).

strike_filter_with_coordinates_test() ->
    LightningStrikes = get_lightning_strikes(),
    LatLonMap = #{"lat_min" => ?LAT_MIN, "lat_max" => ?LAT_MAX, "lon_min" => ?LON_MIN, "lon_max" => ?LON_MAX},
    {ok, FilteredStrikes} = lightning_strike_filter:strike_filter(LightningStrikes, LatLonMap),
    ?assert(length(FilteredStrikes) =:= 2),
    ok.

strike_filter_without_coordinates_test() ->
    LightningStrikes = get_lightning_strikes(),
    {ok, FilteredStrikes} = lightning_strike_filter:strike_filter(LightningStrikes),
    ?assert(length(FilteredStrikes) =:= 1),
    ok.

get_lightning_strikes() ->
    {ok, FileContent} = file:read_file("truncated_example_data.json"),
    FileContentDecoded = jsx:decode(FileContent, [return_maps]),
    maps:get(<<"values">>, FileContentDecoded).