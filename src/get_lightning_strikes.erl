-module(get_lightning_strikes).

-export([get_strikes/1]).

get_strikes(smhi) ->
    get_strikes_smhi();
get_strikes(Unknown) ->
    {error, "Can't get strikes from ~p", [Unknown]}.

get_strikes_smhi() ->
    {{Year, Month, Day}, _Time} = calendar:local_time(),
    Url = "https://opendata-download-lightning.smhi.se/api/version/latest/year/" ++ integer_to_list(Year)
          ++ "/month/" ++ integer_to_list(Month) ++ "/day/" ++ integer_to_list(Day) ++ "/data.json",
    {ok, Response} = httpc:request(Url),
    {_HttpStatus, _Header, Values} = Response,
    list_to_binary(Values).