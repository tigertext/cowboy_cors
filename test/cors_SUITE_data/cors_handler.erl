-module(cors_handler).

-export([init/2]).

init(Req, _) ->
    Cors = maps:get(cowboy_cors, Req, undefined),
    case Cors of
        {stop, Code} ->
            cowboy_req:reply(Code, Req),
            {ok, Req, undefined_state};
        _ ->
            Headers = #{<<"X-Exposed">> => <<"exposed">>, <<"X-Hidden">> => <<"hidden">>},
            Req1 = cowboy_req:reply(204, Headers, [], Req),
            {ok, Req1, undefined_state}
    end.

