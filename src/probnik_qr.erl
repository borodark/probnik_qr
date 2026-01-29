-module(probnik_qr).

%% API
-export([show/0, payload/0]).

-define(PAIR_TAG, probnik_pair).

%% Local copy of the record used by komone/qrcode.
-record(qrcode, {version, ecc, dimension, data}).

%% Print an ANSI ASCII QR for pairing with Probnik.
show() ->
    Node = node(),
    case Node of
        'nonode@nohost' ->
            io:format("Error: Node not started with distribution enabled.~n"),
            io:format("Start with: iex --sname myapp@localhost --cookie secret -S mix~n"),
            {error, no_distribution};
        _ ->
            case qr_encode(payload()) of
                {ok, QRCode} ->
                    io:put_chars(render_ansi(QRCode)),
                    io:format("Node: ~p~n", [Node]),
                    io:format("Mode: ~p~n", [detect_name_mode(Node)]),
                    ok;
                {error, Reason} ->
                    io:format("Failed to render QR (~p).~n", [Reason]),
                    {error, Reason}
            end
    end.

%% Returns the pairing payload as a binary.
payload() ->
    Node = node(),
    Cookie = erlang:get_cookie(),
    Mode = detect_name_mode(Node),
    Term = {?PAIR_TAG, Node, Cookie, [{mode, Mode}]},
    iolist_to_binary(io_lib:format("~p", [Term])).

%% Internal
qr_encode(PayloadBin) ->
    case code:which(qrcode) of
        non_existing ->
            {error, no_qrcode_module};
        _ ->
            try
                {ok, qrcode:encode(PayloadBin)}
            catch
                Class:Reason ->
                    {error, {Class, Reason}}
            end
    end.

render_ansi(#qrcode{dimension = Dim, data = Data}) ->
    render_rows(Data, Dim, []).

render_rows(<<>>, _Dim, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
render_rows(Bits, Dim, Acc) ->
    <<Row:Dim/bits, Rest/bits>> = Bits,
    RowStr = render_row(Row, []),
    render_rows(Rest, Dim, [RowStr, "\n" | Acc]).

render_row(<<>>, Acc) ->
    lists:reverse(Acc);
render_row(<<1:1, Rest/bits>>, Acc) ->
    render_row(Rest, ["\e[40m  \e[0m" | Acc]);
render_row(<<0:1, Rest/bits>>, Acc) ->
    render_row(Rest, ["\e[0;107m  \e[0m" | Acc]).

detect_name_mode(Node) when is_atom(Node) ->
    detect_name_mode(atom_to_list(Node));
detect_name_mode(Node) when is_list(Node) ->
    case string:split(Node, "@", all) of
        [_, Host] ->
            case string:find(Host, ".") of
                nomatch -> shortnames;
                _ -> longnames
            end;
        _ ->
            shortnames
    end.
