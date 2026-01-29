-module(probnik_qr).

%% API
-export([show/0, payload/0]).

-define(PAIR_TAG, probnik_pair).

-record(qrcode, {version, ecc, dimension, data}).

%% Print an ASCII QR for pairing with Probnik.
show() ->
    Node = node(),
    case Node of
        'nonode@nohost' ->
            io:format("Error: Node not started with distribution enabled.~n"),
            io:format("Start with: iex --sname myapp@localhost --cookie secret -S mix~n"),
            {error, no_distribution};
        _ ->
            PayloadBin = payload(),
            case qr_encode(PayloadBin) of
                {ok, QRCode} ->
                    QR = render_ascii(QRCode),
                    io:format("~s~n", [QR]),
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

render_ascii(#qrcode{dimension = Dim, data = Data}) ->
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
    render_row(Rest, ["##" | Acc]);
render_row(<<0:1, Rest/bits>>, Acc) ->
    render_row(Rest, ["  " | Acc]).

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
