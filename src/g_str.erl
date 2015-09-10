%%%-------------------------------------------------------------------
%%% @author lopiola
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Aug 2015 13:07
%%%-------------------------------------------------------------------
-module(g_str).
-author("lopiola").


% Conversion
-export([to_list/1, to_binary/1, join_to_binary/1]).

% Conversion between unicode and binaries
-export([unicode_list_to_binary/1, binary_to_unicode_list/1]).

% Formatting, escaping and encoding
-export([format/2, format_bin/2, js_escape/1, html_encode/1, url_encode/1, url_decode/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Converts any term to list.
%% @end
%%--------------------------------------------------------------------
-spec to_list(Term :: term()) -> list().
to_list(undefined) -> [];
to_list(Term) when is_list(Term) -> Term;
to_list(Term) when is_binary(Term) -> binary_to_list(Term);
to_list(Term) -> format("~p", [Term]).

%%--------------------------------------------------------------------
%% @doc Converts any term to binary.
%% @end
%%--------------------------------------------------------------------
-spec to_binary(Term :: term()) -> binary().
to_binary(Term) when is_binary(Term) -> Term;
to_binary(Term) -> list_to_binary(to_list(Term)).

%%--------------------------------------------------------------------
%% @doc Joins any terms to a binary.
%% @end
%%--------------------------------------------------------------------
-spec join_to_binary(List :: [term()]) -> binary().
join_to_binary(Terms) ->
    join_to_binary(Terms, <<"">>).

join_to_binary([], Acc) ->
    Acc;

join_to_binary([H | T], Acc) ->
    join_to_binary(T, <<Acc/binary, (to_binary(H))/binary>>).

%%--------------------------------------------------------------------
%% @doc Converts a unicode list to utf8 binary.
%% @end
%%--------------------------------------------------------------------
-spec unicode_list_to_binary(String :: string()) -> binary().
unicode_list_to_binary(String) ->
    unicode:characters_to_binary(String).

%%--------------------------------------------------------------------
%% @doc Converts a utf8 binary to unicode list.
%% @end
%%--------------------------------------------------------------------
-spec binary_to_unicode_list(Binary :: binary()) -> string().
binary_to_unicode_list(Binary) ->
    unicode:characters_to_list(Binary).

%%--------------------------------------------------------------------
%% @doc Escapes all javascript - sensitive characters.
%% @end
%%--------------------------------------------------------------------
-spec format(Format :: string(), Args :: [term()]) -> list().
format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

%%--------------------------------------------------------------------
%% @doc Escapes all javascript - sensitive characters.
%% @end
%%--------------------------------------------------------------------
-spec format_bin(Format :: string(), Args :: [term()]) -> binary().
format_bin(Format, Args) ->
    to_binary(format(Format, Args)).

%%--------------------------------------------------------------------
%% @doc Escapes all javascript - sensitive characters.
%% @end
%%--------------------------------------------------------------------
-spec js_escape(String :: binary() | string()) -> binary().
js_escape(undefined) ->
    <<"">>;
js_escape(Value) when is_list(Value) ->
    js_escape(iolist_to_binary(Value));
js_escape(Value) ->
    js_escape(Value, <<"">>).
js_escape(<<"\\", Rest/binary>>, Acc) ->
    js_escape(Rest, <<Acc/binary, "\\\\">>);
js_escape(<<"\r", Rest/binary>>, Acc) ->
    js_escape(Rest, <<Acc/binary, "\\r">>);
js_escape(<<"\n", Rest/binary>>, Acc) ->
    js_escape(Rest, <<Acc/binary, "\\n">>);
js_escape(<<"\"", Rest/binary>>, Acc) ->
    js_escape(Rest, <<Acc/binary, "\\\"">>);
js_escape(<<"'", Rest/binary>>, Acc) ->
    js_escape(Rest, <<Acc/binary, "\\'">>);
js_escape(<<"<script", Rest/binary>>, Acc) ->
    js_escape(Rest, <<Acc/binary, "&lt;script">>);
js_escape(<<"script>", Rest/binary>>, Acc) ->
    js_escape(Rest, <<Acc/binary, "script&gt;">>);
js_escape(<<C, Rest/binary>>, Acc) ->
    js_escape(Rest, <<Acc/binary, C>>);
js_escape(<<"">>, Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% @doc Performs safe URL encoding.
%% @end
%%--------------------------------------------------------------------
-spec html_encode(String :: binary() | string()) -> binary().
html_encode(List) when is_list(List) ->
    html_encode(to_binary(List));

html_encode(<<"">>) -> <<"">>;
html_encode(<<$<, Rest/binary>>) -> <<"&lt;", (html_encode(Rest))/binary>>;
html_encode(<<$>, Rest/binary>>) -> <<"&gt;", (html_encode(Rest))/binary>>;
html_encode(<<$", Rest/binary>>) -> <<"&quot;", (html_encode(Rest))/binary>>;
html_encode(<<$', Rest/binary>>) -> <<"&#39;", (html_encode(Rest))/binary>>;
html_encode(<<$&, Rest/binary>>) -> <<"&amp;", (html_encode(Rest))/binary>>;
html_encode(<<H, Rest/binary>>) -> <<H, (html_encode(Rest))/binary>>.

%%--------------------------------------------------------------------
%% @doc Performs safe URL encoding
%% @end
%%--------------------------------------------------------------------
-spec url_encode(String :: binary() | string()) -> binary().
url_encode(String) ->
    quote_plus(String).

%%--------------------------------------------------------------------
%% @doc Performs URL-uncoded string decoding
%% @end
%%--------------------------------------------------------------------
-spec url_decode(String :: binary() | string()) -> binary().
url_decode(String) ->
    unquote(String).

%%% CODE BELOW IS FROM MOCHIWEB %%%

%% This is the MIT license.
%%
%% Copyright (c) 2007 Mochi Media, Inc.
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(IS_HEX(C), ((C >= $0 andalso C =< $9) orelse
    (C >= $a andalso C =< $f) orelse
    (C >= $A andalso C =< $F))).
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
    (C >= $A andalso C =< $Z) orelse
    (C >= $0 andalso C =< $9) orelse
    (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
        C =:= $_))).


hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

unhexdigit(C) when C >= $0, C =< $9 -> C - $0;
unhexdigit(C) when C >= $a, C =< $f -> C - $a + 10;
unhexdigit(C) when C >= $A, C =< $F -> C - $A + 10.


quote_plus(Atom) when is_atom(Atom) ->
    quote_plus(atom_to_list(Atom));
quote_plus(Int) when is_integer(Int) ->
    quote_plus(integer_to_list(Int));
quote_plus(Bin) when is_binary(Bin) ->
    quote_plus(binary_to_list(Bin));
quote_plus(String) ->
    quote_plus(String, []).

quote_plus([], Acc) ->
    lists:reverse(Acc);
quote_plus([C | Rest], Acc) when ?QS_SAFE(C) ->
    quote_plus(Rest, [C | Acc]);
quote_plus([$\s | Rest], Acc) ->
    quote_plus(Rest, [$+ | Acc]);
quote_plus([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    quote_plus(Rest, [hexdigit(Lo), hexdigit(Hi), ?PERCENT | Acc]).

%% @spec unquote(string() | binary()) -> string()
%% @doc Unquote a URL encoded string.
unquote(Binary) when is_binary(Binary) ->
    unquote(binary_to_list(Binary));
unquote(String) ->
    qs_revdecode(lists:reverse(String)).

qs_revdecode(S) ->
    qs_revdecode(S, []).

qs_revdecode([], Acc) ->
    Acc;
qs_revdecode([$+ | Rest], Acc) ->
    qs_revdecode(Rest, [$\s | Acc]);
qs_revdecode([Lo, Hi, ?PERCENT | Rest], Acc) when ?IS_HEX(Lo), ?IS_HEX(Hi) ->
    qs_revdecode(Rest, [(unhexdigit(Lo) bor (unhexdigit(Hi) bsl 4)) | Acc]);
qs_revdecode([C | Rest], Acc) ->
    qs_revdecode(Rest, [C | Acc]).
