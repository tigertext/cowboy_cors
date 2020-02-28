%%%-------------------------------------------------------------------
%%% @author anders
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Feb 2020 11:33 AM
%%%-------------------------------------------------------------------
-module(cowboy_cors_utils).
-author("anders").

%% API
-export([token/2, list/2, token_ci/2, nonempty_list/2]).
%% @doc Parse a case-insensitive token.
%%
%% Changes all characters to lowercase.
-spec token_ci(binary(), fun()) -> any().
token_ci(Data, Fun) ->
    token(Data, Fun, ci, <<>>).


token(Data, Fun) ->
    token(Data, Fun, cs, <<>>).

token(<<>>, Fun, _Case, Acc) ->
    Fun(<<>>, Acc);
token(Data = << C, _Rest/binary >>, Fun, _Case, Acc)
    when C =:= $(; C =:= $); C =:= $<; C =:= $>; C =:= $@;
    C =:= $,; C =:= $;; C =:= $:; C =:= $\\; C =:= $";
    C =:= $/; C =:= $[; C =:= $]; C =:= $?; C =:= $=;
    C =:= ${; C =:= $}; C =:= $\s; C =:= $\t;
    C < 32; C =:= 127 ->
    Fun(Data, Acc);
token(<< C, Rest/binary >>, Fun, Case = ci, Acc) ->
    C2 = cowboy_bstr:char_to_lower(C),
    token(Rest, Fun, Case, << Acc/binary, C2 >>);
token(<< C, Rest/binary >>, Fun, Case, Acc) ->
    token(Rest, Fun, Case, << Acc/binary, C >>).

%% @doc Parse a non-empty list of the given type.
-spec nonempty_list(binary(), fun()) -> [any(), ...] | {error, badarg}.
nonempty_list(Data, Fun) ->
    case list(Data, Fun, []) of
        {error, badarg} -> {error, badarg};
        [] -> {error, badarg};
        L -> lists:reverse(L)
    end.

%% @doc Parse a list of the given type.
-spec list(binary(), fun()) -> list() | {error, badarg}.
list(Data, Fun) ->
    case list(Data, Fun, []) of
        {error, badarg} -> {error, badarg};
        L -> lists:reverse(L)
    end.

-spec list(binary(), fun(), [binary()]) -> [any()] | {error, badarg}.
%% From the RFC:
%% <blockquote>Wherever this construct is used, null elements are allowed,
%% but do not contribute to the count of elements present.
%% That is, "(element), , (element) " is permitted, but counts
%% as only two elements. Therefore, where at least one element is required,
%% at least one non-null element MUST be present.</blockquote>
list(Data, Fun, Acc) ->
    whitespace(Data,
        fun (<<>>) -> Acc;
            (<< $,, Rest/binary >>) -> list(Rest, Fun, Acc);
            (Rest) -> Fun(Rest,
                fun (D, I) -> whitespace(D,
                    fun (<<>>) -> [I|Acc];
                        (<< $,, R/binary >>) -> list(R, Fun,
                            [I|Acc]);
                        (_Any) -> {error, badarg}
                    end)
                end)
        end).

%% @doc Skip whitespace.
-spec whitespace(binary(), fun()) -> any().
whitespace(<< C, Rest/binary >>, Fun)
    when C =:= $\s; C =:= $\t ->
    whitespace(Rest, Fun);
whitespace(Data, Fun) ->
    Fun(Data).
