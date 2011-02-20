%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(rebar_config).

-export([new/0, new/1,
         get/3, get_local/3, get_list/3,
         get_all/2,
         set/3,
         set_global/2, get_global/2,
         is_verbose/0, get_jobs/0]).

-include("rebar.hrl").


%% ===================================================================
%% Public API
%% ===================================================================

new() ->
    #config { dir = rebar_utils:get_cwd(),
              opts = []}.

new(ParentConfig) ->
    %% If we are at the top level we might want to load another rebar.config
    %% We can be certain that we are at the top level if we don't have any
    %% configs yet since if we are at another level we must have some config.
    ConfName = case ParentConfig of
                   {config, _, []} ->
                       rebar_config:get_global(config, "rebar.config");
                   _ ->
                       "rebar.config"
               end,

    %% Load terms from rebar.config, if it exists
    Dir = rebar_utils:get_cwd(),
    ConfigFile = filename:join([Dir, ConfName]),
    Opts = case file:consult(ConfigFile) of
               {ok, Terms} ->
                   %% Found a config file with some terms.
                   
                   %% Replace terms of form {test, ...} with values generated
                   %% by functions in configure.erl
                   Terms2 = apply_platform_tests(Terms),

                   %% We need to
                   %% be able to distinguish between local definitions
                   %% (i.e. from the file in the cwd) and inherited
                   %% definitions. To accomplish this, we use a marker
                   %% in the proplist (since order matters) between
                   %% the new and old defs.
                   Terms2 ++ [local] ++
                       [Opt || Opt <- ParentConfig#config.opts, Opt /= local];
               {error, enoent} ->
                   [local] ++
                       [Opt || Opt <- ParentConfig#config.opts, Opt /= local];
               Other ->
                   ?ABORT("Failed to load ~s: ~p\n", [ConfigFile, Other])
           end,

    #config { dir = Dir, opts = Opts }.

get(Config, Key, Default) ->
    proplists:get_value(Key, Config#config.opts, Default).

get_list(Config, Key, Default) ->
    get(Config, Key, Default).

get_local(Config, Key, Default) ->
    proplists:get_value(Key, local_opts(Config#config.opts, []), Default).

get_all(Config, Key) ->
    proplists:get_all_values(Key, Config#config.opts).

set(Config, Key, Value) ->
    Opts = proplists:delete(Key, Config#config.opts),
    Config#config { opts = [{Key, Value} | Opts] }.

set_global(jobs=Key, Value) when is_list(Value) ->
    set_global(Key, list_to_integer(Value));
set_global(jobs=Key, Value) when is_integer(Value) ->
    application:set_env(rebar_global, Key, erlang:max(1,Value));
set_global(Key, Value) ->
    application:set_env(rebar_global, Key, Value).

get_global(Key, Default) ->
    case application:get_env(rebar_global, Key) of
        undefined ->
            Default;
        {ok, Value} ->
            Value
    end.

is_verbose() ->
    get_global(verbose, "0") =:= "1".

get_jobs() ->
    get_global(jobs, 3).

%% ===================================================================
%% Internal functions
%% ===================================================================

local_opts([], Acc) ->
    lists:reverse(Acc);
local_opts([local | _Rest], Acc) ->
    lists:reverse(Acc);
local_opts([Item | Rest], Acc) ->
    local_opts(Rest, [Item | Acc]).

apply_platform_tests(Terms) ->
    case find_platform_tests(Terms, []) of
        [] ->
            Terms;
        Tests ->
            case cache_status() of
                create ->
                    ?CONSOLE("Creating platform configuration cache.~n", []),
                    ok = create_platform_cache(Tests);
                update ->
                    ?CONSOLE("Updating platform configuration cache.~n", []),
                    ok = file:delete("rebar.cache"),
                    ok = create_platform_cache(Tests);
                ok ->
                    ok
            end,
            replace_platform_values(Terms)
    end.

find_platform_tests([], Acc) ->
    Acc;
find_platform_tests([{test, Fun, Args} | Rest], Acc) ->
    Acc2 = case lists:member({Fun, Args}, Acc) of
               true ->
                   Acc;
               false ->
                   [{Fun, Args} | Acc]
           end,
    find_platform_tests(Rest, Acc2);
find_platform_tests([Term | Rest], Acc) when is_list(Term) ->
    find_platform_tests(Term ++ Rest, Acc);
find_platform_tests([Term | Rest], Acc) when is_tuple(Term) ->
    find_platform_tests(tuple_to_list(Term) ++ Rest, Acc);
find_platform_tests([_Term | Rest], Acc) ->
    find_platform_tests(Rest, Acc).

cache_status() ->
    Cache = filelib:last_modified("rebar.cache"), %% 0 if not found
    RebarConfig = filelib:last_modified("rebar.config"),
    ConfigureErl = filelib:last_modified("configure.erl"),
    if
        Cache == 0 -> create;
        Cache < RebarConfig -> update;
        Cache < ConfigureErl -> update;
        true -> ok
    end.

create_platform_cache(Tests) ->
    case compile:file("configure", [binary, debug_info, report]) of
        {ok, _, Bin} ->
            {module, _} = code:load_binary(configure, "configure", Bin),
            Vals = [io_lib:format("~w.~n", [{{Test, Args}, 
                                             erlang:apply(configure, Test, Args)}])
                    || {Test, Args} <- Tests],
            ok = file:write_file("rebar.cache", Vals),
            true = code:delete(configure),
            ok;
        _ -> 
            ?ABORT("Failed to compile platform tests.", [])
    end.

replace_platform_values(ConfigTerms) ->
    case file:consult("rebar.cache") of
        {ok, CacheTerms} ->
            replace_platform_values(ConfigTerms, CacheTerms, []);
        _ ->
            ?ABORT("Failed to read rebar.cache.", [])
    end.

replace_platform_values([], _, Acc) ->
    lists:reverse(Acc);
replace_platform_values([{test, Fun, Args} | Rest], ConfigTerms, Acc) ->
    replace_platform_values(Rest, ConfigTerms,
                            [proplists:get_value({Fun, Args}, ConfigTerms)
                             | Acc]);
replace_platform_values([Term | Rest], ConfigTerms, Acc) when is_list(Term) ->
    List = replace_platform_values(Term, ConfigTerms, []),
    replace_platform_values(Rest, ConfigTerms,
                            [List | Acc]);
replace_platform_values([Term | Rest], ConfigTerms, Acc) when is_tuple(Term) ->
    Tuple = list_to_tuple(
              replace_platform_values(tuple_to_list(Term), ConfigTerms, [])),
    replace_platform_values(Rest, ConfigTerms, [Tuple | Acc]);
replace_platform_values([Term | Rest], ConfigTerms, Acc) ->
    replace_platform_values(Rest, ConfigTerms, [Term | Acc]).
