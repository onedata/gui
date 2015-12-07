%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Developer modules that allows for easy recompilation and reload of
%%% erlang modules, erlyDTL templates and static GUI files.
%%% @end
%%%-------------------------------------------------------------------
-module(sync).
-author("Lukasz Opiola").

%% ETS name that holds md5 checksums of files
-define(MD5_ETS, md5_ets).

%% Predefined file with config (location relative to including project root).
-define(GUI_CONFIG_LOCATION, "rel/gui.config").

%% API
-export([reset/0, recompile/1, recompile/2, recompile/3]).

%%%===================================================================
%%% API
%%%===================================================================

reset() ->
    % ForceClear: true
    ensure_ets(true).


recompile(ProjectDir) ->
    recompile(ProjectDir, [], []).

recompile(ProjectDir, DirsToRecompile) ->
    recompile(ProjectDir, DirsToRecompile, []).

recompile(ProjectDir, DirsToRecompileArg, IncludeDirsArg) ->
    % Make sure ets exists. ForceClear: false.
    ensure_ets(false),
    EnsureListOfLists = fun(List) ->
        case List of
            [] -> [];
            [H | _] when is_list(H) -> DirsToRecompileArg;
            Other -> [Other]
        end
    end,
    % Make sure args are in proper format
    DirsToRecompile = EnsureListOfLists(DirsToRecompileArg),
    IncludeDirs = EnsureListOfLists(IncludeDirsArg),

    % Check for gui.config. If exists, automatically update GUI files.
    UpdateGUIFilesRes =
        case get_gui_config(ProjectDir) of
            {ok, GuiConfig} ->
                info_msg("gui.config found. Updating GUI files..."),
                SrcGuiDir = proplists:get_value(source_gui_dir, GuiConfig),
                ResErl = update_erl_files(ProjectDir, [SrcGuiDir], IncludeDirs),
                ResStatic = update_gui_static_files(ProjectDir, GuiConfig),
                ResErl andalso ResStatic;
            _ ->
                true
        end,

    % Recompile erl files. If gui.config exists, GUI erl files will
    % be recompiled automatically.
    UpdateErlFilesRes = update_erl_files(
        ProjectDir, DirsToRecompile, IncludeDirs),

    % Check the results.
    case UpdateGUIFilesRes andalso UpdateErlFilesRes of
        true ->
            info_msg("Success!"),
            ok;
        false ->
            info_msg("There were errors."),
            error
    end.


update_erl_files(ProjectDir, DirsToRecompile, UserIncludes) ->
    % Resolve list of files to recompile
    FilesToCheck = lists:foldl(
        fun(DirPath, Acc) ->
            Files = find_all_files(
                filename:join(ProjectDir, DirPath), "*.erl", false),
            Files ++ Acc
        end, [], DirsToRecompile),

    % Resolve all paths to includes
    ProjIncludes = [filename:join(ProjectDir, "include")],
    Deps = find_all_dirs(filename:join(ProjectDir, "deps"), false),
    DepsIncludes = lists:map(
        fun(DepPath) ->
            filename:join(DepPath, "include")
        end, Deps),
    AllIncludes = lists:map(
        fun(DepPath) ->
            {i, DepPath}
        end, ProjIncludes ++ DepsIncludes ++ UserIncludes),

    % Do the recompilation
    _Result = lists:foldl(fun(File, AccSuccess) ->
        % Return true only if AccSuccess is true (all previous compilations
        % succeeded) and current compilation succeeds
        AccSuccess andalso update_erl_file(File, AllIncludes ++ [report])
    end, true, FilesToCheck).


update_erl_file(File, CompileOpts) ->
    CurrentMD5 = file_md5(File),
    case should_update(File, CurrentMD5) of
        false ->
            true;
        true ->
            case compile:file(File, CompileOpts) of
                {ok, ModuleName} ->
                    code:purge(ModuleName),
                    code:load_file(ModuleName),
                    update_file_md5(File, CurrentMD5),
                    info_msg("Compiled:  ~s", [filename:basename(File)]),
                    true;
                _ ->
                    false
            end
    end.


update_gui_static_files(ProjectDir, GuiConfig) ->
    RelaseStaticFilesDir = proplists:get_value(
        release_static_files_dir, GuiConfig),
    SourceGuiDir = filename:join(
        [ProjectDir, proplists:get_value(source_gui_dir, GuiConfig)]),

    % Returns tuples with source file path, and target file path but relative to
    % RelaseStaticFilesDir.
    SourceFileMappings = lists:map(
        fun(File) ->
            {filename:join([SourceGuiDir, File]), File}
        end, find_all_files(SourceGuiDir, "*", true)),

    _Result = lists:foldl(fun({SourceFilePath, FileName}, AccSuccess) ->
        CurrentSuccess =
            case filename:extension(FileName) of
                ".erl" ->
                    % Do not copy erl files
                    true;
                ".coffee" ->
                    % Compile coffee files, place js in release
                    update_coffee_script(SourceFilePath,
                        RelaseStaticFilesDir, FileName);
                ".hbs" ->
                    % Precompile handlebars files, place js in release
                    update_handlebars_template(SourceFilePath,
                        RelaseStaticFilesDir, FileName);
                ".html" ->
                    % Copy html files to static files root
                    update_static_file(SourceFilePath,
                        RelaseStaticFilesDir,
                        filename:basename(SourceFilePath));
                _ ->
                    % Copy all other files 1:1 (path-wise)
                    update_static_file(SourceFilePath,
                        RelaseStaticFilesDir, FileName)
            end,
        % Return true only if AccSuccess is true (all previous updates
        % succeeded) and current update succeeds
        AccSuccess andalso CurrentSuccess
    end, true, SourceFileMappings).


update_static_file(SourceFile, RelaseStaticFilesDir, FileName) ->
    TargetPath = filename:join(RelaseStaticFilesDir, FileName),
    CurrentMD5 = file_md5(SourceFile),
    case should_update(SourceFile, CurrentMD5) of
        false ->
            true;
        true ->
            case shell_cmd(["mkdir -p", filename:dirname(TargetPath)]) of
                [] ->
                    case shell_cmd(["cp -f", SourceFile, TargetPath]) of
                        [] ->
                            info_msg("Updated:   ~s", [abs_path(FileName)]),
                            update_file_md5(SourceFile, CurrentMD5),
                            true;
                        Other1 ->
                            info_msg("Cannot copy ~s: ~s", [FileName, Other1]),
                            false
                    end;
                Other2 ->
                    info_msg("Cannot create dir ~s: ~s",
                        [filename:dirname(FileName), Other2]),
                    false
            end
    end.


update_coffee_script(SourceFile, RelaseStaticFilesDir, FileName) ->
    TargetPath = filename:join(RelaseStaticFilesDir, FileName),
    TargetDir = filename:dirname(TargetPath),
    CurrentMD5 = file_md5(SourceFile),
    case should_update(SourceFile, CurrentMD5) of
        false ->
            true;
        true ->
            case shell_cmd(["mkdir -p", TargetDir]) of
                [] ->
                    case shell_cmd(
                        ["coffee", "-o", TargetDir, "-c", SourceFile]) of
                        [] ->
                            JSFile = filename:rootname(FileName) ++ ".js",
                            update_file_md5(SourceFile, CurrentMD5),
                            info_msg("Compiled:  ~s -> ~s",
                                [abs_path(FileName), abs_path(JSFile)]),
                            true;
                        Other ->
                            info_msg("Cannot compile ~s: ~s",
                                [SourceFile, Other]),
                            false
                    end;
                Other2 ->
                    info_msg("Cannot create dir ~s: ~s", [TargetDir, Other2]),
                    false
            end
    end.


update_handlebars_template(SourceFile, RelaseStaticFilesDir, FileName) ->
    TargetFileName = filename:rootname(FileName) ++ ".js",
    TargetPath = filename:join([RelaseStaticFilesDir, TargetFileName]),
    TargetDir = filename:dirname(TargetPath),
    CurrentMD5 = file_md5(SourceFile),
    case should_update(SourceFile, CurrentMD5) of
        false ->
            true;
        true ->
            case shell_cmd(["mkdir -p", TargetDir]) of
                [] ->
                    case shell_cmd(
                        ["ember-precompile", SourceFile, "-f", TargetPath]) of
                        [] ->
                            update_file_md5(SourceFile, CurrentMD5),
                            info_msg("Compiled:  ~s -> ~s",
                                [abs_path(FileName), abs_path(TargetFileName)]),
                            true;
                        Other ->
                            info_msg("Cannot compile ~s: ~s",
                                [SourceFile, Other]),
                            false
                    end;
                Other2 ->
                    info_msg("Cannot create dir ~s: ~s", [TargetDir, Other2]),
                    false
            end
    end.


get_gui_config(ProjectDir) ->
    GuiConfigPath = filename:join([ProjectDir, ?GUI_CONFIG_LOCATION]),
    case file:consult(GuiConfigPath) of
        {ok, GuiConfig} -> {ok, GuiConfig};
        {error, enoent} -> {error, enoent}
    end.


%--------------------------------------------------------------------
%% @doc
%% @private
%% Performs a shell call given a list of arguments.
%% @end
%%--------------------------------------------------------------------
-spec shell_cmd([string()]) -> string().
shell_cmd(List) ->
    os:cmd(string:join(List, " ")).


abs_path(FilePath) ->
    filename:absname_join("/", FilePath).


ensure_ets(ForceClear) ->
    case ets:info(?MD5_ETS) of
        undefined ->
            ets:new(?MD5_ETS,
                [set, protected, named_table, {read_concurrency, true}]),
            info_msg("Started new ETS table to track changes in files.");
        _ ->
            case ForceClear of
                true ->
                    ets:delete_all_objects(?MD5_ETS),
                    info_msg("Cleared the cache.");
                false ->
                    ok
            end
    end.


file_md5(FilePath) ->
    {ok, Bin} = file:read_file(FilePath),
    erlang:md5(Bin).


update_file_md5(FilePath, CurrentMD5) ->
    ets:insert(?MD5_ETS, {FilePath, CurrentMD5}).


should_update(FilePath, CurrentMD5) ->
    case ets:lookup(?MD5_ETS, FilePath) of
        [{FilePath, CurrentMD5}] ->
            false;
        _ ->
            true
    end.


find_all_files(Where, NameRegexp, RelativePaths) ->
    case RelativePaths of
        false ->
            string:tokens(shell_cmd(
                ["find", Where, "-type f -name", "'" ++ NameRegexp ++ "'"]),
                "\n");
        true ->
            string:tokens(shell_cmd(
                ["cd", Where, "&&", "find . -type f -name",
                        "'" ++ NameRegexp ++ "'"]),
                "\n")
    end.


find_all_dirs(Where, RelativePaths) ->
    case RelativePaths of
        false ->
            string:tokens(
                shell_cmd(["ls", "-d", Where ++ "/*/"]),
                "\n");
        true ->
            string:tokens(
                shell_cmd(["cd", Where, "&&", "ls", "-d", "*/"]),
                "\n")
    end.


info_msg(Message) ->
    info_msg(Message, []).

info_msg(Format, Args) ->
    io:format("[SYNC] ~s~n", [str_utils:format(Format, Args)]).