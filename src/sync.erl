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

% Prints a single variable
-define(dump(_Arg), io:format(user, "[DUMP] ~s: ~p~n~n", [??_Arg, _Arg])).

%% Predefined file with config (location relative to including project root).
-define(GUI_CONFIG_LOCATION, "rel/gui.config").

%% API
-export([start/1, reset/0, sync/0]).
-export([track_gui/0, dont_track_gui/0]).
-export([track_dep/1, dont_track_dep/1]).
-export([track_dir/1, dont_track_dir/1]).
-export([track_module/1, dont_track_module/1]).
-export([add_includes/1]).

%%%===================================================================
%%% API
%%%===================================================================

start(ProjectSourceDir) ->
    % ForceClear: true,
    start_ets(),
    ets_insert(project_dir, ProjectSourceDir),
    % Resolve all paths to includes
    ProjIncludes = [filename:join(ProjectSourceDir, "include")],
    Deps = find_all_dirs(filename:join(ProjectSourceDir, "deps"), false),
    DepsIncludes = lists:map(
        fun(DepPath) ->
            filename:join(DepPath, "include")
        end, Deps),
    ets_insert(includes, ProjIncludes ++ DepsIncludes).


reset() ->
    ProjectDir = ets_lookup(project_dir),
    start(ProjectDir).


track_gui() ->
    % Make sure ets exists.
    case ensure_ets() of
        false -> error;
        true -> toggle_track_gui(true)
    end.


dont_track_gui() ->
    % Make sure ets exists.
    case ensure_ets() of
        false -> error;
        true -> toggle_track_gui(false)
    end.


track_dep(DepOrDeps) ->
    Deps = ensure_list_of_lists(DepOrDeps),
    toggle_track_dep(Deps, true).


dont_track_dep(DepOrDeps) ->
    Deps = ensure_list_of_lists(DepOrDeps),
    toggle_track_dep(Deps, false).


track_dir(DirOrDirs) ->
    Dirs = ensure_list_of_lists(DirOrDirs),
    Results = lists:map(
        fun(Dir) ->
            toggle_track_dir(Dir, true)
        end, Dirs),
    lists:all(fun(Res) -> Res end, Results).


dont_track_dir(DirOrDirs) ->
    Dirs = ensure_list_of_lists(DirOrDirs),
    Results = lists:map(
        fun(Dir) ->
            toggle_track_dir(Dir, false)
        end, Dirs),
    lists:all(fun(Res) -> Res end, Results).


track_module(ModuleOrModules) ->
    Modules = ensure_list_of_lists(ModuleOrModules),
    Results = lists:map(
        fun(Module) ->
            toggle_track_module(Module, false)
        end, Modules),
    lists:all(fun(Res) -> Res end, Results).


dont_track_module(ModuleOrModules) ->
    Modules = ensure_list_of_lists(ModuleOrModules),
    Results = lists:map(
        fun(Module) ->
            toggle_track_module(Module, true)
        end, Modules),
    lists:all(fun(Res) -> Res end, Results).


add_includes(IncludeOrIncludes) ->
    IncludesToAdd = ensure_list_of_lists(IncludeOrIncludes),
    Includes = ets_lookup(includes, []),
    ets_insert(includes, Includes ++ IncludesToAdd),
    true.


toggle_track_gui(Flag) ->
    case get_gui_config() of
        {error, enoent} ->
            error_msg("Cannot track GUI: gui.config not found in `~s`.",
                [?GUI_CONFIG_LOCATION]),
            false;
        {ok, _} ->
            case Flag of
                true ->
                    info_msg("Tracking all GUI files");
                false ->
                    info_msg("Untracked GUI files")
            end,
            ets_insert(track_gui, Flag),
            true
    end.


toggle_track_dep(Deps, Flag) ->
    Results = lists:map(
        fun(Dep) ->
            toggle_track_dir(filename:join(["deps", Dep, "src"]),
                Flag)
        end, Deps),
    lists:all(fun(Res) -> Res end, Results).


toggle_track_dir(Path, Flag) ->
    ProjectDir = ets_lookup(project_dir),
    Dirs = ets_lookup(dirs, []),
    DirsWithout = Dirs -- [Path],
    case filelib:is_dir(filename:join([ProjectDir, Path])) of
        true ->
            NewDirs = case Flag of
                          true -> DirsWithout ++ [Path];
                          false -> DirsWithout
                      end,
            ets_insert(dirs, NewDirs),
            case Flag of
                true ->
                    info_msg("Tracking all files in directory `~s`", [Path]);
                false ->
                    info_msg("Untracked files in directory `~s`", [Path])
            end,
            true;
        false ->
            error_msg("Cannot track directory `~s` - it was not found in "
            "the source project dir.", [Path]),
            false
    end.


toggle_track_module(PathOrName, Flag) ->
    ProjectDir = ets_lookup(project_dir),
    Path = case filelib:is_file(PathOrName) of
               true ->
                   PathOrName;
               _ ->
                   case find_all_files(ProjectDir, PathOrName ++ "'.erl'",
                       true) of
                       [] ->
                           error_msg("Cannot track module `~s` - it was not "
                           "found in the source project dir.", [PathOrName]),
                           undefined;
                       [FilePath] ->
                           FilePath
                   end
           end,
    case Path of
        undefined ->
            false;
        _ ->
            Files = ets_lookup(files, []),
            FilesWithout = Files -- [Path],
            NewFiles = case Flag of
                           true -> FilesWithout ++ [
                               filename:join([ProjectDir, Path])
                           ];
                           false -> FilesWithout
                       end,
            ets_insert(files, NewFiles),
            true
    end.


sync() ->
    % Make sure ets exists.
    case ensure_ets() of
        false ->
            false;
        true ->
            info_msg("Running sync..."),
            info_msg("-------------------------------------------", [], ""),
            ProjectDir = ets_lookup(project_dir),
            DirsToRecompile = ets_lookup(dirs, []),
            Includes = ets_lookup(includes, []),

            % Check if GUI is tracked.
            Flag = ets_lookup(track_gui, false),
            UpdateGUIFilesRes =
                case Flag of
                    true ->
                        {ok, GuiCfg} = get_gui_config(),
                        SrcGuiDir = proplists:get_value(source_gui_dir, GuiCfg),
                        {EOK, EUTD, EErr} =
                            update_erl_files(ProjectDir, [SrcGuiDir], Includes),
                        {SOK, SUTD, SErr} =
                            update_gui_static_files(ProjectDir, GuiCfg),
                        {EOK + SOK, EUTD + SUTD, EErr + SErr};
                    false ->
                        {0, 0, 0}
                end,

            % Recompile erl files. If gui.config exists, GUI erl files will
            % be recompiled automatically.
            UpdateErlFilesRes = update_erl_files(
                ProjectDir, DirsToRecompile, Includes),

            % Check the results.
            {GUIOK, GUIUpToDate, GUIError} = UpdateGUIFilesRes,
            {ErlOK, ErlUpToDate, ErlError} = UpdateErlFilesRes,
            OK = GUIOK + ErlOK,
            UpToDate = GUIUpToDate + ErlUpToDate,
            Error = GUIError + ErlError,
            case OK + UpToDate + Error of
                0 ->
                    info_msg("No files are tracked. Use sync:track_* first.");
                _ ->
                    case OK of
                        0 -> ok;
                        _ -> info_msg(
                            "-------------------------------------------",
                            [], "")
                    end,
                    info_msg("~4.b file(s) were updated", [OK]),
                    info_msg("~4.b file(s) were already up to date",
                        [UpToDate]),
                    info_msg("~4.b file(s) could not be updated", [Error]),
                    case Error of
                        0 ->
                            info_msg("Success!"),
                            true;
                        _ ->
                            error_msg("There were errors."),
                            false
                    end
            end
    end.


update_erl_files(ProjectDir, DirsToRecompile, Includes) ->
    AllIncludes = lists:map(
        fun(DepPath) ->
            {i, DepPath}
        end, Includes),
    % Resolve list of files to recompile
    FilesInDirs = lists:foldl(
        fun(DirPath, Acc) ->
            Files = find_all_files(
                filename:join(ProjectDir, DirPath), "*.erl", false),
            Files ++ Acc
        end, [], DirsToRecompile),

    FilesToCheck = FilesInDirs ++ ets_lookup(files, []),

    % Do the recompilation
    CompilationResults = utils:pmap(
        fun(File) ->
            update_erl_file(File, AllIncludes ++ [report])
        end, FilesToCheck),

    % Count number of successful updates, files that were up to data and
    % fiels that were failed to update.
    lists:foldl(fun(Res, {AccOK, AccUpToDate, AccError}) ->
        case Res of
            true -> {AccOK + 1, AccUpToDate, AccError};
            up_to_date -> {AccOK, AccUpToDate + 1, AccError};
            false -> {AccOK, AccUpToDate, AccError + 1}
        end
    end, {0, 0, 0}, CompilationResults).


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

    % Do the updating
    CompilationResults = utils:pmap(
        fun({SourceFilePath, FileName}) ->
            case filename:extension(FileName) of
                ".erl" ->
                    % Do not copy erl files
                    skip;
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
            end
        end, SourceFileMappings),

    % Count number of successful updates, files that were up to data and
    % fiels that were failed to update.
    lists:foldl(fun(Res, {AccOK, AccUpToDate, AccError}) ->
        case Res of
            true -> {AccOK + 1, AccUpToDate, AccError};
            up_to_date -> {AccOK, AccUpToDate + 1, AccError};
            false -> {AccOK, AccUpToDate, AccError + 1};
            skip -> {AccOK, AccUpToDate, AccError}
        end
    end, {0, 0, 0}, CompilationResults).


update_erl_file(File, CompileOpts) ->
    CurrentMD5 = file_md5(File),
    case should_update(File, CurrentMD5) of
        false ->
            up_to_date;
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


update_static_file(SourceFile, RelaseStaticFilesDir, FileName) ->
    TargetPath = filename:join(RelaseStaticFilesDir, FileName),
    CurrentMD5 = file_md5(SourceFile),
    case should_update(SourceFile, CurrentMD5) of
        false ->
            up_to_date;
        true ->
            case shell_cmd(["mkdir -p", filename:dirname(TargetPath)]) of
                [] ->
                    case shell_cmd(["cp -f", SourceFile, TargetPath]) of
                        [] ->
                            info_msg("Updated:   ~s", [abs_path(FileName)]),
                            update_file_md5(SourceFile, CurrentMD5),
                            true;
                        Other1 ->
                            error_msg("Cannot copy ~s: ~s", [FileName, Other1]),
                            false
                    end;
                Other2 ->
                    error_msg("Cannot create dir ~s: ~s",
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
            up_to_date;
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
                            error_msg("Cannot compile ~s: ~s",
                                [SourceFile, Other]),
                            false
                    end;
                Other2 ->
                    error_msg("Cannot create dir ~s: ~s", [TargetDir, Other2]),
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
            up_to_date;
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
                            error_msg("Cannot compile ~s: ~s",
                                [SourceFile, Other]),
                            false
                    end;
                Other2 ->
                    error_msg("Cannot create dir ~s: ~s", [TargetDir, Other2]),
                    false
            end
    end.


get_gui_config() ->
    ProjectDir = ets_lookup(project_dir),
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


start_ets() ->
    case ets:info(?MD5_ETS) of
        undefined ->
            % Start ETS in another process so it won't be deleted
            % if calling process crashes
            spawn(
                fun() ->
                    ets:new(?MD5_ETS, [public, set, protected, named_table,
                        {read_concurrency, true}
                    ]),
                    receive kill -> ok end
                end),
            info_msg("Started new ETS table to track changes in files.");
        _ ->
            ets:delete_all_objects(?MD5_ETS),
            info_msg("Cleared the ETS cache.")
    end.


ensure_ets() ->
    case ets:info(?MD5_ETS) of
        undefined ->
            error_msg("You must do sync:start/1 before using sync."),
            error_msg("Then, use sync:track_* to specify files to be tracked."),
            false;
        _ ->
            true
    end.


ets_lookup(Key) ->
    ets_lookup(Key, undefined).

ets_lookup(Key, Default) ->
    case ets:lookup(?MD5_ETS, Key) of
        [{Key, Val}] -> Val;
        _ -> Default
    end.


ets_insert(Key, Val) ->
    ets:insert(?MD5_ETS, {Key, Val}).


file_md5(FilePath) ->
    {ok, Bin} = file:read_file(FilePath),
    erlang:md5(Bin).


update_file_md5(FilePath, CurrentMD5) ->
    ets_insert(FilePath, CurrentMD5).


should_update(FilePath, CurrentMD5) ->
    case ets_lookup(FilePath) of
        CurrentMD5 ->
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


ensure_list_of_lists(List) ->
    case List of
        [] ->
            [];
        [H | _] when is_list(H) orelse is_atom(H) ->
            [str_utils:to_list(E) || E <- List];
        Other ->
            [Other]
    end.


info_msg(Message) ->
    info_msg(Message, [], "[SYNC] ").

info_msg(Format, Args) ->
    info_msg(Format, Args, "[SYNC] ").

error_msg(Message) ->
    info_msg(Message, [], "[SYNC ERROR] ").

error_msg(Format, Args) ->
    info_msg(Format, Args, "[SYNC ERROR] ").

info_msg(Format, Args, Prefix) ->
    io:format("~s~s~n", [Prefix, str_utils:format(Format, Args)]).