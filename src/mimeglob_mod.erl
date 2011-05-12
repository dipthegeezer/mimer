%% @doc Gives a good MIME type guess based on file extension.

-module(mimeglob_mod).

-export([mimetype_from_file_extension]).

-include("mimer.hrl").

%% @spec mimetype_from_file_extension(S::string()) -> string() | undefined
%% @doc Given a filename extension (e.g. ".html") return a guess for the MIME
%%      type such as "text/html". Will return the atom undefined if no good
%%      guess is available.

mimetype_from_file_extension( MimeFileExtList, Filename ) ->
    case lists:keyfind(filename:extension(Filename), 1, MimeFileExtList) of
        false ->
            undefined;
        {_, MimeType} ->
            MimeType
    end.

%% @spec parse_mime_glob(S::string()) -> {ok, list()} | error
%% @doc Given a filename return the a list containing the
%% mappings of the file extensions and their mime type
parse_mime_glob(File) ->
    Data =
    case file:read_file(File) of
        {ok, Data0} ->
            Data0;
        {error, enoent} ->
            io:fwrite(standard_error,"Error File not Found ~p", [File]);
        {error, eacces} ->
            io:fwrite(standard_error,"Inadequate file permissions ~p", [File]);
        {error, Reason} ->
            io:fwrite(standard_error,"File error ~p ~p", [Reason, File])
    end,
    Lines = re:split(Data, "\r\n|\n|\r|\032", [{return, list}]),
    ParsedValues =
    lists:foldr(
      fun(Line, FileInfo) ->
           case string:strip(Line) of
               "#" ++ _Comment ->
                   FileInfo;
               Line2 ->
                   case re:split(Line2, "\s?:\s?", [{return, list}]) of
                       [MimeType,"*" ++ Ext] ->
                           [{Ext,MimeType}|FileInfo];
                       _ -> FileInfo
                   end
           end
      end,[{"",undefined}],Lines),
    {ok,ParsedValues}.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

mimetype_from_file_extension_test() ->
    FileInfo = [{".html","text/html"}],
    ?assertEqual("text/html",
                 mimetype_from_file_extension(FileInfo, "monkey.html")),
    ?assertEqual(undefined,
                 mimetype_from_file_extension(FileInfo,"")),
    ?assertEqual(undefined,
                 mimetype_from_file_extension(FileInfo,"magic_people.voodoo_people")),
    ok.

file_load_test() ->
    {ok,Data} = parse_mime_glob("/usr/share/mime/globs"),
    io:fwrite(standard_error,"THE DATA ~p", [Data]),
    ?assert(true),
    ok.

-endif.

