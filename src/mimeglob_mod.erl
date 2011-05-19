%% @doc Gives a good MIME type guess based on file extension.

-module(mimeglob_mod).

-export([parse_mime_glob/1]).

-include("mimer.hrl").

%% @spec parse_mime_glob(S::string()) -> {ok, list()} | error
%% @doc Given a filename return the a list containing the
%% mappings of the file extensions and their mime type
parse_mime_glob(File) ->
    case file:read_file(File) of
        {ok, Data} ->
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
            {ok, ParsedValues };
        {error, enoent} ->
            io:fwrite("Error File not Found ~p", [File]),
            {error, enoent};
        {error, eacces} ->
            io:fwrite("Inadequate file permissions ~p", [File]),
            {error, eacces};
        {error, Reason} ->
            io:fwrite("File error ~p ~p", [Reason, File]),
            {error, Reason}
    end.



%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

file_load_test() ->
    {ok,Monkey} = parse_mime_glob("/usr/share/mime/globs"),
    {".cpp",Type} = lists:keyfind(".cpp", 1, Monkey),
    ?assertEqual("text/x-c++src", Type).

-endif.

