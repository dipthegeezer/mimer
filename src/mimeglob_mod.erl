%% @doc Gives a good MIME type guess based on file extension.

-module(mimeglob_mod).

-export([parse_mime_glob/1]).

-include("mimer.hrl").

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

file_load_test() ->
    PrivDir = code:priv_dir(mimer_app),
    io:fwrite(standard_error,"File error ~p ", [PrivDir]),
    {ok,_} = parse_mime_glob("/usr/share/mime/globs"),
    ?assert(true),
    ok.

-endif.

