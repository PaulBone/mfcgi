% FCGI Single threaded demo
%
% Copyright (C) 2014 Ebrahim Azarisooreh
% Copyright (C) 2014 Paul Bone
% Distributed under the BSD 3-clause license, see LICENSE.
%

:- module mfcgi_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module mfcgi.
:- import_module mfcgi.basic.
:- import_module mfcgi.streams.

main(!IO) :-
    fcgx_accept(Success, !IO),
    (
        Success = yes,
        fcgx_get_output_stream(Output, !IO),
        fcgx_get_param("QUERY_STRING", MaybeStr, !IO),
        fcgx_puts(Output, header, _, !IO),
        (
            MaybeStr = yes(Str),
            fcgx_puts(Output, format("Query string was ""%s""\n",
                [s(Str)]), _, !IO)
        ;
            MaybeStr = no,
            fcgx_puts(Output, "No query string\n", _, !IO)
        ),
        main(!IO)
    ;
        Success = no
    ).


:- func header = string.

header = "Content-Type: text/plain\n\n".

