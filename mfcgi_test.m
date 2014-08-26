% FCGI Single threaded demo
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

main(!IO) :-
    fcgx_accept(Success, !IO),
    fcgx_get_output_stream(Out, !IO),
    (
        Success = yes,
        fcgx_get_param("QUERY_STRING", MaybeStr, !IO),
        fcgx_puts(header, Out, _, !IO),
        (
            MaybeStr = yes(Str),
            fcgx_puts(format("Query string was ""%s""\n",
                [s(Str)]), Out, _, !IO)
        ;
            MaybeStr = no,
            fcgx_puts("No query string\n", Out, _, !IO)
        ),
        main(!IO)
    ;
        Success = no
    ).


:- func header = string.

header = "Content-Type: text/plain\n\n".

