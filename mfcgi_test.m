% FCGI Single threaded demo
%
:- module mfcgi_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module string.

:- import_module mfcgi.

main(!IO) :-
    fcgx_accept(Success, !IO),
    (
        Success = yes,
        fcgx_get_param("QUERY_STRING", Str, !IO),
        fcgx_puts(header, _, !IO),
        fcgx_puts(format("Query string was ""%s""\n",
            [s(Str)]), _, !IO),
        main(!IO)
    ;
        Success = no
    ).


:- func header = string.

header = "Content-Type: text/plain\n\n".

