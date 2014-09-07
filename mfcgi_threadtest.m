% FCGI Multi threaded demo
%
% Copyright (C) 2014 Ebrahim Azarisooreh
% Copyright (C) 2014 Paul Bone
% Distributed under the BSD 3-clause license, see LICENSE.
%

:- module mfcgi_threadtest.

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
:- import_module mfcgi.multi.

main(!IO) :-
  fcgx_init(Result0, !IO),
  (
    Result0 = yes,
    fcgx_init_request(MaybeRequest, 0, 0, !IO),
    (
      MaybeRequest = yes(Request),
      fcgx_accept_r(Result1, Request, !IO),
      (
        Result1 = yes,
        fcgx_write(header, Request, _, !IO),
        fcgx_get_param_r("QUERY_STRING", Request, MaybeQuery, !IO),
        (
          MaybeQuery = yes(Query),
          fcgx_write(Query, Request, _, !IO)
        ;
          MaybeQuery = no,
          fcgx_write("No Query\n", Request, _, !IO)
        ),
        fcgx_finish_r(Request, !IO),
        main(!IO)
      ;
        Result1 = no,
        fcgx_finish_r(Request, !IO)
        % accept failed.
      )
    ;
      MaybeRequest = no
      % init request failed.
    )
  ;
    Result0 = no
    % Init fcgx failed.
  ).



:- func header = string.

header = "Content-Type: text/plain\n\n".

