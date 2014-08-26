% Mercury-fcgi library binding
% Contains thread-safe interface
%
% This binding is a simple wrapping of the C interface.
%
% Authors: Ebrahim Azarisooreh, Paul Bone

:- module multi.

:- interface.

:- import_module bool.
:- import_module io.
:- import_module maybe.

%--------------------------------------------------------------------------%
% Thread safe interface.
%--------------------------------------------------------------------------%

:- type request.

% finish request accepted by previous call to fcgx_accept
% also frees any storage allocated by previous call
:- pred fcgx_finish_r(request::in, io::di, io::uo) is det.

% initialize FCGX library. This is called by fcgx_accept
% but should be caleld when using fcgx_accept_r
:- pred fcgx_init(bool::out, io::di, io::uo) is det.

% accept new request from HTTP server, 0 is success, -1 is error (thread-safe)
:- pred fcgx_accept_r(bool::out, request::in, io::di, io::uo) is det.

% Initialize an FCGX_Request for use with FCGX_Accept_r()
:- pred fcgx_init_request(maybe(request)::out, int::in, int::in,
  io::di, io::uo) is det.

% writing string to buffer in FCGX_Request struct 
:- pred fcgx_write(string::in, request::in, bool::out, io::di, io::uo) is det.

:- implementation.

%--------------------------------------------------------------------------%

:- pragma foreign_decl("C",
"
   #include \"fcgiapp.h\"
").

%--------------------------------------------------------------------------%

:- pragma foreign_type("C", request, "FCGX_Request*").

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C", fcgx_finish_r(ReqDataPtr::in, _IO0::di, _IO::uo),
  [thread_safe, promise_pure, will_not_call_mercury, tabled_for_io],
  "
   FCGX_Finish_r(ReqDataPtr);
  ").

:- pragma foreign_proc("C", fcgx_init(Success::out, _IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
  "
   Success = FCGX_Init() == 0 ? MR_YES : MR_NO;
  ").		

:- pragma foreign_proc("C", fcgx_accept_r(Success::out, ReqDataPtr::in,
  _IO0::di, _IO::uo),
  [thread_safe, promise_pure, will_not_call_mercury, tabled_for_io],
  "
   Success = FCGX_Accept_r(ReqDataPtr) >= 0 ? MR_YES : MR_NO;
  ").

%--------------------------------------------------------------------------%

fcgx_init_request(MaybeRequest, Sock, Flags, !IO) :-
    do_init_request(Result, Request, Sock, Flags, !IO),
    (
        Result = yes,
        MaybeRequest = yes(Request)
    ;
        Result = no,
        MaybeRequest = no
    ).

:- pred do_init_request(bool::out, request::out, int::in, int::in,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_init_request(Success::out, Request::out, Sock::in, Flags::in,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    FCGX_Request *r = MR_NEW(FCGX_Request);

    Success = FCGX_InitRequest(r, Sock, Flags) == 0 ? MR_YES : MR_NO;
    Request = r;
").

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C", fcgx_write(Str::in, Request::in, Success::out,
  _IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
  "
   Success = FCGX_PutStr(Str, strlen(Str), (Request)->out)
                >= 0 ? MR_YES : MR_NO;
  ").

%--------------------------------------------------------------------------%
