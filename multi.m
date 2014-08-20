% Mercury-fcgi library binding
% Contains thread-safe interface
%
% This binding is a simple wrapping of the C interface.
%
% Authors: Ebrahim Azarisooreh, Paul Bone

:- module multi.

:- interface.

:- import_module bool.
:- import_module maybe.
:- import_module io.

%--------------------------------------------------------------------------%
% Thread safe interface.
%--------------------------------------------------------------------------%

% finish request accepted by previous call to fcgx_accept
% also frees any storage allocated by previous call
:- pred fcgx_finish_r(c_pointer::in, io::di, io::uo) is det.

% initialize FCGX library. This is called by fcgx_accept
% but should be caleld when using fcgx_accept_r
:- pred fcgx_init(bool::out, io::di, io::uo) is det.

% accept new request from HTTP server, 0 is success, -1 is error (thread-safe)
:- pred fcgx_accept_r(bool::out, c_pointer::in, io::di, io::uo) is det.

% Initialize an FCGX_Request for use with FCGX_Accept_r(), 0 is success
:- pred fcgx_init_request(bool::out, c_pointer::out, int::in, int::in,
  io::di, io::uo) is det.

% writing string to buffer in FCGX_Request struct 
:- pred fcgx_write(string::in, c_pointer::in, bool::out, io::di, io::uo) is det.

% get the parameters
:- pred fcgx_get_param_r(string::in, c_pointer::in, maybe(string)::uo,
  io::di, io::uo) is det.

:- implementation.

%--------------------------------------------------------------------------%

:- pragma foreign_decl("C",
"
   #include \"fcgiapp.h\"
").

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C", fcgx_finish_r(ReqDataPtr::in, _IO0::di, _IO::uo),
  [thread_safe, promise_pure, will_not_call_mercury, tabled_for_io],
  "
   FCGX_Finish_r((FCGX_Request *)ReqDataPtr);
  ").

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C", fcgx_init(Success::out, _IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
  "
   Success = FCGX_Init() == 0 ? MR_YES : MR_NO;
  ").		

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C", fcgx_accept_r(Success::out, ReqDataPtr::in,
  _IO0::di, _IO::uo),
  [thread_safe, promise_pure, will_not_call_mercury, tabled_for_io],
  "
   Success = FCGX_Accept_r((FCGX_Request *)ReqDataPtr) >= 0 ? MR_YES : MR_NO;
  ").

%--------------------------------------------------------------------------%

:- pragma no_inline(fcgx_init_request/6).
:- pragma foreign_proc("C", fcgx_init_request(Success::out, Request::out,
  Sock::in, Flags::in, _IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
  "
   static FCGX_Request r;
   Success = FCGX_InitRequest(&r, Sock, Flags) == 0 ? MR_YES : MR_NO;
   Request = (MR_Word)&r;
  ").

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C", fcgx_write(Str::in, Request::in, Success::out,
  _IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
  "
   Success = FCGX_PutStr(Str, strlen(Str),
       ((FCGX_Request *)Request)->out) >= 0 ? MR_YES : MR_NO;
  ").

%--------------------------------------------------------------------------%

fcgx_get_param_r(Name, Request, MaybeParam, !IO) :-
    get_param_r(Name, Request, String, Result, !IO),
    (
        Result = yes,
        MaybeParam = yes(String)
    ;
        Result = no,
        MaybeParam = no
    ).

:- pred get_param_r(string::in, c_pointer::in, string::uo, bool::uo,
  io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_param_r(Name::in, Request::in, String::uo, Result::uo, _IO2::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    char *temp;

    temp = FCGX_GetParam(Name, ((FCGX_Request *)Request)->envp);
    if (temp != NULL) {
        Result = MR_YES;
        /*
         * Duplicate the string so we can use it after calling fcgi_finish()
         */
        MR_make_aligned_string_copy(String, temp);
    } else {
        Result = MR_NO;
        String = NULL;
    }
").

