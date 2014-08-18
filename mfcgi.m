% Mercury-fcgi library binding
% contains single thread and multi-thread parts of api
%
% Author: eazar001

:- module mfcgi.

:- interface.

:- import_module bool.
:- import_module io.
:- import_module string.

% accept a request from the client
:- pred fcgx_accept(bool::out, io::di, io::uo) is det.

% get a parameter from the environment array
:- pred fcgx_get_param(string::in, string::out, io::di, io::uo) is det.

% write a string to buffer
:- pred fcgx_puts(string::in, bool::out, io::di, io::uo) is det.

:- implementation.

% Declarations ------------------------------------------------------------------
       
:- pragma foreign_decl("C",
  "
   #include <assert.h>
   #include <errno.h>
   #include <fcntl.h>      /* for fcntl */
   #include <math.h>
   #include <memory.h>     /* for memchr() */
   #include <stdarg.h>
   #include <stdio.h>
   #include <stdlib.h>
   #include <string.h>
   #include <sys/types.h>

   #include \"fcgi_config.h\"

   #ifdef HAVE_SYS_SOCKET_H
   #include <sys/socket.h> /* for getpeername */
   #endif

   #ifdef HAVE_SYS_TIME_H
   #include <sys/time.h>
   #endif

   #ifdef HAVE_UNISTD_H
   #include <unistd.h>
   #endif

   #ifdef HAVE_LIMITS_H
   #include <limits.h>
   #endif

   #ifdef _WIN32
   #define DLLAPI  __declspec(dllexport)
   #endif

   #include \"fcgimisc.h\"
   #include \"fastcgi.h\"
   #include \"fcgios.h\"
   #include \"fcgiapp.h\"

   #ifdef HAVE_VA_ARG_LONG_DOUBLE_BUG
   #define LONG_DOUBLE double
   #else
   #define LONG_DOUBLE long double
   #endif

   /*
    * Globals
    */
   static int libInitialized = 0;
   static int isFastCGI = -1;
   static char *webServerAddressList = NULL;
   static FCGX_Request the_request;

   FCGX_Stream *in, *out, *err;
   FCGX_ParamArray envp;
  ").

% API ---------------------------------------------------------------------------

:- pragma foreign_proc("C", fcgx_accept(Success::out, _IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
  "
   Success = FCGX_Accept(&in, &out, &err, &envp) >= 0 ? MR_YES : MR_NO;
  ").

:- pragma foreign_proc("C", fcgx_get_param(Name::in, Result::out,
  _IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
  "
   Result = FCGX_GetParam(Name, envp);
  ").

:- pragma foreign_proc("C", fcgx_puts(Str::in, Success::out,
  _IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
  "
   Success = FCGX_PutStr(Str, strlen(Str), out) >= 0 ? MR_YES : MR_NO;
  ").

% tests whether application is CGI or FCGI (for compatibility)
% True if CGI, False if FCGI
:- pred fcgx_isCGI(bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C", fcgx_isCGI(Result::out, _IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
  "
   Result = FCGX_isCGI() ? MR_YES : MR_NO;
  ").		      

:- pred fcgx_finish(io::di, io::uo) is det.

:- pragma foreign_proc("C", fcgx_finish(_IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
  "
   FCGX_Finish_r(&the_request);
  ").

% thread safe
% finish request accepted by previous call to fcgx_accept
% also frees any storage allocated by previous call
:- pred fcgx_finish_r(c_pointer::in, io::di, io::uo) is det.

:- pragma foreign_proc("C", fcgx_finish_r(ReqDataPtr::in, _IO0::di, _IO::uo),
  [thread_safe, promise_pure, will_not_call_mercury, tabled_for_io],
  "
   FCGX_Finish_r((FCGX_Request *)ReqDataPtr);
  ").

% initialize FCGX library. This is called by fcgx_accept
% but should be caleld when using fcgx_accept_r
:- pred fcgx_init(bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C", fcgx_init(Success::out, _IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
  "
   Success = FCGX_Init() == 0 ? MR_YES : MR_NO;
  ").		      

% thread safe
% accept new request from HTTP server
% 0 is success, -1 is error.
:- pred fcgx_accept_r(bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C", fcgx_accept_r(Success::out, _IO0::di, _IO::uo),
  [thread_safe, promise_pure, will_not_call_mercury, tabled_for_io],
  "
   Success = FCGX_Accept(&in, &out, &err, &envp) >= 0 ? MR_YES : MR_NO;		      
  ").
