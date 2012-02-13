#ifndef __RUNSCRIPT__
#define __RUNSCRIPT__

#if TARGET_API_MAC_CARBON

#ifdef __APPLE_CC__
#include <Carbon/Carbon.h>
#else
#include <Carbon.h>
#endif

#else

#include <Types.h>
#include <AppleEvents.h>

#endif



#ifdef __cplusplus
extern "C" {
#endif

size_t _hs_AEDescSize(void);
ptrdiff_t _hs_getUTF8Size(const AEDesc * input);
OSErr _hs_getData(const AEDesc* input, void* dataPtr, size_t maxSize);
OSErr _hs_dispose(AEDesc* input);
void _hs_initNull(AEDesc * input);

/* AppleScriptAvailable returns true if AppleScript is available. */
Boolean AppleScriptAvailable(void);


/* LowRunAppleScript compiles and runs an AppleScript provided as
text in the buffer pointed to by text.  textLength bytes will be
compiled from this buffer and run as an AppleScript using all of the
default environment and execution settings.  If resultData is not
NULL, then the result returned by the execution command will be
returned as typeChar in this descriptor record (or typeNull if there
is no result information).  If the function returns
errOSAScriptError, then resultData will be set to a descriptive
error message describing the error (if one is available).  */
OSStatus LowRunAppleScript(const void* text, long textLength, AEDesc *resultData);


#ifdef __cplusplus
}
#endif

#endif
