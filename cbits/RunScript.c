#include <Carbon/Carbon.h>

#include "RunScript.h"

#if ! TARGET_API_MAC_CARBON

#include <OSA.h>
#include <AppleScript.h>
#include <Gestalt.h>

#endif

#include <string.h>

size_t _hs_AEDescSize(void) {
  AEDesc a;
  return sizeof(a);
}

// if the input is UTF8Text, returns its size in bytes. Otherwise returns -1
ptrdiff_t _hs_getUTF8Size(const AEDesc* input) {
  if (input != NULL && input->descriptorType == typeUTF8Text) {
    return AEGetDescDataSize(input);
  } else {
    return -1;
  }
}

OSErr _hs_getData(const AEDesc* input, void* dataPtr, size_t maxSize) {
  AEGetDescData(input, dataPtr, maxSize);
}

OSErr _hs_dispose(AEDesc* input) {
  if (input != NULL && input->descriptorType != typeNull) {
    AEDisposeDesc(input);
  }
}

void _hs_initNull(AEDesc * input) {
  AECreateDesc(typeNull, NULL, 0, input);
}

	/* AppleScriptAvailable returns true if AppleScript is available
	and the routines defined herein can be called. */
Boolean AppleScriptAvailable(void) {
	long response;
	if (Gestalt(gestaltAppleScriptAttr, &response) != noErr) response = 0; 
	return ((response & (1<<gestaltAppleScriptPresent)) != 0);
}

	/* LowRunAppleScript compiles and runs an AppleScript
	provided as text in the buffer pointed to by text.  textLength
	bytes will be compiled from this buffer and run as an AppleScript
	using all of the default environment and execution settings.  resultData
        must be non-NULL, and should have been previously initialised, for example
        with _hs_initNull. The result returned by the execution
	command will be returned as typeUTF8Text in this descriptor record
	(or typeNull if there is no result information).  If the function
	returns errOSAScriptError, then resultData will be set to a
	descriptive error message describing the error (if one is
	available).  */
OSStatus LowRunAppleScript(const void* text, long textLength, AEDesc *resultData) {
	ComponentInstance theComponent;
	AEDesc scriptTextDesc;
	OSStatus err;
	OSAID scriptID, resultID;
		/* set up locals to a known state */
	theComponent = NULL;
	AECreateDesc(typeNull, NULL, 0, &scriptTextDesc);
	scriptID = kOSANullScript;
	resultID = kOSANullScript;
		/* open the scripting component */
	theComponent = OpenDefaultComponent(kOSAComponentType,
					typeAppleScript);
	if (theComponent == NULL) { err = paramErr; goto bail; }
		/* put the script text into an aedesc */
	err = AECreateDesc(typeUTF8Text, text, textLength, &scriptTextDesc);
	if (err != noErr) goto bail;
		/* compile the script */
	err = OSACompile(theComponent, &scriptTextDesc, 
					kOSAModeNull, &scriptID);
	if (err != noErr) goto bail;
		/* run the script/get the result */
	err = OSAExecute(theComponent, scriptID, kOSANullScript,
					kOSAModeNull, &resultID);
	if (resultData != NULL) {
		if (err == noErr && resultID != kOSANullScript) {
			OSADisplay(theComponent, resultID, typeUTF8Text,
						kOSAModeNull, resultData);
		}
	}
bail:
	if (err == errOSAScriptError) {
          AECreateDesc(typeNull, NULL, 0, resultData);
          OSAScriptError(theComponent, kOSAErrorMessage, typeUTF8Text, resultData);
        }

	AEDisposeDesc(&scriptTextDesc);
	if (scriptID != kOSANullScript) OSADispose(theComponent,  scriptID);
	if (resultID != kOSANullScript) OSADispose(theComponent,  resultID);
	if (theComponent != NULL) CloseComponent(theComponent);
	return err;
}
