#
# Post-build code signing for the JASP executable.
#
# Why: on arm64 the linker ad-hoc signs every binary, so the signature changes
# on every rebuild. The macOS Keychain keys item access lists to the code
# signature (Desktop/auth/secretvault.cpp), which makes every fresh build a
# "different app" that re-prompts for the login password. Signing with a stable
# identity after each link fixes that.
#
# Behaviour:
#   - signs with IDENTITY (a Developer ID hash, or a certificate name)
#   - if that fails — no such identity in any keychain, locked keys, a
#     cancelled prompt — falls back to an ad-hoc signature, which is exactly
#     what the linker would have produced anyway
#   - never fails the build
#
# No --timestamp and no --options runtime here: this is a development
# signature. Distribution signing stays in Pack.cmake.
#
# Usage:
#   cmake -DFILE=<binary> -DIDENTITY=<hash-or-name> -P SignJasp.cmake

if(NOT DEFINED FILE OR NOT DEFINED IDENTITY)
	message(FATAL_ERROR "SignJasp.cmake requires -DFILE=<binary> and -DIDENTITY=<identity>")
endif()

execute_process(
	COMMAND codesign --force --sign "${IDENTITY}" "${FILE}"
	RESULT_VARIABLE _result
	OUTPUT_VARIABLE _out
	ERROR_VARIABLE  _err)

if(_result EQUAL 0)
	message(STATUS "SignJasp: signed ${FILE} with '${IDENTITY}'")
else()
	message(WARNING
		"SignJasp: signing with '${IDENTITY}' failed (${_result}) — falling back to ad-hoc.\n${_err}")
	execute_process(
		COMMAND codesign --force --sign - "${FILE}"
		RESULT_VARIABLE _adhoc)
	if(NOT _adhoc EQUAL 0)
		message(WARNING "SignJasp: ad-hoc signing failed too (${_adhoc}) — keeping the linker signature")
	endif()
endif()
