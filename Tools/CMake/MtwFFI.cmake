# MtwFFI.cmake wires JASP to the mtw-ffi C library: the Minitab (MTW/MPJ/MTP)
# worksheet reader used by MinitabOldImporter. See MTW_IMPORT_NOTES.md at the
# repository root.
#
# mtw-ffi is vendored as a git submodule of this repository:
#     Libraries/MTW-FFI   ->   https://github.com/jasp-stats/MTW-FFI
# The Rust crate is built from that checkout with cargo as part of the JASP
# build — a Rust toolchain is therefore required on JASP build hosts — and the
# compiled static library is linked in through an IMPORTED target.
#
# Artifact layout inside the submodule (release profile):
#   Linux/macOS: target/release/libmtw_ffi.a
#   Windows:     target/release/mtw_ffi.lib    (statically linked CRT via the
#                submodule's .cargo/config.toml — no runtime DLL dependency)
# The C header comes straight from the submodule (crates/mtw-ffi/mtw.h): the
# submodule commit pins header and code together, so they cannot drift apart
# and nothing needs to be vendored here.
#
# ABI guards:
#   * upstream, the header_sync drift-guard test keeps the submodule's
#     committed mtw.h in sync with the Rust API (cargo test -p mtw-ffi).
#   * configure-time ABI probe: a tiny program calling mtw_abi_version() is
#     compiled AND run here; anything but 1 fails the configure loudly.

list(APPEND CMAKE_MESSAGE_CONTEXT MtwFFI)

set(MTW_FFI_ABI_EXPECTED 1)

set(MTW_FFI_SUBMODULE "${CMAKE_SOURCE_DIR}/Libraries/MTW-FFI")

if(NOT EXISTS "${MTW_FFI_SUBMODULE}/Cargo.toml")
	message(FATAL_ERROR
		"The mtw-ffi submodule (the Minitab reader used by MinitabOldImporter) is not checked out.\n"
		"Run:  git submodule update --init Libraries/MTW-FFI\n"
		"See MTW_IMPORT_NOTES.md for details.")
endif()

find_program(MTW_FFI_CARGO_EXECUTABLE cargo)
if(NOT MTW_FFI_CARGO_EXECUTABLE)
	message(FATAL_ERROR
		"cargo was not found on the PATH, but building the mtw-ffi submodule\n"
		"(Libraries/MTW-FFI) requires a Rust toolchain.\n"
		"Install rustup (https://rustup.rs), make sure 'cargo' is on the PATH, and re-configure.")
endif()

if(WIN32)
	set(MTW_FFI_LIBRARY "${MTW_FFI_SUBMODULE}/target/release/mtw_ffi.lib")
else()
	set(MTW_FFI_LIBRARY "${MTW_FFI_SUBMODULE}/target/release/libmtw_ffi.a")
endif()

# First configure on a fresh checkout: provision the library up front so the
# configure-time ABI probe below has something to probe. Later configures skip
# this (the library is already there); staleness from then on is handled by the
# build-time rule further down.
if(NOT EXISTS "${MTW_FFI_LIBRARY}")
	message(STATUS "mtw-ffi: building the submodule library with cargo (first configure) ...")
	execute_process(
		COMMAND "${MTW_FFI_CARGO_EXECUTABLE}" build --release --manifest-path "${MTW_FFI_SUBMODULE}/Cargo.toml" -p mtw-ffi
		RESULT_VARIABLE _mtw_ffi_cargo_result)
	if(NOT _mtw_ffi_cargo_result EQUAL 0)
		message(FATAL_ERROR "cargo build --release -p mtw-ffi failed (exit code ${_mtw_ffi_cargo_result}).")
	endif()
	if(NOT EXISTS "${MTW_FFI_LIBRARY}")
		message(FATAL_ERROR "cargo reported success but ${MTW_FFI_LIBRARY} is still missing.")
	endif()
	unset(_mtw_ffi_cargo_result)
endif()

# --- build-time rule ----------------------------------------------------------
# Rebuild the library whenever the crate's sources change (e.g. after a
# submodule bump) without needing a reconfigure. CONFIGURE_DEPENDS re-globs at
# build time, so files added or removed by a bump are picked up as well. The
# dependency is attached to the IMPORTED target, so every consumer (the desktop
# library, tests) builds it first.

file(GLOB_RECURSE MTW_FFI_SOURCES CONFIGURE_DEPENDS
	"${MTW_FFI_SUBMODULE}/Cargo.toml"
	"${MTW_FFI_SUBMODULE}/Cargo.lock"
	"${MTW_FFI_SUBMODULE}/crates/*.toml"
	"${MTW_FFI_SUBMODULE}/crates/*.rs")

add_custom_command(
	OUTPUT "${MTW_FFI_LIBRARY}"
	COMMAND "${MTW_FFI_CARGO_EXECUTABLE}" build --release --manifest-path "${MTW_FFI_SUBMODULE}/Cargo.toml" -p mtw-ffi
	DEPENDS ${MTW_FFI_SOURCES}
	COMMENT "Building mtw-ffi (cargo, release)"
	USES_TERMINAL
	VERBATIM)

add_custom_target(mtw_ffi-build DEPENDS "${MTW_FFI_LIBRARY}")

if(NOT TARGET mtw_ffi)
	add_library(mtw_ffi STATIC IMPORTED GLOBAL)
	set_target_properties(mtw_ffi PROPERTIES
		IMPORTED_LOCATION             "${MTW_FFI_LIBRARY}"
		INTERFACE_INCLUDE_DIRECTORIES "${MTW_FFI_SUBMODULE}/crates/mtw-ffi")
	# The static build pulls in the Rust standard library runtime.
	if(UNIX AND NOT APPLE)
		set_property(TARGET mtw_ffi APPEND PROPERTY INTERFACE_LINK_LIBRARIES pthread dl m)
	endif()
endif()

add_dependencies(mtw_ffi mtw_ffi-build)

# --- configure-time ABI probe ------------------------------------------------
# Compile AND run a tiny consumer of the submodule header + the built library:
# mtw_abi_version() must return MTW_FFI_ABI_EXPECTED. The library path is baked
# into the probe source as a comment so a re-provisioned library (different
# path) always re-runs the probe.

set(_mtw_ffi_probe_dir "${CMAKE_BINARY_DIR}/CMakeFiles/mtw_ffi_abi_probe")
file(WRITE "${_mtw_ffi_probe_dir}/probe.cpp" "// mtw-ffi library: ${MTW_FFI_LIBRARY}
#include \"mtw.h\"
int main()
{
	return mtw_abi_version() == ${MTW_FFI_ABI_EXPECTED} ? 0 : 1;
}
")

try_run(_mtw_ffi_probe_run _mtw_ffi_probe_build
	"${_mtw_ffi_probe_dir}"
	SOURCES "${_mtw_ffi_probe_dir}/probe.cpp"
	LINK_LIBRARIES "${MTW_FFI_LIBRARY}"
	CMAKE_FLAGS "-DINCLUDE_DIRECTORIES=${MTW_FFI_SUBMODULE}/crates/mtw-ffi"
	OUTPUT_VARIABLE _mtw_ffi_probe_output)

if(NOT _mtw_ffi_probe_build)
	message(FATAL_ERROR
		"mtw-ffi ABI probe failed to compile/link: ${MTW_FFI_LIBRARY}\n"
		"${_mtw_ffi_probe_output}")
endif()
if(NOT _mtw_ffi_probe_run STREQUAL "0")
	message(FATAL_ERROR
		"mtw-ffi ABI mismatch: the probe called mtw_abi_version() and got\n"
		"'${_mtw_ffi_probe_run}' instead of ${MTW_FFI_ABI_EXPECTED}.\n"
		"The built library does not match the header in Libraries/MTW-FFI/crates/mtw-ffi/mtw.h\n"
		"— the submodule checkout is inconsistent; update it (git submodule update) and rebuild.")
endif()
message(STATUS "mtw-ffi ABI probe OK (mtw_abi_version() == ${MTW_FFI_ABI_EXPECTED})")

message(STATUS "mtw-ffi: ${MTW_FFI_LIBRARY}")

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
