# You can search the availability of a certain library here, https://conan.io/center/
# After adding the library, you can add it to the target using
#
# - `CONAN_PKG::library-name` (if you use this, you don't need to add the header files)
# - or ${CONAN_LIBS_LIBRARY-name} (it needs to be upper case)
# 
# The header files can be added to a target using ${CONAN_INCLUDE_DIRS_LIBRARY-NAME}
from conan import ConanFile

class JaspConanConfig(ConanFile):
    settings = "os", "compiler", "build_type", "arch"
    generators = "CMakeToolchain", "CMakeDeps"
    options = {"syntax_interface_only": [True, False]}
    default_options = {
        "brotli*:shared": True,
        "syntax_interface_only": False,
    }

    def requirements(self):
        # Core dependencies required by SyntaxInterface (CommonData, Common)
        self.requires("libiconv/1.18", force=True)
        self.requires("boost/1.86.0")
        self.requires("zlib/1.3.1")
        self.requires("libarchive/3.8.1")
        self.requires("zstd/1.5.7")
        self.requires("openssl/3.4.1")
        self.requires("sqlite3/3.49.1")

        if not self.options.syntax_interface_only:
            # jsoncpp is vendored in Common/json/ so Conan's copy is not linked,
            # but keep it here for the full build to avoid unexpected Conan graph changes
            self.requires("jsoncpp/1.9.6")
            self.requires("brotli/1.1.0")
            self.requires("gmp/6.3.0")
            self.requires("mpfr/4.2.1")
            self.requires("freexl/2.0.99.cci.20260225")
            self.requires("libsodium/1.0.20")
            # librdata is not available for Windows platforms on conan-center yet
            if self.settings_build.os == "Macos":
                self.requires("librdata/0.0.0.cci.20231003")

    def build_requirements(self):
        self.tool_requires("cmake/3.30.0")
        if not self.options.syntax_interface_only:
            self.tool_requires("bison/3.7.6")

