#include <iostream>
#include <fstream>
#include <filesystem>
#include <string>

//quickly check if we have access to dirs/files listed in argv[]. return -1 if permissions need to be fixed by jasp
int main( int argc, char* argv[] ) {
    if(argc < 2) {
        std::cout << "Please provid a path to a file that list all paths to test" << std::endl;
        return -2;
    }
    
    for(int i = 1; i < argc; i++)
    {
        std::string line = std::string(argv[i]);
        std::filesystem::file_status status;
        try {
            status = std::filesystem::status(line);
        }
        catch(std::exception& e) {
            status = std::filesystem::file_status(std::filesystem::file_type::unknown);
        }
        if(status.type() == std::filesystem::file_type::unknown)
            return -1;
    }
    return 0;
}