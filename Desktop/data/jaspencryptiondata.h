#ifndef JASPENCRYPTIONDATA_H
#define JASPENCRYPTIONDATA_H

#include <mutex>
#include <string>

class JaspEncryptionData {
public:
    JaspEncryptionData(const JaspEncryptionData& obj) = delete;
    static JaspEncryptionData* getInstance();

    bool encryptionActive() {return _encryptionActive;};
    std::string getPassword() {return encryptionActive() ? _password : ""; };
    const char* getPasswordPtr() { return _password.c_str();}

    bool queryUserForPassword(bool hideInput = false);

    void reset() { _encryptionActive = false; _password.clear(); };

private:
    static JaspEncryptionData* instancePtr;
    static std::mutex mtx;
    JaspEncryptionData() {}

    std::string _password = "";
    bool _encryptionActive = false;

};


#endif // JASPENCRYPTIONDATA_H
