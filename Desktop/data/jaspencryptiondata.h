#ifndef JASPENCRYPTIONDATA_H
#define JASPENCRYPTIONDATA_H

#include <mutex>
#include <string>

class JaspEncryptionData {
public:
    JaspEncryptionData(const JaspEncryptionData& obj) = delete;
    static JaspEncryptionData* getInstance();

    bool encryptionActive() {return _encryptionActive;};
	void setEncryptionActive(bool value) {_encryptionActive = value;}

	bool paramsSet() {return _parametersSet;};
	void setParamsSet(bool value) {_parametersSet = value;}

	bool jaspTeamSubmission() {return _jaspTeamSubmission;};
	void setJaspTeamSubmission(bool value) {_jaspTeamSubmission = value;}

	std::string getPassword() {return encryptionActive() ? _password : ""; };
	void setPassword(std::string newPassword) { _password = newPassword; }

    std::string getPublicKeyResponse(); //base64 encoded public key
    void setPublicKeyResponse(const std::string& key) {_publickeyResponse = key;}

    std::string getPrivatekey() { return _privatekey; } //base64 encoded private key
    void setPrivatekey(const std::string& key) {_privatekey = key;}

    //these are only used when forming a response in mixed privkey/password mode (eg response jaspTeamSubmission)
    std::string getPasswordSaltResponse(); //base64 encoded passwordsalt
    void setPasswordSaltResponse(const std::string& key) {_passwordSaltResponse = key;}

	void reset();

private:
    static JaspEncryptionData* instancePtr;
    static std::mutex mtx;
    JaspEncryptionData() {}

    std::string _password				= "",
				_publickeyResponse		= "",
				_passwordSaltResponse	= "",
				_privatekey				= "";
    bool		_encryptionActive		= false,
				_jaspTeamSubmission		= false,
				_parametersSet			= false;

	const std::string JASPTeamPublicKey = "sRIvR8JdS9XUTPDpo74Z8MAebG/at315MXTnRybHH2o="; //base64
};


#endif // JASPENCRYPTIONDATA_H
