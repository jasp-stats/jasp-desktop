#ifndef JASPSHAREDMEM_H
#define JASPSHAREDMEM_H

#include "../JASP-Common/ipcchannel.h"
#include "jasp-sharedmem_global.h"

class JASPChannelInterface {
protected:
  ~JASPChannelInterface() { }  // use destroy()

public:
	virtual void STDCALL destroy() = 0;
	virtual void STDCALL send(const char* data) = 0;
	virtual bool STDCALL receive(char** data, int timeout) = 0;
};

class JASPChannelImpl : public JASPChannelInterface {
public:
	JASPChannelImpl(const char* name, int slaveNo);
	~JASPChannelImpl();

	virtual void STDCALL destroy();
	virtual void STDCALL send(const char* data);
	virtual bool STDCALL receive(char** data, int timeout);

private:
	IPCChannel *_channel;

};



extern "C" JASPSHAREDMEMSHARED_EXPORT JASPChannelInterface* CreateJASPChannel(const char* name, int slaveNo);
#endif // JASPSHAREDMEM_Hs
